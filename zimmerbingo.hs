{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
import           Yesod
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Random.Extras
import           Data.Random.RVar
import           Data.Random.Source.DevRandom
import qualified Data.Foldable                as F
import qualified Data.Map                     as M

data Zimmerbingo = Zimmerbingo

mkYesod "Zimmerbingo" [$parseRoutes|
/ HomeR GET
|]

instance Yesod Zimmerbingo where approot _ = ""

data UserConfiguration 
    = UserConfiguration {
        rooms :: [String]
      , grid :: (Int, Int)
      }
    deriving (Show)

defaultUserConfiguration 
    = UserConfiguration {
        rooms = map show [1 .. 23]
      , grid = (3, 3)
      }

getRooms :: UserConfiguration -> String
getRooms UserConfiguration { rooms = w } = unwords w

setRooms :: String -> UserConfiguration -> Maybe UserConfiguration
setRooms s uc = Just uc { rooms = words s }

getGrid :: UserConfiguration -> String
getGrid UserConfiguration { grid = (w, h) } = unwords [show w, show h]

setGrid :: String -> UserConfiguration -> Maybe UserConfiguration
setGrid s uc = case map reads $ words s of
                 [[(w, "")], [(h, "")]] -> Just uc { grid = (w, h) }
                 _                      -> Nothing

fromCookiesMap = M.fromList [("rooms", setRooms), ("grid", setGrid)]

fromCookies :: UserConfiguration -> [(String, String)] -> Maybe UserConfiguration
fromCookies = F.foldrM $ \(n, v) uc -> (do
                                         setter <- M.lookup n fromCookiesMap
                                         setter v uc) 
                                        <|> return uc

fromCookiesDefault = fromCookies defaultUserConfiguration

toCookiesList = [("rooms", getRooms), ("grid", getGrid)]

toCookies :: Int -> UserConfiguration -> GHandler sub master ()
toCookies timeout uc = forM_ toCookiesList $ \(name, f) -> setCookie timeout name (f uc)

unflattenGrid n = unfoldr $ \x -> case splitAt n x of 
                                    ([], _) -> Nothing
                                    (a, b) -> Just (a, b)

randomGrid :: UserConfiguration -> IO [[String]]
randomGrid UserConfiguration { rooms = r, grid = (w, h) }
    = unflattenGrid w <$> runRVar (sample (w * h) r) DevURandom

gridWidget :: [[String]] -> GWidget sub master ()
gridWidget grid = addHamlet [$hamlet|
                             %table!class="grid"
                               $forall grid row
                                 %tr!class="grid_row"
                                   $forall row cell
                                     %td!class="grid_cell" $cell$
                             |]

getHomeR = do
  Just uc <- (fromCookiesDefault . reqCookies) `fmap` getRequest
  toCookies 120 uc
  grid <- liftIO $ randomGrid uc
  liftIO $ print uc
  liftIO $ print grid
  defaultLayout $ do
             setTitle "Zimmerbingo"
             addHamlet [$hamlet|
                        %h1 Zimmerbingo
                        |]
             -- input widget
             gridWidget grid

main = basicHandler 3000 Zimmerbingo

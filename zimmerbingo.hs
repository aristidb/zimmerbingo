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

fromQueryMap = M.fromList [("rooms", setRooms), ("grid", setGrid)]

fromQuery :: UserConfiguration -> [(String, String)] -> Maybe UserConfiguration
fromQuery = F.foldrM $ \(n, v) uc -> (do
                                         setter <- M.lookup n fromQueryMap
                                         setter v uc) 
                                        <|> return uc

fromQueryDefault = fromQuery defaultUserConfiguration

unflattenGrid n = unfoldr $ \x -> case splitAt n x of 
                                    ([], _) -> Nothing
                                    (a, b) -> Just (a, b)

randomGrid UserConfiguration { rooms = r, grid = (w, h) }
    = unflattenGrid w <$> runRVar (sample (w * h) r) DevURandom
      
inputWidget uc = addHamlet [$hamlet|
                            %div!class="input_container"
                              %form!method="GET"!action=@HomeR@
                                %p
                                  %label!for="rooms" Rooms
                                  %input!name="rooms"!value="$getRooms.uc$"
                                %p
                                  %label!for="grid" Grid
                                  %input!name="grid"!value="$getGrid.uc$"
                                %p
                                  %button Update
                            |]

gridWidget grid = addHamlet [$hamlet|
                             %table!class="grid"
                               $forall grid row
                                 %tr!class="grid_row"
                                   $forall row cell
                                     %td!class="grid_cell" $cell$
                             |]

getHomeR = do
  Just uc <- (fromQueryDefault . reqGetParams) `fmap` getRequest
  grid <- liftIO $ randomGrid uc
  liftIO $ print uc
  liftIO $ print grid
  defaultLayout $ do
             setTitle "Zimmerbingo"
             addHamlet [$hamlet|
                        %h1 Zimmerbingo
                        |]
             inputWidget uc
             gridWidget grid

main = basicHandler 3000 Zimmerbingo

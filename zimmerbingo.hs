{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
import qualified Data.Map as M
import qualified Data.Foldable as F

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

fromCookiesMap 
    = M.fromList [
        ("rooms", setRooms)
      , ("grid", setGrid)
      ]

fromCookies :: UserConfiguration -> [(String, String)] -> Maybe UserConfiguration
fromCookies = F.foldrM $ \(n, v) uc -> do
                setter <- M.lookup n fromCookiesMap
                setter v uc

fromCookiesDefault = fromCookies defaultUserConfiguration

getHomeR = do
  Just userConfiguration <- (fromCookiesDefault . reqCookies) `fmap` getRequest
  defaultLayout $ do
             setTitle "Zimmerbingo"
             addHamlet [$hamlet|
                        %h1 Zimmerbingo
                        |]

main = basicHandler 3000 Zimmerbingo

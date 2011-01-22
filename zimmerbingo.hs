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

instance Yesod Zimmerbingo where
    approot _ = ""
    defaultLayout contents = do
      PageContent title headTags bodyTags <- widgetToPageContent $
        do
          addCassius [$cassius|
                      body
                        font-family: Verdana,sans-serif
                        font-size: 12pt
                      #wrapper
                        padding-left: 32pt
                        padding-right: 32pt
                      |]
          addWidget contents
      hamletToRepHtml [$hamlet|
                       !!!
                       %html
                         %head
                           %title $title$
                           ^headTags^
                         %body
                           #wrapper
                             %h1 $title$
                             ^bodyTags^
                       |]

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
      
inputWidget uc = do
  addCassius [$cassius|
              .input_container
                margin-bottom: 12pt
              @@media print
                .input_container { display: none }
              |]
  addHamlet [$hamlet|
             %div!class="input_container"
               %p Sie können diese Seite bedenkenlos drucken, dieses Formular wird nicht angezeigt werden.
               %form!method="GET"!action=@HomeR@
               %table!class="input_table"
                 %tr
                   %td
                     %label!for="rooms" Räume
                   %td
                     %input!name="rooms"!value="$getRooms.uc$"!size=60
                 %tr
                   %td
                     %label!for="grid" Raster
                   %td
                     %input!name="grid"!value="$getGrid.uc$"!size=6
                 %tr
                     %td!colspan="2"
                       %button Neu generieren
                 |]

gridWidget grid = do
  addCassius [$cassius|
              table.grid
                font-size: 36pt
                text-align: center
                border-collapse: collapse
              table.grid td
                padding: 5pt
                width: 2ex
                height: 2ex
                border: 1pt solid #888888
              |]
  addHamlet [$hamlet|
             %table!class="grid"
               $forall grid row
                 %tr
                   $forall row cell
                     %td $cell$
             |]

getHomeR = do
  Just uc <- (fromQueryDefault . reqGetParams) `fmap` getRequest
  grid <- liftIO $ randomGrid uc
  liftIO $ print uc
  liftIO $ print grid
  defaultLayout $ do
             setTitle "Zimmerbingo"
             inputWidget uc
             gridWidget grid

main = basicHandler 3000 Zimmerbingo

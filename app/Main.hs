{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Media.MediaType
import Control.Monad.Trans.Class
import System.Directory
import Data.ByteString
import Data.Aeson
import GHC.Generics
import Servant
import Servant.JQuery

data Response = Response
  { status :: Int
  , path :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Response

type MyAPI = Capture "name" String :> ReqBody '[OctetStream] ByteString :> Put '[JSON] Response
        :<|> Raw

myAPI :: Server MyAPI
myAPI = imageHandler :<|> serveDirectory "."

imageHandler :: String -> ByteString -> EitherT ServantErr IO Response
imageHandler name image  = storeImage name image

storeImage :: String -> ByteString -> EitherT ServantErr IO Response
storeImage name image = do
  lift $ Data.ByteString.writeFile name image
  return $ Response status' path'
    where status' = 200
          path' = "/" ++ name

app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

main :: IO ()
main = run 8080 $ logStdoutDev app

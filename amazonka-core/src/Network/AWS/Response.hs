-- |
-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Functions contained in this module fully consume the body and thus close
-- the connection. This is needed to avoid hitting this issue:
-- <https://github.com/brendanhay/amazonka/issues/490>.
--
-- The only exception is 'receiveBody', which passes a streaming response
-- body to a callback and thus is not allowed to close the connection. Users
-- of streaming functions are advised to be careful and consume the response
-- body manually if they want the connection to be closed promptly.
--
-- Note that using 'runResourceT' will always close the connection.
module Network.AWS.Response
  ( receiveNull,
    receiveEmpty,
    receiveXMLWrapper,
    receiveXML,
    receiveJSON,
    receiveBytes,
    receiveBody,
  )
where

-- import Control.Monad.Catch
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Resource
-- import Data.Aeson
-- import Data.Conduit
-- import qualified Data.Conduit.Binary as Conduit
-- import Data.Proxy
-- import Data.Text (Text)
-- import Network.AWS.Data.Body
-- import Network.AWS.Data.ByteString
-- import Network.AWS.Data.Log
-- import Network.AWS.Data.XML
-- import Network.AWS.Types
-- import Network.HTTP.Client.Conduit (responseClose)
-- import Network.HTTP.Conduit hiding (Proxy, Request, Response)
-- import Network.HTTP.Types
-- import Text.XML (Node)

import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.Conduit.Binary as Conduit.Binary
import qualified Data.Conduit as Conduit
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import Network.AWS.Data
import Network.AWS.Prelude
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP
import Network.AWS.Types
import qualified Text.XML as XML

receiveNull ::
  MonadResource m =>
  AWSResponse request ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveNull rs _ =
 stream $ \r _ _ _ ->
  liftIO (Client.responseClose r)
    *> pure (Right rs)

receiveEmpty ::
  MonadResource m =>
  (Int -> HTTP.ResponseHeaders -> () -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveEmpty f _ =
 stream $ \r s h _ ->
  liftIO (Client.responseClose r)
    *> pure (f s h ())

receiveXMLWrapper ::
  MonadResource m =>
  XML.Name ->
  (Int -> HTTP.ResponseHeaders -> XMLCursor -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveXMLWrapper n f =
  receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML ::
  MonadResource m =>
  (Int -> HTTP.ResponseHeaders -> XMLCursor -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveXML =
  deserialise decodeXML

receiveJSON ::
  MonadResource m =>
  (Int -> HTTP.ResponseHeaders -> Aeson.Object -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveJSON =
  deserialise (Bifunctor.first Text.pack . Aeson.eitherDecode')

receiveBytes ::
  MonadResource m =>
  (Int -> HTTP.ResponseHeaders -> ByteString -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveBytes =
  deserialise (Right . ByteString.Lazy.toStrict)

receiveBody ::
  MonadResource m =>
  (Int -> HTTP.ResponseHeaders -> ResponseBody -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
receiveBody f _ =
  stream $ \_ s h x ->
      pure $! f s h x

-- | Deserialise an entire response body, such as an XML or JSON payload.
deserialise ::
  MonadResource m =>
  (ByteStringLazy -> Either Text a) ->
  (Int -> HTTP.ResponseHeaders -> a -> Either Text (AWSResponse request)) ->
  Logger ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
deserialise g f l Service {..} _ rs = do
  let s = Client.responseStatus rs
      h = Client.responseHeaders rs
      x = Client.responseBody rs
      
  bytes <- sinkLBS x
  
  if not (serviceCheck s)
    then pure $! Left (serviceError s h bytes)
    else do
      liftIO $ l Debug $
        "[Raw Response Body] {\n"
          <> Builder.lazyByteString bytes
          <> "\n}"

      pure $!
        Bifunctor.bimap
          (SerializeError . SerializeError' serviceAbbrev s (Just bytes))
          (<$ rs)
          (g bytes >>= f (fromEnum s) h)

-- | Stream a raw response body, such as an S3 object payload.
stream ::
  MonadResource m =>
  ( ClientResponse ClientBody ->
    Int ->
    HTTP.ResponseHeaders ->
    ResponseBody ->
    m (Either Text (AWSResponse request))
  ) ->
  Service ->
  Proxy request ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse request)))
stream f Service {..} _ rs = do
  let s = Client.responseStatus rs
      h = Client.responseHeaders rs
      x = Client.responseBody rs
 
  if not (serviceCheck s)
    then do
      bytes <- sinkLBS x
      pure $! Left (serviceError s h bytes)
    else
     f rs (fromEnum s) h (ResponseBody x) <&>
      Bifunctor.bimap
       (SerializeError . SerializeError' serviceAbbrev s Nothing)
       (<$ rs)

sinkLBS :: MonadResource m => ClientBody -> m ByteStringLazy
sinkLBS body =
  Resource.liftResourceT (body `Conduit.connect` Conduit.Binary.sinkLbs)

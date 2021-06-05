-- |
-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2021 Brendan Hay
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

import qualified Control.Exception as Exception
import Control.Monad.Trans.Resource (liftResourceT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ()
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.Log
import Network.AWS.Data.XML
import Network.AWS.Prelude
import Network.AWS.Types
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (ResponseHeaders)
import qualified Text.XML as XML

receiveNull ::
  MonadResource m =>
  Rs a ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveNull rs _ =
  stream $ \r _ _ _ ->
    liftIO (Client.responseClose r) *> pure (Right rs)

receiveEmpty ::
  MonadResource m =>
  (Int -> ResponseHeaders -> () -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveEmpty f _ =
  stream $ \r s h _ ->
    liftIO (Client.responseClose r) *> pure (f s h ())

receiveXMLWrapper ::
  MonadResource m =>
  Text ->
  (Int -> ResponseHeaders -> [XML.Node] -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML ::
  MonadResource m =>
  (Int -> ResponseHeaders -> [XML.Node] -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveXML = deserialise decodeXML

receiveJSON ::
  MonadResource m =>
  (Int -> ResponseHeaders -> Aeson.Object -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveJSON = deserialise Aeson.eitherDecode'

receiveBytes ::
  MonadResource m =>
  (Int -> ResponseHeaders -> ByteString -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveBytes = deserialise (Right . LBS.toStrict)

receiveBody ::
  MonadResource m =>
  (Int -> ResponseHeaders -> RsBody -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
receiveBody f _ = stream $ \_ s h x -> pure (f s h (RsBody x))

-- | Deserialise an entire response body, such as an XML or JSON payload.
deserialise ::
  MonadResource m =>
  (ByteStringLazy -> Either String b) ->
  (Int -> ResponseHeaders -> b -> Either String (Rs a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
deserialise g f l Service {..} _ rs = do
  let s = Client.responseStatus rs
      h = Client.responseHeaders rs
      x = Client.responseBody rs
  b <- sinkLBS x
  if not (_svcCheck s)
    then liftIO (Exception.throwIO (_svcError s h b))
    else do
      liftIO $ l Debug $ build $ "[Raw Response Body] {\n" <> b <> "\n}"

      case g b >>= f (fromEnum s) h of
        Right r -> pure (s, r)
        Left e ->
          liftIO . Exception.throwIO . SerializeError $
            SerializeError' _svcAbbrev s (Just b) e

-- | Stream a raw response body, such as an S3 object payload.
stream ::
  MonadResource m =>
  ( ClientResponse ->
    Int ->
    ResponseHeaders ->
    ResponseBody ->
    m (Either String (Rs a))
  ) ->
  Service ->
  Proxy a ->
  ClientResponse ->
  m (Response a)
stream f Service {..} _ rs = do
  let s = Client.responseStatus rs
      h = Client.responseHeaders rs
      x = Client.responseBody rs
  if not (_svcCheck s)
    then sinkLBS x >>= liftIO . Exception.throwIO . _svcError s h
    else do
      e <- f rs (fromEnum s) h x
      either
        (liftIO . Exception.throwIO . SerializeError . SerializeError' _svcAbbrev s Nothing)
        (pure . (s,))
        e

sinkLBS :: MonadResource m => ResponseBody -> m ByteStringLazy
sinkLBS bdy = liftResourceT (bdy `Conduit.connect` Conduit.Binary.sinkLbs)

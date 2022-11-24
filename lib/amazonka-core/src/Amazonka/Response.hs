-- |
-- Module      : Amazonka.Response
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
module Amazonka.Response
  ( receiveNull,
    receiveEmpty,
    receiveXMLWrapper,
    receiveXML,
    receiveJSON,
    receiveBytes,
    receiveBody,
  )
where

import Amazonka.Data
import Amazonka.Prelude hiding (error)
import Amazonka.Types
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Resource (liftResourceT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ()
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (ResponseHeaders)
import qualified Text.XML as XML

receiveNull ::
  MonadResource m =>
  AWSResponse a ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveNull rs _ =
  stream $ \r _ _ _ ->
    liftIO (Client.responseClose r) *> pure (Right rs)

receiveEmpty ::
  MonadResource m =>
  (Int -> ResponseHeaders -> () -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveEmpty f _ =
  stream $ \r s h _ ->
    liftIO (Client.responseClose r) *> pure (f s h ())

receiveXMLWrapper ::
  MonadResource m =>
  Text ->
  (Int -> ResponseHeaders -> [XML.Node] -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML ::
  MonadResource m =>
  (Int -> ResponseHeaders -> [XML.Node] -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveXML = deserialise decodeXML

receiveJSON ::
  MonadResource m =>
  (Int -> ResponseHeaders -> Aeson.Object -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveJSON = deserialise Aeson.eitherDecode'

receiveBytes ::
  MonadResource m =>
  (Int -> ResponseHeaders -> ByteString -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveBytes = deserialise (Right . LBS.toStrict)

receiveBody ::
  MonadResource m =>
  (Int -> ResponseHeaders -> ResponseBody -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
receiveBody f _ =
  stream $ \_ s h x ->
    pure (f s h (ResponseBody x))

-- | Deserialise an entire response body, such as an XML or JSON payload.
deserialise ::
  MonadResource m =>
  (ByteStringLazy -> Either String b) ->
  (Int -> ResponseHeaders -> b -> Either String (AWSResponse a)) ->
  Logger ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
deserialise reader parser logger Service {..} _ rs =
  Except.runExceptT $ do
    let status = Client.responseStatus rs
        headers = Client.responseHeaders rs

    body <- sinkLBS (Client.responseBody rs)

    unless (check status) $ Except.throwE (error status headers body)

    liftIO . logger Trace $
      build ("[Raw Response Body] {\n" <> body <> "\n}")

    case reader body >>= parser (fromEnum status) headers of
      Right ok -> pure (ok <$ rs)
      Left err ->
        Except.throwE $
          SerializeError (SerializeError' abbrev status (Just body) err)

-- | Stream a raw response body, such as an S3 object payload.
stream ::
  MonadResource m =>
  ( ClientResponse () ->
    Int ->
    ResponseHeaders ->
    ClientBody ->
    m (Either String (AWSResponse a))
  ) ->
  Service ->
  Proxy a ->
  ClientResponse ClientBody ->
  m (Either Error (ClientResponse (AWSResponse a)))
stream parser Service {..} _ rs =
  Except.runExceptT $ do
    let status = Client.responseStatus rs
        headers = Client.responseHeaders rs
        body = Client.responseBody rs

    unless (check status) $ do
      lazy <- sinkLBS body
      Except.throwE (error status headers lazy)

    lift (parser (() <$ rs) (fromEnum status) headers body) >>= \case
      Right ok -> pure (ok <$ rs)
      Left err ->
        Except.throwE $
          SerializeError (SerializeError' abbrev status Nothing err)

sinkLBS :: MonadResource m => ClientBody -> m ByteStringLazy
sinkLBS bdy = liftResourceT (bdy `Conduit.connect` Conduit.Binary.sinkLbs)

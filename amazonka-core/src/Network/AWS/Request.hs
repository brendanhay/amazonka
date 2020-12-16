{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Request
  ( -- * Plugins
    contentMD5Header,
    expectHeader,

    -- * Lenses
    requestHeaders,
    queryString,
    requestURL,
    -- Re-exported types
    StdMethod (..),
  )
where

import Data.Maybe
import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.Headers
import Network.AWS.Data.JSON
import Network.AWS.Data.Path
import Network.AWS.Data.Query
import Network.AWS.Data.XML
import Network.AWS.Lens (Lens', (%~), (&), (.~), (<&>))
import Network.AWS.Types
import qualified Network.HTTP.Conduit as Client
import Network.HTTP.Types (StdMethod (..))
import qualified Network.HTTP.Types as HTTP

-- head' ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- head' service path query headers body =
--   (get service path query headers)
--     { _rqMethod = HEAD
--     }

-- delete ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- delete service path query headers body =
--   (get service path query headers)
--     { _rqMethod = DELETE
--     }

-- get ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    Request a
-- get service path query headers =
--  Request
--     { _rqService = service,
--       _rqMethod = GET,
--       _rqPath = path,
--       _rqQuery = query,
--       _rqHeaders = headers,
--       _rqBody = ""
--     }

-- post ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- post service path query headers body =
--   (get service path query headers)
--     { _rqMethod = POST,
--       _rqBody = body
--     }

-- put ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- put service path query headers body =
--   (get service path query headers)
--    { _rqMethod = PUT
--    }

-- postForm ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postForm service path query headers body =
--   Request
--     { _rqService = s,
--       _rqMethod = POST,
--       _rqPath = rawPath x,
--       _rqQuery = mempty,
--       _rqBody = toBody (toQuery x),
--       _rqHeaders = hdr hContentType hFormEncoded headers
--     }

mkRequest ::
  Service ->
  StdMethod ->
  ByteString ->
  QueryString ->
  [Header] ->
  RqBody ->
  Request a
mkRequest service method path query headers body =
  Request
    { _rqService = service,
      _rqMethod = method,
      _rqPath = rawPath path,
      _rqQuery = query,
      _rqHeaders = headers,
      _rqBody = body
    }

-- postXML ::
--    ToElement a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postXML service path query headers body =
--    (putXML service path query headers body)
--      { _rqMethod = POST
--      }

-- putXML ::
--    ToElement a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- putXML service path query headers body =
--   (get service path query headers)
--     { _rqMethod = PUT
--     , _rqBody = maybe "" toBody (maybeElement x)
--     }

-- patchJSON ::
--    ToJSON a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- patchJSON service path query headers body =
--    (putJSON service path query headers body)
--      { _rqMethod = PATCH
--      }

-- postJSON ::
--    ToJSON a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postJSON service path query headers body =
--    (putJSON service path query headers body)
--      { _rqMethod = POST
--      }

-- json ::
--    ToJSON a =>
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- json service method path query headers body =
--    Request
--     { _rqService = service,
--       _rqMethod = method,
--       _rqPath = path,
--       _rqQuery = query,
--       _rqHeaders = headers,
--       _rqBody = toBody (toJSON x)
--     }

queryString :: Lens' Client.Request ByteString
queryString f x =
  f (Client.queryString x) <&> \y -> x {Client.queryString = y}

requestHeaders :: Lens' Client.Request HTTP.RequestHeaders
requestHeaders f x =
  f (Client.requestHeaders x) <&> \y -> x {Client.requestHeaders = y}

requestURL :: ClientRequest -> ByteString
requestURL x =
  scheme
    <> toBS (Client.host x)
    <> port (Client.port x)
    <> toBS (Client.path x)
    <> toBS (Client.queryString x)
  where
    scheme
      | secure = "https://"
      | otherwise = "http://"

    port = \case
      80 -> ""
      443 | secure -> ""
      n -> ":" <> toBS n

    secure = Client.secure x

-- formEncodedHeader :: Request a -> Request a
-- formEncodedHeader rq =
--   rq & rqHeaders %~ hdr hContentType hFormEncoded

contentMD5Header :: Request a -> Request a
contentMD5Header rq
  | missing, Just x <- md5 = rq & rqHeaders %~ hdr HTTP.hContentMD5 x
  | otherwise = rq
  where
    missing = isNothing $ lookup HTTP.hContentMD5 (_rqHeaders rq)
    md5 = md5Base64 (_rqBody rq)

expectHeader :: Request a -> Request a
expectHeader = rqHeaders %~ hdr hExpect "100-continue"

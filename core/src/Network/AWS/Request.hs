{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Request
    (
    -- * Requests
      head'
    , delete
    , get

    -- ** Empty body
    , post
    , put

    -- ** Specialised body
    , postXML
    , postJSON
    , postQuery
    , postBody

    , putXML
    , putJSON
    , putBody

    -- ** Constructors
    , defaultRequest

    -- ** Hashing
    , contentSHA256
    , contentMD5

    -- ** Lenses
    , requestHeaders
    , queryString
    , requestURL
    ) where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.XML
import           Network.AWS.Types
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Types           (StdMethod (..))
import qualified Network.HTTP.Types           as HTTP

type ToRequest a = (ToPath a, ToQuery a, ToHeaders a)

head' :: ToRequest a => Service -> a -> Request a
head' s x = get s x & rqMethod .~ HEAD

delete :: ToRequest a => Service -> a -> Request a
delete s x = get s x & rqMethod .~ DELETE

get :: ToRequest a => Service -> a -> Request a
get s = contentSHA256 . defaultRequest s

post :: ToRequest a => Service -> a -> Request a
post s x = get s x & rqMethod .~ POST

put :: ToRequest a => Service -> a -> Request a
put s x = get s x & rqMethod .~ PUT

postXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
postXML s x = putXML s x & rqMethod .~ POST

postJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
postJSON s x = putJSON s x & rqMethod .~ POST

postQuery :: ToRequest a => Service -> a -> Request a
postQuery s x = Request
    { _rqService = s
    , _rqMethod  = POST
    , _rqPath    = rawPath x
    , _rqQuery   = mempty
    , _rqBody    = toBody (toQuery x)
    , _rqHeaders = hdr hContentType hFormEncoded (toHeaders x)
    } & contentSHA256

postBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
postBody s x = putBody s x & rqMethod .~ POST

putXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
putXML s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toElement x)
    & contentSHA256

putJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
putJSON s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toJSON x)
    & contentSHA256

putBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
putBody s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ toBody x
    & contentSHA256

defaultRequest :: ToRequest a => Service -> a -> Request a
defaultRequest s x = Request
    { _rqService = s
    , _rqMethod  = GET
    , _rqPath    = rawPath x
    , _rqQuery   = toQuery x
    , _rqHeaders = toHeaders x
    , _rqBody    = ""
    }

contentSHA256 :: Request a -> Request a
contentSHA256 rq = rq & rqHeaders %~
    hdr hAMZContentSHA256 (rq ^. rqBody . to (digestToBase Base16 . bodySHA256))

contentMD5 :: Request a -> Request a
contentMD5 rq
    | missing, Just x <- md5 = rq & rqHeaders %~ hdr HTTP.hContentMD5 x
    | otherwise              = rq
  where
    missing = isNothing $ lookup HTTP.hContentMD5 (rq ^. rqHeaders)
    md5     = rq ^. rqBody . to (fmap (digestToBase Base64) . bodyCalculateMD5)

queryString :: Lens' Client.Request ByteString
queryString f x =
    f (Client.queryString x) <&> \y -> x { Client.queryString = y }

requestHeaders :: Lens' Client.Request HTTP.RequestHeaders
requestHeaders f x =
    f (Client.requestHeaders x) <&> \y -> x { Client.requestHeaders = y }

requestURL :: ClientRequest -> ByteString
requestURL x = scheme
    <> toBS (Client.host        x)
    <> port (Client.port        x)
    <> toBS (Client.path        x)
    <> toBS (Client.queryString x)
  where
    scheme
        | secure    = "https://"
        | otherwise = "http://"

    port = \case
        80           -> ""
        443 | secure -> ""
        n            -> ":" <> toBS n

    secure = Client.secure x

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

head' :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head' x = get x & rqMethod .~ HEAD

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get = contentSHA256 . defaultRequest

post :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
post x = get x & rqMethod .~ POST

put :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
put x = get x & rqMethod .~ PUT

postXML :: (ToQuery a, ToPath a, ToHeaders a, ToElement a) => a -> Request a
postXML x = putXML x & rqMethod .~ POST

postJSON :: (ToQuery a, ToPath a, ToHeaders a, ToJSON a) => a -> Request a
postJSON x = putJSON x & rqMethod .~ POST

postQuery :: (ToQuery a, ToPath a, ToHeaders a) => a -> Request a
postQuery x = Request
    { _rqMethod  = POST
    , _rqPath    = rawPath x
    , _rqQuery   = mempty
    , _rqBody    = toBody (toQuery x)
    , _rqHeaders =
        hdr HTTP.hContentType "application/x-www-form-urlencoded; charset=utf-8"
            (toHeaders x)
    } & contentSHA256

postBody :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
postBody x = putBody x & rqMethod .~ POST

putXML :: (ToPath a, ToQuery a, ToHeaders a, ToElement a) => a -> Request a
putXML x = defaultRequest x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toElement x)
    & contentSHA256

putJSON :: (ToQuery a, ToPath a, ToHeaders a, ToJSON a) => a -> Request a
putJSON x = defaultRequest x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toJSON x)
    & contentSHA256

putBody :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
putBody x = defaultRequest x
    & rqMethod .~ PUT
    & rqBody   .~ toBody x
    & contentSHA256

defaultRequest :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
defaultRequest x = Request
    { _rqMethod  = GET
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

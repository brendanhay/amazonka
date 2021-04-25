{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Sign.V2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V2
    ( v2
    ) where

import qualified Data.ByteString.Char8       as BS8
import           Data.Monoid
import           Data.Time
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Log
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Time
import           Network.AWS.Types
import qualified Network.HTTP.Conduit        as Client
import           Network.HTTP.Types          hiding (toQuery)

data V2 = V2
    { metaTime      :: !UTCTime
    , metaEndpoint  :: !Endpoint
    , metaSignature :: !ByteString
    }

instance ToLog V2 where
    build V2{..} = buildLines
        [ "[Version 2 Metadata] {"
        , "  time      = " <> build metaTime
        , "  endpoint  = " <> build (_endpointHost metaEndpoint)
        , "  signature = " <> build metaSignature
        , "}"
        ]

v2 :: Signer
v2 = Signer sign (const sign) -- FIXME: revisit v2 presigning.

sign :: Algorithm a
sign Request{..} AuthEnv{..} r t = Signed meta rq
  where
    meta = Meta (V2 t end signature)

    rq = (clientRequest end _svcTimeout)
        { Client.method         = meth
        , Client.path           = path'
        , Client.queryString    = toBS authorised
        , Client.requestHeaders = headers
        , Client.requestBody    = toRequestBody _rqBody
        }

    meth  = toBS _rqMethod
    path' = toBS (escapePath _rqPath)

    end@Endpoint{..} = _svcEndpoint r

    Service{..} = _rqService

    authorised = pair "Signature" (urlEncode True signature) query

    signature = digestToBase Base64
        . hmacSHA256 (toBS _authSecretAccessKey)
        $ BS8.intercalate "\n"
            [ meth
            , _endpointHost
            , path'
            , toBS query
            ]

    query =
         pair "Version"          _svcVersion
       . pair "SignatureVersion" ("2"          :: ByteString)
       . pair "SignatureMethod"  ("HmacSHA256" :: ByteString)
       . pair "Timestamp"        time
       . pair "AWSAccessKeyId"   (toBS _authAccessKeyId)
       $ _rqQuery <> maybe mempty toQuery token

    token = ("SecurityToken" :: ByteString,) . toBS <$> _authSessionToken

    headers = hdr hDate time _rqHeaders

    time = toBS (Time t :: ISO8601)

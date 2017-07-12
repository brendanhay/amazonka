{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Sign.V2Header
-- Description : This module provides an AWS compliant V2 Header request signer. It is based heavily on boto (https://github.com/boto/boto)
--               Specifically boto's HmacAuthV1Handler AWS capable signer. Documentation in http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html
--               Notice: Limitations include inability to sign with a security token and inability to overwrite Date header with expiry.
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V2Header
    ( v2Header
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
import           Network.AWS.Data.Time
import           Network.AWS.Types
import qualified Network.HTTP.Conduit        as Client
import           Network.HTTP.Types

import qualified Network.AWS.Sign.V2Header.Base as VB

data V2Header = V2Header
    { metaTime      :: !UTCTime
    , metaEndpoint  :: !Endpoint
    , metaSignature :: !ByteString
    , headers       :: !Network.HTTP.Types.RequestHeaders
    , signer        :: !ByteString
    }

instance ToLog V2Header where
    build V2Header{..} = buildLines
        [ "[Version 2 Header Metadata] {"
        , "  time      = " <> build metaTime
        , "  endpoint  = " <> build (_endpointHost metaEndpoint)
        , "  signature = " <> build metaSignature
        , "  headers = " <> build headers
        , "  signer = " <> build signer
        , "}"
        ]

v2Header :: Signer
v2Header = Signer sign (const sign)

sign :: Algorithm a
sign Request{..} AuthEnv{..} r t = Signed meta rq
  where
    meta = Meta (V2Header t end signature headers signer)

    signer = VB.constructHeaderSigner headers meth path' _rqQuery

    rq = (clientRequest end _svcTimeout)
        { Client.method         = meth
        , Client.path           = path'
        , Client.queryString    = toBS _rqQuery
        , Client.requestHeaders = headers
        , Client.requestBody    = toRequestBody _rqBody
        }

    meth  = toBS _rqMethod
    path' = toBS (escapePath _rqPath)

    end@Endpoint{..} = _svcEndpoint r

    Service{..} = _rqService

    signature = digestToBase Base64
        . hmacSHA1 (toBS _authSecret) $ signer

    headers = hdr hDate time _rqHeaders ++ [(hAuthorization, BS8.append "AWS " (BS8.append (toBS _authAccess) (BS8.append ":" signature)))]

    time = toBS (Time t :: RFC822)

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Sign.V4
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V4
    ( V4
    , Meta (..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import qualified Data.CaseInsensitive         as CI
import qualified Data.Foldable                as Fold
import           Data.Function                (on)
import           Data.List                    (nubBy, sortBy)
import           Data.Monoid
import           GHC.TypeLits
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Log
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Time
import           Network.AWS.Request
import           Network.AWS.Types
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Types.Header

import           Prelude

data V4

data instance Meta V4 = Meta
    { metaTime             :: !UTCTime
    , metaMethod           :: !Method
    , metaPath             :: !Path
    , metaEndpoint         :: !Endpoint
    , metaCredential       :: !Credential
    , metaCanonicalQuery   :: !CanonicalQuery
    , metaCanonicalRequest :: !CanonicalRequest
    , metaCanonicalHeaders :: !CanonicalHeaders
    , metaSignedHeaders    :: !SignedHeaders
    , metaStringToSign     :: !StringToSign
    , metaSignature        :: !Signature
    , metaHeaders          :: ![Header]
    , metaBody             :: Client.RequestBody
    , metaTimeout          :: !(Maybe Seconds)
    }

instance ToLog (Meta V4) where
    build Meta{..} = buildLines
        [ "[Version 4 Metadata] {"
        , "  time              = " <> build metaTime
        , "  endpoint          = " <> build (_endpointHost metaEndpoint)
        , "  credential        = " <> build metaCredential
        , "  signed headers    = " <> build metaSignedHeaders
        , "  signature         = " <> build metaSignature
        , "  string to sign    = {"
        , build metaStringToSign
        , "}"
        , "  canonical request = {"
        , build metaCanonicalRequest
        , "  }"
        , "}"
        ]

instance AWSPresigner V4 where
    presigned auth reg ts ex svc rq = finalise meta authorise
      where
        authorise = queryString
            <>~ ("&X-Amz-Signature=" <> toBS (metaSignature meta))

        meta = sign auth reg ts svc presign digest (prepare rq)

        presign c shs =
              pair (CI.original hAMZAlgorithm)     algorithm
            . pair (CI.original hAMZCredential)    (toBS c)
            . pair (CI.original hAMZDate)          (Time ts :: AWSTime)
            . pair (CI.original hAMZExpires)       ex
            . pair (CI.original hAMZSignedHeaders) (toBS shs)
            . pair (CI.original hAMZToken)         (toBS <$> _authToken auth)

        digest = Tag "UNSIGNED-PAYLOAD"

        prepare = rqHeaders .~ []

instance AWSSigner V4 where
    signed auth reg ts svc rq = finalise meta authorise
      where
        authorise = requestHeaders
            <>~ [(hAuthorization, authorisation meta)]

        meta = sign auth reg ts svc presign digest (prepare rq)

        presign _ _ = id

        digest = Tag . digestToBase Base16 . bodySHA256 $ _rqBody rq

        prepare = rqHeaders %~
            ( hdr hHost    (_endpointHost (_svcEndpoint svc reg))
            . hdr hAMZDate (toBS (Time ts :: AWSTime))
            . maybe id (hdr hAMZToken . toBS) (_authToken auth)
            )

-- | Used to tag provenance. This allows keeping the same layout as
-- the signing documentation, passing 'ByteString's everywhere, with
-- some type guarantees.
--
-- Data.Tagged is not used for no reason other than syntactic length and
-- the ToByteString instance.
newtype Tag (s :: Symbol) a = Tag { unTag :: a }

instance ToByteString (Tag s ByteString) where toBS  = unTag
instance ToLog        (Tag s ByteString) where build = build . unTag

instance ToByteString CredentialScope where
    toBS = BS8.intercalate "/" . unTag

type Hash              = Tag "body-digest"        ByteString
type StringToSign      = Tag "string-to-sign"     ByteString
type Credential        = Tag "credential"         ByteString
type CredentialScope   = Tag "credential-scope"   [ByteString]
type CanonicalRequest  = Tag "canonical-request"  ByteString
type CanonicalHeaders  = Tag "canonical-headers"  ByteString
type CanonicalQuery    = Tag "canonical-query"    ByteString
type SignedHeaders     = Tag "signed-headers"     ByteString
type NormalisedHeaders = Tag "normalised-headers" [(ByteString, ByteString)]
type Method            = Tag "method"             ByteString
type Path              = Tag "path"               ByteString
type Signature         = Tag "signature"          ByteString

authorisation :: Meta V4 -> ByteString
authorisation Meta{..} = algorithm
    <> " Credential="     <> toBS metaCredential
    <> ", SignedHeaders=" <> toBS metaSignedHeaders
    <> ", Signature="     <> toBS metaSignature

finalise :: Meta V4 -> (ClientRequest -> ClientRequest) -> Signed V4 a
finalise m@Meta{..} authorise = Signed m (authorise rq)
  where
    rq = (clientRequest metaEndpoint metaTimeout)
        { Client.method         = toBS metaMethod
        , Client.host           = metaEndpoint ^. endpointHost
        , Client.path           = toBS metaPath
        , Client.queryString    = qry
        , Client.requestHeaders = metaHeaders
        , Client.requestBody    = metaBody
        }

    qry | BS.null x = x
        | otherwise = '?' `BS8.cons` x
      where
        x = toBS metaCanonicalQuery

sign :: AuthEnv
     -> Region
     -> UTCTime
     -> Service s
     -> (Credential -> SignedHeaders -> QueryString -> QueryString)
     -> Hash
     -> Request a
     -> Meta V4
sign auth reg ts svc presign digest rq = Meta
    { metaTime             = ts
    , metaMethod           = method
    , metaPath             = path
    , metaEndpoint         = end
    , metaCredential       = cred
    , metaCanonicalQuery   = query
    , metaCanonicalRequest = crq
    , metaCanonicalHeaders = chs
    , metaSignedHeaders    = shs
    , metaStringToSign     = sts
    , metaSignature        = signature (_authSecret auth) scope sts
    , metaHeaders          = _rqHeaders rq
    , metaBody             = bodyRequest (_rqBody rq)
    , metaTimeout          = _svcTimeout svc
    }
  where
    query = canonicalQuery . presign cred shs $ _rqQuery rq

    sts   = stringToSign ts scope crq
    cred  = credential (_authAccess auth) scope
    scope = credentialScope svc end ts
    crq   = canonicalRequest method path digest query chs shs

    chs     = canonicalHeaders headers
    shs     = signedHeaders    headers
    headers = normaliseHeaders (_rqHeaders rq)

    end    = _svcEndpoint svc reg
    method = Tag . toBS $ _rqMethod rq
    path   = escapedPath svc rq

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256"

signature :: SecretKey -> CredentialScope -> StringToSign -> Signature
signature k c = Tag . digestToBase Base16 . hmacSHA256 signingKey . unTag
  where
    signingKey = Fold.foldl' hmac ("AWS4" <> toBS k) (unTag c)

    hmac x y = digestToBS (hmacSHA256 x y)

stringToSign :: UTCTime -> CredentialScope -> CanonicalRequest -> StringToSign
stringToSign t c r = Tag $ BS8.intercalate "\n"
    [ algorithm
    , toBS (Time t :: AWSTime)
    , toBS c
    , digestToBase Base16 . hashSHA256 $ toBS r
    ]

credential :: AccessKey -> CredentialScope -> Credential
credential k c = Tag (toBS k <> "/" <> toBS c)

credentialScope :: Service s -> Endpoint -> UTCTime -> CredentialScope
credentialScope s e t = Tag
    [ toBS (Time t :: BasicTime)
    , toBS (_endpointScope e)
    , toBS (_svcPrefix     s)
    , "aws4_request"
    ]

canonicalRequest :: Method
                 -> Path
                 -> Hash
                 -> CanonicalQuery
                 -> CanonicalHeaders
                 -> SignedHeaders
                 -> CanonicalRequest
canonicalRequest meth path digest query chs shs = Tag $
   BS8.intercalate "\n"
       [ toBS meth
       , toBS path
       , toBS query
       , toBS chs
       , toBS shs
       , toBS digest
       ]

escapedPath :: Service s -> Request a -> Path
escapedPath s r = Tag . toBS . escapePath $
    case _svcAbbrev s of
        "S3" -> _rqPath r
        _    -> collapsePath (_rqPath r)

canonicalQuery :: QueryString -> CanonicalQuery
canonicalQuery = Tag . toBS

-- FIXME: the following use of stripBS is too naive, should remove
-- all internal whitespace, replacing with a single space char,
-- unless quoted with \"...\"
canonicalHeaders :: NormalisedHeaders -> CanonicalHeaders
canonicalHeaders = Tag . Fold.foldMap (uncurry f) . unTag
  where
    f k v = k <> ":" <> stripBS v <> "\n"

signedHeaders :: NormalisedHeaders -> SignedHeaders
signedHeaders = Tag . BS8.intercalate ";" . map fst . unTag

normaliseHeaders :: [Header] -> NormalisedHeaders
normaliseHeaders = Tag
    . map    (first CI.foldedCase)
    . nubBy  ((==)    `on` fst)
    . sortBy (compare `on` fst)
    . filter ((/= "authorization") . fst)

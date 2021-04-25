{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}

-- |
-- Module      : Network.AWS.Sign.V4.Base
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V4.Base where

import           Data.Bifunctor
import           Data.Function               (on)
import           Data.List                   (nubBy, sortBy)
import           Data.Monoid

import           GHC.TypeLits

import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Log
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Sensitive  (_Sensitive)
import           Network.AWS.Data.Time
import           Network.AWS.Lens            ((%~), (<>~), (^.))
import           Network.AWS.Request
import           Network.AWS.Types

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Data.CaseInsensitive        as CI
import qualified Data.Foldable               as Fold
import qualified Network.HTTP.Conduit        as Client
import qualified Network.HTTP.Types.Header   as H

default (ByteString)

data V4 = V4
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
    , metaTimeout          :: !(Maybe Seconds)
    }

instance ToLog V4 where
    build V4{..} = buildLines
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

base :: Hash
     -> Request a
     -> AuthEnv
     -> Region
     -> UTCTime
     -> (V4, ClientRequest -> ClientRequest)
base h rq a r ts = (meta, auth)
  where
    auth = requestHeaders <>~ [(H.hAuthorization, authorisation meta)]

    meta = signMetadata a r ts presigner h (prepare rq)

    presigner _ _ = id

    prepare = rqHeaders %~
        ( hdr hHost             (_endpointHost end)
        . hdr hAMZDate          (toBS (Time ts :: AWSTime))
        . hdr hAMZContentSHA256 (toBS h)
        . maybe id (hdr hAMZToken . toBS) (_authSessionToken a)
        )

    end = _svcEndpoint (_rqService rq) r

-- | Used to tag provenance. This allows keeping the same layout as
-- the signing documentation, passing 'ByteString's everywhere, with
-- some type guarantees.
--
-- Data.Tagged is not used for no reason other than syntactic length and
-- the ToByteString instance.
newtype Tag (s :: Symbol) a = Tag { unTag :: a } deriving (Show)

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

authorisation :: V4 -> ByteString
authorisation V4{..} = algorithm
    <> " Credential="     <> toBS metaCredential
    <> ", SignedHeaders=" <> toBS metaSignedHeaders
    <> ", Signature="     <> toBS metaSignature

signRequest :: V4                               -- ^ Pre-signRequestd signing metadata.
            -> Client.RequestBody               -- ^ The request body.
            -> (ClientRequest -> ClientRequest) -- ^ Insert authentication information.
            -> Signed a
signRequest m@V4{..} b auth = Signed (Meta m) (auth rq)
  where
    rq = (clientRequest metaEndpoint metaTimeout)
        { Client.method         = toBS metaMethod
        , Client.path           = toBS metaPath
        , Client.queryString    = qry
        , Client.requestHeaders = metaHeaders
        , Client.requestBody    = b
        }

    qry | BS.null x = x
        | otherwise = '?' `BS8.cons` x
      where
        x = toBS metaCanonicalQuery

signMetadata :: AuthEnv
             -> Region
             -> UTCTime
             -> (Credential -> SignedHeaders -> QueryString -> QueryString)
             -> Hash
             -> Request a
             -> V4
signMetadata a r ts presign digest rq = V4
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
    , metaSignature        = signature (_authSecretAccessKey a ^. _Sensitive) scope sts
    , metaHeaders          = _rqHeaders rq
    , metaTimeout          = _svcTimeout svc
    }
  where
    query = canonicalQuery . presign cred shs $ _rqQuery rq

    sts   = stringToSign ts scope crq
    cred  = credential (_authAccessKeyId a) scope
    scope = credentialScope svc end ts
    crq   = canonicalRequest method path digest query chs shs

    chs     = canonicalHeaders headers
    shs     = signedHeaders    headers
    headers = normaliseHeaders (_rqHeaders rq)

    end    = _svcEndpoint svc r
    method = Tag . toBS $ _rqMethod rq
    path   = escapedPath rq

    svc    = _rqService rq

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

credentialScope :: Service -> Endpoint -> UTCTime -> CredentialScope
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

escapedPath :: Request a -> Path
escapedPath r = Tag . toBS . escapePath $
    case _svcAbbrev (_rqService r) of
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
    . filter ((/= "content-length") . fst)

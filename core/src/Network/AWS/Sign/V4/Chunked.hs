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
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Network.AWS.Sign.V4.Chunked
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V4.Chunked
    ( chunked
    ) where

import Data.ByteString.Builder
import Data.Conduit
import Data.Maybe
import Data.Monoid

import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.Crypto
import Network.AWS.Data.Headers
import Network.AWS.Data.Sensitive  (_Sensitive)
import Network.AWS.Data.Time
import Network.AWS.Lens            ((<>~), (^.))
import Network.AWS.Sign.V4.Base    hiding (algorithm)
import Network.AWS.Types
import Network.HTTP.Types.Header

import Numeric (showHex)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

default (Builder, Integer)

chunked :: ChunkedBody -> Algorithm a
chunked c rq a r ts = signRequest meta (toRequestBody body) auth
  where
    (meta, auth) = base (Tag digest) (prepare rq) a r ts

    prepare = rqHeaders <>~
        [ (hContentEncoding,         "aws-chunked")
        , (hAMZDecodedContentLength, toBS (_chunkedLength c))
        , (hContentLength,           toBS (metadataLength   c))
        ]

    body = Chunked (c `fuseChunks` sign (metaSignature meta))

    sign :: Monad m => Signature -> ConduitM ByteString ByteString m ()
    sign prev = do
        mx <- await
        let next = chunkSignature prev (fromMaybe mempty mx)
        case mx of
            Nothing -> yield (chunkData next mempty)
            Just x  -> yield (chunkData next x) >> sign next

    chunkData next x = toBS
         $ word64Hex  (fromIntegral (BS.length x))
        <> byteString chunkSignatureHeader
        <> byteString (toBS next)
        <> byteString crlf
        <> byteString x
        <> byteString crlf

    chunkSignature prev x =
        signature (_authSecretAccessKey a ^. _Sensitive) scope (chunkStringToSign prev x)

    chunkStringToSign prev x = Tag $ BS8.intercalate "\n"
        [ algorithm
        , time
        , toBS scope
        , toBS prev
        , sha256Empty
        , sha256 x
        ]

    time :: ByteString
    time = toBS (Time ts :: AWSTime)

    scope :: CredentialScope
    scope = credentialScope (_rqService rq) end ts

    end :: Endpoint
    end = _svcEndpoint (_rqService rq) r

metadataLength :: ChunkedBody -> Integer
metadataLength c =
      -- Number of full sized chunks.
      fullChunks c * chunkLength (_chunkedSize c)
      -- Non-full chunk preceeding the final chunk.
    + maybe 0 chunkLength (remainderBytes c)
      -- The final empty chunk.
    + chunkLength 0
  where
    chunkLength :: Integral a => a -> Integer
    chunkLength (toInteger -> n) =
          fromIntegral (length (showHex n ""))
        + headerLength
        + signatureLength
        + crlfLength
        + n
        + crlfLength

    headerLength    = toInteger (BS.length chunkSignatureHeader)
    crlfLength      = toInteger (BS.length crlf)
    signatureLength = 64

sha256 :: ByteString -> ByteString
sha256 = digestToBase Base16 . hashSHA256

sha256Empty :: ByteString
sha256Empty = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256-PAYLOAD"

digest :: ByteString
digest = "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"

chunkSignatureHeader :: ByteString
chunkSignatureHeader = ";chunk-signature="

crlf :: ByteString
crlf = "\r\n"

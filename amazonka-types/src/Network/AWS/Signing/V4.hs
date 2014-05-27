{-# LANGUAGE RecordWildCards #-}

-- Module      : Network.AWS.Signing.V4
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.V4 (V4) where

import           Data.List                 (sort)
import qualified Data.Text.Lazy.Builder    as Build
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Types

data V4

-- try and make it look the same as the documentation's flow?

-- v2 returns a modified querystring, and headers
-- v3 returns modified headers
-- v4 returns modified headers

-- Authorization: AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, 
--   SignedHeaders=host;range;x-amz-date,
--   Signature=fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024

instance SigningAlgorithm V4 where
    finalise = undefined
      -- where
      --   canonicalRequest = BS.intercalate "\n"
      --      [ toByteString rqMethod
      --      , canonicalURI
      --      , canonicalQuery
      --      , canonicalHeaders
      --      , signedHeaders
      --      , hashedPayload
      --      ]

-- | CanonicalURI is the URI-encoded version of the absolute path component of the
-- URI—everything starting with the "/" that follows the domain name and up to the
-- end of the string or to the question mark character ('?') if you have query
-- string parameters. For example, in the URI
--
-- http://s3.amazonaws.com/examplebucket/myphoto.jpg /examplebucket/myphoto.jpg is
-- the absolute path. In the absolute path, you don't encode the "/".
-- canonicalURI = encodeURI False . rqPath

-- | CanonicalQueryString specifies the URI-encoded query string parameters.
-- You URI-encode name and values individually.
-- You must also sort the parameters in the canonical query string alphabetically
-- by key name. The sorting occurs after encoding. For example, in the URI:
-- http://s3.amazonaws.com/examplebucket?prefix=somePrefix&marker=someMarker&max-keys=20
--
-- the query string is prefix=somePrefix&marker=someMarker&max-keys=20. The
-- canonical query string is as follows. Line breaks are added to this example for
-- readability:
--
-- URI-encode("marker")+"="+URI-encode("someMarker")+"&"+
-- URI-encode("max-keys")+"="+URI-encode("20") + "&" +
-- URI-encode("prefix")+"="+URI-encode("somePrefix")
-- When a request targets a subresource, the corresponding query parameter value
-- will be an empty string (""). For example, the following URI identifies the ACL
-- subresource on the examplebucket bucket:
-- http://s3.amazonaws.com/examplebucket?acl
--
-- The CanonicalQueryString in this case is:
-- URI-encode("acl") + "=" + ""
-- If the URI does not include a '?', there is no query string in the request, and
-- you set the canonical query string to an empty string (""). You will still need
-- to include the "\n".
canonicalQuery = renderQuery "&" "=" Build.fromText . encodedQuery

encodedQuery :: Request a -> Query
encodedQuery = encodeQuery (encodeURI True) . rqQuery

-- | CanonicalHeaders is a list of request headers with their values.
-- Individual header name and value pairs are separated by the newline character
-- ("\n"). Header names must be in lowercase. You must sort the header names
-- alphabetically to construct the string, as shown in the following example:
--
-- Lowercase(<HeaderName1>)+":"+Trim(<value>)+"\n"
-- Lowercase(<HeaderName2>)+":"+Trim(<value>)+"\n"
-- ...
-- Lowercase(<HeaderNameN>)+":"+Trim(<value>)+"\n"
--
-- The CanonicalHeaders list must include the following:
--
-- HTTP host header
-- If the Content-Type header is present in the request, it must be added to the
-- CanonicalHeaders list.
-- Any x-amz-* headers that you plan to include in your request must also be
-- added. For example, if you are using temporary security credentials, you will
-- include x-amz-security-token in your request. You must add this header in the
-- list of CanonicalHeaders.
--
-- The following is an example CanonicalHeaders string. The header names are in
-- lowercase and sorted.
-- host:s3.amazonaws.com
-- x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b785
-- 2b855
-- x-amz-date:20130708T220855Z
--
-- Note: For the purpose of calculating a signature, only the host and any x-amz-*
-- headers are required; however, in order to prevent data tampering, you should
-- consider including all the headers in the signature calculation. The
-- x-amz-content-sha256 header in the previous example provides a hash of the
-- request payload. If there is no payload, you provide the hash of an empty
-- string.
-- canonicalHeaders
--   where


-- stringToSign
--   where
--     algorithm = "AWS4-HMAC-SHA256" + \n +

--     timeStamp = + ISO8601Time \n +

--     scope = BS.intercalate "/"
--         [ BasicTime t,
--         , r
--         , service name
--         ,
--         ]
--         + \n

--     hex(sha256hash(canonicalRequest))

-- signature
--   where
--     dateKey
--     dateRegionKey
--     dateRegionServiceKey
--     signingKey

--     hmac-sha256(signingKey, stringToSign)



-- --    finalise s@Service{..} c@Context{..} Auth{..} r t = undefined

-- --       where
-- --         host = endpoint s r
-- --         meth = toByteString ctxMethod
-- --         path = toByteString ctxPath
-- --         reg  = toByteString r

-- --         headers = hAMZDate t : maybeToList (hAMZToken <$> sigToken) ++ ctxHeaders

-- --         algorithm = "AWS4-HMAC-SHA256"

-- --         authorisation = mconcat
-- --             [ algorithm
-- --             , " Credential="
-- --             , authAccess
-- --             , "/"
-- --             , credentialScope
-- --             , ", SignedHeaders="
-- --             , signedHeaders
-- --             , ", Signature="
-- --             , signature
-- --             ]

-- --         signature = Base16.encode $ hmacSHA256 signingKey stringToSign

-- --         signingKey = foldl1 hmacSHA256 $ ("AWS4" <> authSecret) : scope

-- --         stringToSign = BS.intercalate "\n"
-- --             [ algorithm
-- --             , toByteString (AWSTime t)
-- --             , credentialScope
-- --             , Base16.encode $ SHA256.hash canonicalRequest
-- --             ]

-- --         credentialScope = BS.intercalate "/" scope

-- --         scope =
-- --             [ toByteString (BasicTime t)
-- --             , reg
-- --             , svcName
-- --             , "aws4sigRequest"
-- --             ]

-- --         canonicalRequest = BS.intercalate "\n"
-- --             [ meth
-- --             , path
-- --             , query
-- --             , canonicalHeaders
-- --             , signedHeaders
-- --             , bodySHA256
-- --             ]

-- --         canonicalHeaders = mconcat $ map flattenValues grouped

-- --         signedHeaders = BS.intercalate ";" . nub $ map (CI.foldedCase . fst) grouped

-- --         grouped = groupHeaders headers

-- --         bodySHA256 = Base16.encode $ SHA256.hash ""

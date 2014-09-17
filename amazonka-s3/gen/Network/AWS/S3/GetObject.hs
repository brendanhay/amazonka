{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves objects from Amazon S3.
module Network.AWS.S3.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , mkGetObject
    -- ** Request lenses
    , goBucket
    , goIfMatch
    , goIfModifiedSince
    , goIfNoneMatch
    , goIfUnmodifiedSince
    , goKey
    , goRange
    , goResponseCacheControl
    , goResponseContentDisposition
    , goResponseContentEncoding
    , goResponseContentLanguage
    , goResponseContentType
    , goResponseExpires
    , goVersionId
    , goSSECustomerAlgorithm
    , goSSECustomerKey
    , goSSECustomerKeyMD5

    -- * Response
    , GetObjectResponse
    -- ** Response constructor
    , mkGetObjectResponse
    -- ** Response lenses
    , gorBody
    , gorDeleteMarker
    , gorAcceptRanges
    , gorExpiration
    , gorRestore
    , gorLastModified
    , gorContentLength
    , gorETag
    , gorMissingMeta
    , gorVersionId
    , gorCacheControl
    , gorContentDisposition
    , gorContentEncoding
    , gorContentLanguage
    , gorContentType
    , gorExpires
    , gorWebsiteRedirectLocation
    , gorServerSideEncryption
    , gorMetadata
    , gorSSECustomerAlgorithm
    , gorSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObject = GetObject
    { _goBucket :: BucketName
    , _goIfMatch :: Maybe Text
    , _goIfModifiedSince :: Maybe RFC822
    , _goIfNoneMatch :: Maybe Text
    , _goIfUnmodifiedSince :: Maybe RFC822
    , _goKey :: ObjectKey
    , _goRange :: Maybe Text
    , _goResponseCacheControl :: Maybe Text
    , _goResponseContentDisposition :: Maybe Text
    , _goResponseContentEncoding :: Maybe Text
    , _goResponseContentLanguage :: Maybe Text
    , _goResponseContentType :: Maybe Text
    , _goResponseExpires :: Maybe RFC822
    , _goVersionId :: Maybe ObjectVersionId
    , _goSSECustomerAlgorithm :: Maybe Text
    , _goSSECustomerKey :: Maybe Text
    , _goSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObject' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @IfMatch ::@ @Maybe Text@
--
-- * @IfModifiedSince ::@ @Maybe RFC822@
--
-- * @IfNoneMatch ::@ @Maybe Text@
--
-- * @IfUnmodifiedSince ::@ @Maybe RFC822@
--
-- * @Key ::@ @ObjectKey@
--
-- * @Range ::@ @Maybe Text@
--
-- * @ResponseCacheControl ::@ @Maybe Text@
--
-- * @ResponseContentDisposition ::@ @Maybe Text@
--
-- * @ResponseContentEncoding ::@ @Maybe Text@
--
-- * @ResponseContentLanguage ::@ @Maybe Text@
--
-- * @ResponseContentType ::@ @Maybe Text@
--
-- * @ResponseExpires ::@ @Maybe RFC822@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKey ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
mkGetObject :: BucketName -- ^ 'goBucket'
            -> ObjectKey -- ^ 'goKey'
            -> GetObject
mkGetObject p1 p6 = GetObject
    { _goBucket = p1
    , _goIfMatch = Nothing
    , _goIfModifiedSince = Nothing
    , _goIfNoneMatch = Nothing
    , _goIfUnmodifiedSince = Nothing
    , _goKey = p6
    , _goRange = Nothing
    , _goResponseCacheControl = Nothing
    , _goResponseContentDisposition = Nothing
    , _goResponseContentEncoding = Nothing
    , _goResponseContentLanguage = Nothing
    , _goResponseContentType = Nothing
    , _goResponseExpires = Nothing
    , _goVersionId = Nothing
    , _goSSECustomerAlgorithm = Nothing
    , _goSSECustomerKey = Nothing
    , _goSSECustomerKeyMD5 = Nothing
    }

goBucket :: Lens' GetObject BucketName
goBucket = lens _goBucket (\s a -> s { _goBucket = a })

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\s a -> s { _goIfMatch = a })

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe RFC822)
goIfModifiedSince =
    lens _goIfModifiedSince (\s a -> s { _goIfModifiedSince = a })

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\s a -> s { _goIfNoneMatch = a })

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe RFC822)
goIfUnmodifiedSince =
    lens _goIfUnmodifiedSince (\s a -> s { _goIfUnmodifiedSince = a })

goKey :: Lens' GetObject ObjectKey
goKey = lens _goKey (\s a -> s { _goKey = a })

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\s a -> s { _goRange = a })

-- | Sets the Cache-Control header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl =
    lens _goResponseCacheControl (\s a -> s { _goResponseCacheControl = a })

-- | Sets the Content-Disposition header of the response.
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition =
    lens _goResponseContentDisposition
         (\s a -> s { _goResponseContentDisposition = a })

-- | Sets the Content-Encoding header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding =
    lens _goResponseContentEncoding
         (\s a -> s { _goResponseContentEncoding = a })

-- | Sets the Content-Language header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage =
    lens _goResponseContentLanguage
         (\s a -> s { _goResponseContentLanguage = a })

-- | Sets the Content-Type header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType =
    lens _goResponseContentType (\s a -> s { _goResponseContentType = a })

-- | Sets the Expires header of the response.
goResponseExpires :: Lens' GetObject (Maybe RFC822)
goResponseExpires =
    lens _goResponseExpires (\s a -> s { _goResponseExpires = a })

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe ObjectVersionId)
goVersionId = lens _goVersionId (\s a -> s { _goVersionId = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm =
    lens _goSSECustomerAlgorithm (\s a -> s { _goSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey =
    lens _goSSECustomerKey (\s a -> s { _goSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 =
    lens _goSSECustomerKeyMD5 (\s a -> s { _goSSECustomerKeyMD5 = a })

instance ToPath GetObject

instance ToQuery GetObject

instance ToHeaders GetObject where
    toHeaders GetObject{..} = concat
        [ "If-Match" =: _goIfMatch
        , "If-Modified-Since" =: _goIfModifiedSince
        , "If-None-Match" =: _goIfNoneMatch
        , "If-Unmodified-Since" =: _goIfUnmodifiedSince
        , "Range" =: _goRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _goSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _goSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _goSSECustomerKeyMD5
        ]

instance ToBody GetObject

data GetObjectResponse = GetObjectResponse
    { _gorBody :: RsBody
    , _gorDeleteMarker :: Maybe Bool
    , _gorAcceptRanges :: Maybe Text
    , _gorExpiration :: Maybe RFC822
    , _gorRestore :: Maybe Text
    , _gorLastModified :: Maybe RFC822
    , _gorContentLength :: Maybe Integer
    , _gorETag :: Maybe ETag
    , _gorMissingMeta :: Maybe Integer
    , _gorVersionId :: Maybe ObjectVersionId
    , _gorCacheControl :: Maybe Text
    , _gorContentDisposition :: Maybe Text
    , _gorContentEncoding :: Maybe Text
    , _gorContentLanguage :: Maybe Text
    , _gorContentType :: Maybe Text
    , _gorExpires :: Maybe RFC822
    , _gorWebsiteRedirectLocation :: Maybe Text
    , _gorServerSideEncryption :: Maybe ServerSideEncryption
    , _gorMetadata :: Map Text Text
    , _gorSSECustomerAlgorithm :: Maybe Text
    , _gorSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Body ::@ @RsBody@
--
-- * @DeleteMarker ::@ @Maybe Bool@
--
-- * @AcceptRanges ::@ @Maybe Text@
--
-- * @Expiration ::@ @Maybe RFC822@
--
-- * @Restore ::@ @Maybe Text@
--
-- * @LastModified ::@ @Maybe RFC822@
--
-- * @ContentLength ::@ @Maybe Integer@
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @MissingMeta ::@ @Maybe Integer@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @CacheControl ::@ @Maybe Text@
--
-- * @ContentDisposition ::@ @Maybe Text@
--
-- * @ContentEncoding ::@ @Maybe Text@
--
-- * @ContentLanguage ::@ @Maybe Text@
--
-- * @ContentType ::@ @Maybe Text@
--
-- * @Expires ::@ @Maybe RFC822@
--
-- * @WebsiteRedirectLocation ::@ @Maybe Text@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @Metadata ::@ @Map Text Text@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
mkGetObjectResponse :: RsBody -- ^ 'gorBody'
                    -> GetObjectResponse
mkGetObjectResponse p1 = GetObjectResponse
    { _gorBody = p1
    , _gorDeleteMarker = Nothing
    , _gorAcceptRanges = Nothing
    , _gorExpiration = Nothing
    , _gorRestore = Nothing
    , _gorLastModified = Nothing
    , _gorContentLength = Nothing
    , _gorETag = Nothing
    , _gorMissingMeta = Nothing
    , _gorVersionId = Nothing
    , _gorCacheControl = Nothing
    , _gorContentDisposition = Nothing
    , _gorContentEncoding = Nothing
    , _gorContentLanguage = Nothing
    , _gorContentType = Nothing
    , _gorExpires = Nothing
    , _gorWebsiteRedirectLocation = Nothing
    , _gorServerSideEncryption = Nothing
    , _gorMetadata = mempty
    , _gorSSECustomerAlgorithm = Nothing
    , _gorSSECustomerKeyMD5 = Nothing
    }

-- | Object data.
gorBody :: Lens' GetObjectResponse RsBody
gorBody = lens _gorBody (\s a -> s { _gorBody = a })

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gorDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorDeleteMarker = lens _gorDeleteMarker (\s a -> s { _gorDeleteMarker = a })

gorAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorAcceptRanges = lens _gorAcceptRanges (\s a -> s { _gorAcceptRanges = a })

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gorExpiration :: Lens' GetObjectResponse (Maybe RFC822)
gorExpiration = lens _gorExpiration (\s a -> s { _gorExpiration = a })

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
gorRestore :: Lens' GetObjectResponse (Maybe Text)
gorRestore = lens _gorRestore (\s a -> s { _gorRestore = a })

-- | Last modified date of the object.
gorLastModified :: Lens' GetObjectResponse (Maybe RFC822)
gorLastModified = lens _gorLastModified (\s a -> s { _gorLastModified = a })

-- | Size of the body in bytes.
gorContentLength :: Lens' GetObjectResponse (Maybe Integer)
gorContentLength =
    lens _gorContentLength (\s a -> s { _gorContentLength = a })

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gorETag :: Lens' GetObjectResponse (Maybe ETag)
gorETag = lens _gorETag (\s a -> s { _gorETag = a })

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
gorMissingMeta :: Lens' GetObjectResponse (Maybe Integer)
gorMissingMeta = lens _gorMissingMeta (\s a -> s { _gorMissingMeta = a })

-- | Version of the object.
gorVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorVersionId = lens _gorVersionId (\s a -> s { _gorVersionId = a })

-- | Specifies caching behavior along the request/reply chain.
gorCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorCacheControl = lens _gorCacheControl (\s a -> s { _gorCacheControl = a })

-- | Specifies presentational information for the object.
gorContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorContentDisposition =
    lens _gorContentDisposition (\s a -> s { _gorContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gorContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorContentEncoding =
    lens _gorContentEncoding (\s a -> s { _gorContentEncoding = a })

-- | The language the content is in.
gorContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorContentLanguage =
    lens _gorContentLanguage (\s a -> s { _gorContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
gorContentType :: Lens' GetObjectResponse (Maybe Text)
gorContentType = lens _gorContentType (\s a -> s { _gorContentType = a })

-- | The date and time at which the object is no longer cacheable.
gorExpires :: Lens' GetObjectResponse (Maybe RFC822)
gorExpires = lens _gorExpires (\s a -> s { _gorExpires = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gorWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorWebsiteRedirectLocation =
    lens _gorWebsiteRedirectLocation
         (\s a -> s { _gorWebsiteRedirectLocation = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
gorServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorServerSideEncryption =
    lens _gorServerSideEncryption
         (\s a -> s { _gorServerSideEncryption = a })

-- | A map of metadata to store with the object in S3.
gorMetadata :: Lens' GetObjectResponse (Map Text Text)
gorMetadata = lens _gorMetadata (\s a -> s { _gorMetadata = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
gorSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerAlgorithm =
    lens _gorSSECustomerAlgorithm
         (\s a -> s { _gorSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gorSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerKeyMD5 =
    lens _gorSSECustomerKeyMD5 (\s a -> s { _gorSSECustomerKeyMD5 = a })

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectResponse

    request = get
    response _ = bodyResponse $ \hs bdy ->
        return $! pure GetObjectResponse
            <*> pure (RsBody bdy)
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "accept-ranges"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-restore"
            <*> hs ~:? "Last-Modified"
            <*> hs ~:? "Content-Length"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-missing-meta"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "Cache-Control"
            <*> hs ~:? "Content-Disposition"
            <*> hs ~:? "Content-Encoding"
            <*> hs ~:? "Content-Language"
            <*> hs ~:? "Content-Type"
            <*> hs ~:? "Expires"
            <*> hs ~:? "x-amz-website-redirect-location"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> (Map <$> hs ~:: "x-amz-meta-")
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a copy of an object that is already stored in Amazon S3.
module Network.AWS.S3.CopyObject
    (
    -- * Request
      CopyObject
    -- ** Request constructor
    , copyObject
    -- ** Request lenses
    , corACL
    , corBucket
    , corCacheControl
    , corContentDisposition
    , corContentEncoding
    , corContentLanguage
    , corContentType
    , corCopySource
    , corCopySourceIfMatch
    , corCopySourceIfModifiedSince
    , corCopySourceIfNoneMatch
    , corCopySourceIfUnmodifiedSince
    , corCopySourceSSECustomerAlgorithm
    , corCopySourceSSECustomerKey
    , corCopySourceSSECustomerKeyMD5
    , corExpires
    , corGrantFullControl
    , corGrantRead
    , corGrantReadACP
    , corGrantWriteACP
    , corKey
    , corMetadata
    , corMetadataDirective
    , corSSECustomerAlgorithm
    , corSSECustomerKey
    , corSSECustomerKeyMD5
    , corServerSideEncryption
    , corStorageClass
    , corWebsiteRedirectLocation

    -- * Response
    , CopyObjectOutput
    -- ** Response constructor
    , copyObjectOutput
    -- ** Response lenses
    , cooCopyObjectResult
    , cooCopySourceVersionId
    , cooExpiration
    , cooSSECustomerAlgorithm
    , cooSSECustomerKeyMD5
    , cooServerSideEncryption
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data CopyObject = CopyObject
    { _corACL                            :: Maybe Text
    , _corBucket                         :: Text
    , _corCacheControl                   :: Maybe Text
    , _corContentDisposition             :: Maybe Text
    , _corContentEncoding                :: Maybe Text
    , _corContentLanguage                :: Maybe Text
    , _corContentType                    :: Maybe Text
    , _corCopySource                     :: Text
    , _corCopySourceIfMatch              :: Maybe Text
    , _corCopySourceIfModifiedSince      :: Maybe RFC822
    , _corCopySourceIfNoneMatch          :: Maybe Text
    , _corCopySourceIfUnmodifiedSince    :: Maybe RFC822
    , _corCopySourceSSECustomerAlgorithm :: Maybe Text
    , _corCopySourceSSECustomerKey       :: Maybe (Sensitive Text)
    , _corCopySourceSSECustomerKeyMD5    :: Maybe Text
    , _corExpires                        :: Maybe RFC822
    , _corGrantFullControl               :: Maybe Text
    , _corGrantRead                      :: Maybe Text
    , _corGrantReadACP                   :: Maybe Text
    , _corGrantWriteACP                  :: Maybe Text
    , _corKey                            :: Text
    , _corMetadata                       :: Map Text Text
    , _corMetadataDirective              :: Maybe Text
    , _corSSECustomerAlgorithm           :: Maybe Text
    , _corSSECustomerKey                 :: Maybe (Sensitive Text)
    , _corSSECustomerKeyMD5              :: Maybe Text
    , _corServerSideEncryption           :: Maybe Text
    , _corStorageClass                   :: Maybe Text
    , _corWebsiteRedirectLocation        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CopyObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corACL' @::@ 'Maybe' 'Text'
--
-- * 'corBucket' @::@ 'Text'
--
-- * 'corCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'corContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'corContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'corContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'corContentType' @::@ 'Maybe' 'Text'
--
-- * 'corCopySource' @::@ 'Text'
--
-- * 'corCopySourceIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'corCopySourceIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'corCopySourceIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'corCopySourceIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'corCopySourceSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'corCopySourceSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'corCopySourceSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'corExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'corGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'corGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'corGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'corGrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'corKey' @::@ 'Text'
--
-- * 'corMetadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'corMetadataDirective' @::@ 'Maybe' 'Text'
--
-- * 'corSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'corSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'corSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'corServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'corStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'corWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
copyObject :: Text -- ^ 'corBucket'
           -> Text -- ^ 'corCopySource'
           -> Text -- ^ 'corKey'
           -> CopyObject
copyObject p1 p2 p3 = CopyObject
    { _corBucket                         = p1
    , _corCopySource                     = p2
    , _corKey                            = p3
    , _corACL                            = Nothing
    , _corCacheControl                   = Nothing
    , _corContentDisposition             = Nothing
    , _corContentEncoding                = Nothing
    , _corContentLanguage                = Nothing
    , _corContentType                    = Nothing
    , _corCopySourceIfMatch              = Nothing
    , _corCopySourceIfModifiedSince      = Nothing
    , _corCopySourceIfNoneMatch          = Nothing
    , _corCopySourceIfUnmodifiedSince    = Nothing
    , _corExpires                        = Nothing
    , _corGrantFullControl               = Nothing
    , _corGrantRead                      = Nothing
    , _corGrantReadACP                   = Nothing
    , _corGrantWriteACP                  = Nothing
    , _corMetadata                       = mempty
    , _corMetadataDirective              = Nothing
    , _corServerSideEncryption           = Nothing
    , _corStorageClass                   = Nothing
    , _corWebsiteRedirectLocation        = Nothing
    , _corSSECustomerAlgorithm           = Nothing
    , _corSSECustomerKey                 = Nothing
    , _corSSECustomerKeyMD5              = Nothing
    , _corCopySourceSSECustomerAlgorithm = Nothing
    , _corCopySourceSSECustomerKey       = Nothing
    , _corCopySourceSSECustomerKeyMD5    = Nothing
    }

-- | The canned ACL to apply to the object.
corACL :: Lens' CopyObject (Maybe Text)
corACL = lens _corACL (\s a -> s { _corACL = a })

corBucket :: Lens' CopyObject Text
corBucket = lens _corBucket (\s a -> s { _corBucket = a })

-- | Specifies caching behavior along the request/reply chain.
corCacheControl :: Lens' CopyObject (Maybe Text)
corCacheControl = lens _corCacheControl (\s a -> s { _corCacheControl = a })

-- | Specifies presentational information for the object.
corContentDisposition :: Lens' CopyObject (Maybe Text)
corContentDisposition =
    lens _corContentDisposition (\s a -> s { _corContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
corContentEncoding :: Lens' CopyObject (Maybe Text)
corContentEncoding =
    lens _corContentEncoding (\s a -> s { _corContentEncoding = a })

-- | The language the content is in.
corContentLanguage :: Lens' CopyObject (Maybe Text)
corContentLanguage =
    lens _corContentLanguage (\s a -> s { _corContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
corContentType :: Lens' CopyObject (Maybe Text)
corContentType = lens _corContentType (\s a -> s { _corContentType = a })

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (/). Must be URL-encoded.
corCopySource :: Lens' CopyObject Text
corCopySource = lens _corCopySource (\s a -> s { _corCopySource = a })

-- | Copies the object if its entity tag (ETag) matches the specified tag.
corCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfMatch =
    lens _corCopySourceIfMatch (\s a -> s { _corCopySourceIfMatch = a })

-- | Copies the object if it has been modified since the specified time.
corCopySourceIfModifiedSince :: Lens' CopyObject (Maybe UTCTime)
corCopySourceIfModifiedSince =
    lens _corCopySourceIfModifiedSince
        (\s a -> s { _corCopySourceIfModifiedSince = a })
            . mapping _Time

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
corCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfNoneMatch =
    lens _corCopySourceIfNoneMatch
        (\s a -> s { _corCopySourceIfNoneMatch = a })

-- | Copies the object if it hasn't been modified since the specified time.
corCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe UTCTime)
corCopySourceIfUnmodifiedSince =
    lens _corCopySourceIfUnmodifiedSince
        (\s a -> s { _corCopySourceIfUnmodifiedSince = a })
            . mapping _Time

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
corCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerAlgorithm =
    lens _corCopySourceSSECustomerAlgorithm
        (\s a -> s { _corCopySourceSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
corCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKey =
    lens _corCopySourceSSECustomerKey
        (\s a -> s { _corCopySourceSSECustomerKey = a })
            . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKeyMD5 =
    lens _corCopySourceSSECustomerKeyMD5
        (\s a -> s { _corCopySourceSSECustomerKeyMD5 = a })

-- | The date and time at which the object is no longer cacheable.
corExpires :: Lens' CopyObject (Maybe UTCTime)
corExpires = lens _corExpires (\s a -> s { _corExpires = a })
    . mapping _Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
corGrantFullControl :: Lens' CopyObject (Maybe Text)
corGrantFullControl =
    lens _corGrantFullControl (\s a -> s { _corGrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
corGrantRead :: Lens' CopyObject (Maybe Text)
corGrantRead = lens _corGrantRead (\s a -> s { _corGrantRead = a })

-- | Allows grantee to read the object ACL.
corGrantReadACP :: Lens' CopyObject (Maybe Text)
corGrantReadACP = lens _corGrantReadACP (\s a -> s { _corGrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
corGrantWriteACP :: Lens' CopyObject (Maybe Text)
corGrantWriteACP = lens _corGrantWriteACP (\s a -> s { _corGrantWriteACP = a })

corKey :: Lens' CopyObject Text
corKey = lens _corKey (\s a -> s { _corKey = a })

-- | A map of metadata to store with the object in S3.
corMetadata :: Lens' CopyObject (HashMap Text Text)
corMetadata = lens _corMetadata (\s a -> s { _corMetadata = a })
    . _Map

-- | Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
corMetadataDirective :: Lens' CopyObject (Maybe Text)
corMetadataDirective =
    lens _corMetadataDirective (\s a -> s { _corMetadataDirective = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
corSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corSSECustomerAlgorithm =
    lens _corSSECustomerAlgorithm (\s a -> s { _corSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
corSSECustomerKey :: Lens' CopyObject (Maybe Text)
corSSECustomerKey =
    lens _corSSECustomerKey (\s a -> s { _corSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corSSECustomerKeyMD5 =
    lens _corSSECustomerKeyMD5 (\s a -> s { _corSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
corServerSideEncryption :: Lens' CopyObject (Maybe Text)
corServerSideEncryption =
    lens _corServerSideEncryption (\s a -> s { _corServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
corStorageClass :: Lens' CopyObject (Maybe Text)
corStorageClass = lens _corStorageClass (\s a -> s { _corStorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
corWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
corWebsiteRedirectLocation =
    lens _corWebsiteRedirectLocation
        (\s a -> s { _corWebsiteRedirectLocation = a })

instance ToPath CopyObject where
    toPath CopyObject{..} = mconcat
        [ "/"
        , toText _corBucket
        , "/"
        , toText _corKey
        ]

instance ToQuery CopyObject
    toQuery = const mempty
instance ToHeaders CopyObject where
    toHeaders CopyObject{..} = mconcat
        [ "x-amz-acl"                                                   =: _corACL
        , "Cache-Control"                                               =: _corCacheControl
        , "Content-Disposition"                                         =: _corContentDisposition
        , "Content-Encoding"                                            =: _corContentEncoding
        , "Content-Language"                                            =: _corContentLanguage
        , "Content-Type"                                                =: _corContentType
        , "x-amz-copy-source"                                           =: _corCopySource
        , "x-amz-copy-source-if-match"                                  =: _corCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since"                         =: _corCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match"                             =: _corCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since"                       =: _corCopySourceIfUnmodifiedSince
        , "Expires"                                                     =: _corExpires
        , "x-amz-grant-full-control"                                    =: _corGrantFullControl
        , "x-amz-grant-read"                                            =: _corGrantRead
        , "x-amz-grant-read-acp"                                        =: _corGrantReadACP
        , "x-amz-grant-write-acp"                                       =: _corGrantWriteACP
        , "x-amz-meta-"                                                 =: _corMetadata
        , "x-amz-metadata-directive"                                    =: _corMetadataDirective
        , "x-amz-server-side-encryption"                                =: _corServerSideEncryption
        , "x-amz-storage-class"                                         =: _corStorageClass
        , "x-amz-website-redirect-location"                             =: _corWebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm"             =: _corSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"                   =: _corSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"               =: _corSSECustomerKeyMD5
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _corCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key"       =: _corCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5"   =: _corCopySourceSSECustomerKeyMD5
        ]

instance ToBody CopyObject

data CopyObjectOutput = CopyObjectOutput
    { _cooCopyObjectResult     :: Maybe CopyObjectResult
    , _cooCopySourceVersionId  :: Maybe Text
    , _cooExpiration           :: Maybe RFC822
    , _cooSSECustomerAlgorithm :: Maybe Text
    , _cooSSECustomerKeyMD5    :: Maybe Text
    , _cooServerSideEncryption :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CopyObjectOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cooCopyObjectResult' @::@ 'Maybe' 'CopyObjectResult'
--
-- * 'cooCopySourceVersionId' @::@ 'Maybe' 'Text'
--
-- * 'cooExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'cooSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cooSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cooServerSideEncryption' @::@ 'Maybe' 'Text'
--
copyObjectOutput :: CopyObjectOutput
copyObjectOutput = CopyObjectOutput
    { _cooCopyObjectResult     = Nothing
    , _cooExpiration           = Nothing
    , _cooCopySourceVersionId  = Nothing
    , _cooServerSideEncryption = Nothing
    , _cooSSECustomerAlgorithm = Nothing
    , _cooSSECustomerKeyMD5    = Nothing
    }

cooCopyObjectResult :: Lens' CopyObjectOutput (Maybe CopyObjectResult)
cooCopyObjectResult =
    lens _cooCopyObjectResult (\s a -> s { _cooCopyObjectResult = a })

cooCopySourceVersionId :: Lens' CopyObjectOutput (Maybe Text)
cooCopySourceVersionId =
    lens _cooCopySourceVersionId (\s a -> s { _cooCopySourceVersionId = a })

-- | If the object expiration is configured, the response includes this
-- header.
cooExpiration :: Lens' CopyObjectOutput (Maybe UTCTime)
cooExpiration = lens _cooExpiration (\s a -> s { _cooExpiration = a })
    . mapping _Time

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
cooSSECustomerAlgorithm :: Lens' CopyObjectOutput (Maybe Text)
cooSSECustomerAlgorithm =
    lens _cooSSECustomerAlgorithm (\s a -> s { _cooSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cooSSECustomerKeyMD5 :: Lens' CopyObjectOutput (Maybe Text)
cooSSECustomerKeyMD5 =
    lens _cooSSECustomerKeyMD5 (\s a -> s { _cooSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cooServerSideEncryption :: Lens' CopyObjectOutput (Maybe Text)
cooServerSideEncryption =
    lens _cooServerSideEncryption (\s a -> s { _cooServerSideEncryption = a })

instance AWSRequest CopyObject where
    type Sv CopyObject = S3
    type Rs CopyObject = CopyObjectOutput

    request  = put'
    response = const . xmlResponse $ \h x -> CopyObjectOutput
        <$> x %| "CopyObjectResult"
        <*> h ~:? "x-amz-copy-source-version-id"
        <*> h ~:? "x-amz-expiration"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"

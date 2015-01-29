{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Completes a multipart upload by assembling previously uploaded parts.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CompleteMultipartUpload.html>
module Network.AWS.S3.CompleteMultipartUpload
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , completeMultipartUpload
    -- ** Request lenses
    , cmu1Bucket
    , cmu1Key
    , cmu1MultipartUpload
    , cmu1UploadId

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response constructor
    , completeMultipartUploadResponse
    -- ** Response lenses
    , cmur1Bucket
    , cmur1ETag
    , cmur1Expiration
    , cmur1Key
    , cmur1Location
    , cmur1SSEKMSKeyId
    , cmur1ServerSideEncryption
    , cmur1VersionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmu1Bucket          :: Text
    , _cmu1Key             :: Text
    , _cmu1MultipartUpload :: Maybe CompletedMultipartUpload
    , _cmu1UploadId        :: Text
    } deriving (Eq, Read, Show)

-- | 'CompleteMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmu1Bucket' @::@ 'Text'
--
-- * 'cmu1Key' @::@ 'Text'
--
-- * 'cmu1MultipartUpload' @::@ 'Maybe' 'CompletedMultipartUpload'
--
-- * 'cmu1UploadId' @::@ 'Text'
--
completeMultipartUpload :: Text -- ^ 'cmu1Bucket'
                        -> Text -- ^ 'cmu1Key'
                        -> Text -- ^ 'cmu1UploadId'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmu1Bucket          = p1
    , _cmu1Key             = p2
    , _cmu1UploadId        = p3
    , _cmu1MultipartUpload = Nothing
    }

cmu1Bucket :: Lens' CompleteMultipartUpload Text
cmu1Bucket = lens _cmu1Bucket (\s a -> s { _cmu1Bucket = a })

cmu1Key :: Lens' CompleteMultipartUpload Text
cmu1Key = lens _cmu1Key (\s a -> s { _cmu1Key = a })

cmu1MultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmu1MultipartUpload =
    lens _cmu1MultipartUpload (\s a -> s { _cmu1MultipartUpload = a })

cmu1UploadId :: Lens' CompleteMultipartUpload Text
cmu1UploadId = lens _cmu1UploadId (\s a -> s { _cmu1UploadId = a })

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmur1Bucket               :: Maybe Text
    , _cmur1ETag                 :: Maybe Text
    , _cmur1Expiration           :: Maybe Text
    , _cmur1Key                  :: Maybe Text
    , _cmur1Location             :: Maybe Text
    , _cmur1SSEKMSKeyId          :: Maybe (Sensitive Text)
    , _cmur1ServerSideEncryption :: Maybe ServerSideEncryption
    , _cmur1VersionId            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CompleteMultipartUploadResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmur1Bucket' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ETag' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Expiration' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Key' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Location' @::@ 'Maybe' 'Text'
--
-- * 'cmur1SSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ServerSideEncryption' @::@ 'Maybe' 'ServerSideEncryption'
--
-- * 'cmur1VersionId' @::@ 'Maybe' 'Text'
--
completeMultipartUploadResponse :: CompleteMultipartUploadResponse
completeMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmur1Location             = Nothing
    , _cmur1Bucket               = Nothing
    , _cmur1Key                  = Nothing
    , _cmur1Expiration           = Nothing
    , _cmur1ETag                 = Nothing
    , _cmur1ServerSideEncryption = Nothing
    , _cmur1VersionId            = Nothing
    , _cmur1SSEKMSKeyId          = Nothing
    }

cmur1Bucket :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1Bucket = lens _cmur1Bucket (\s a -> s { _cmur1Bucket = a })

-- | Entity tag of the object.
cmur1ETag :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1ETag = lens _cmur1ETag (\s a -> s { _cmur1ETag = a })

-- | If the object expiration is configured, this will contain the expiration date
-- (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
cmur1Expiration :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1Expiration = lens _cmur1Expiration (\s a -> s { _cmur1Expiration = a })

cmur1Key :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1Key = lens _cmur1Key (\s a -> s { _cmur1Key = a })

cmur1Location :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1Location = lens _cmur1Location (\s a -> s { _cmur1Location = a })

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master
-- encryption key that was used for the object.
cmur1SSEKMSKeyId :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1SSEKMSKeyId = lens _cmur1SSEKMSKeyId (\s a -> s { _cmur1SSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmur1ServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cmur1ServerSideEncryption =
    lens _cmur1ServerSideEncryption
        (\s a -> s { _cmur1ServerSideEncryption = a })

-- | Version of the object.
cmur1VersionId :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmur1VersionId = lens _cmur1VersionId (\s a -> s { _cmur1VersionId = a })

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmu1Bucket
        , "/"
        , toText _cmu1Key
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery rq = "uploadId" =? _cmu1UploadId rq

instance ToHeaders CompleteMultipartUpload

instance ToXMLRoot CompleteMultipartUpload where
    toXMLRoot CompleteMultipartUpload{..} = namespaced ns "CompleteMultipartUpload"
        [ "CompleteMultipartUpload" =@ _cmu1MultipartUpload
        ]

instance ToXML CompleteMultipartUpload

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CompleteMultipartUploadResponse
        <$> x .@? "Bucket"
        <*> x .@? "ETag"
        <*> h ~:? "x-amz-expiration"
        <*> x .@? "Key"
        <*> x .@? "Location"
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"

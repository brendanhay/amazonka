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

-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation initiates a multipart upload. Amazon Glacier creates a
-- multipart upload resource and returns its ID in the response. The multipart
-- upload ID is used in subsequent requests to upload parts of an archive (see 'UploadMultipartPart').
--
-- When you initiate a multipart upload, you specify the part size in number of
-- bytes. The part size must be a megabyte (1024 KB) multiplied by a power of
-- 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4
-- GB.
--
-- Every part you upload to this resource (see 'UploadMultipartPart'), except the
-- last one, must have the same size. The last one can be the same size or
-- smaller. For example, suppose you want to upload a 16.2 MB file. If you
-- initiate the multipart upload with a part size of 4 MB, you will upload four
-- parts of 4 MB each and one part of 0.2 MB.
--
-- You don't need to know the size of the archive when you start a multipart
-- upload because Amazon Glacier does not require you to specify the overall
-- archive size.
--
-- After you complete the multipart upload, Amazon Glacier removes the
-- multipart upload resource referenced by the ID. Amazon Glacier also removes
-- the multipart upload resource if you cancel the multipart upload or it may be
-- removed if there is no activity for a period of 24 hours.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading LargeArchives in Parts (Multipart Upload)> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload> in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-InitiateMultipartUpload.html>
module Network.AWS.Glacier.InitiateMultipartUpload
    (
    -- * Request
      InitiateMultipartUpload
    -- ** Request constructor
    , initiateMultipartUpload
    -- ** Request lenses
    , imuAccountId
    , imuArchiveDescription
    , imuPartSize
    , imuVaultName

    -- * Response
    , InitiateMultipartUploadResponse
    -- ** Response constructor
    , initiateMultipartUploadResponse
    -- ** Response lenses
    , imurLocation
    , imurUploadId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data InitiateMultipartUpload = InitiateMultipartUpload
    { _imuAccountId          :: Text
    , _imuArchiveDescription :: Maybe Text
    , _imuPartSize           :: Maybe Text
    , _imuVaultName          :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InitiateMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imuAccountId' @::@ 'Text'
--
-- * 'imuArchiveDescription' @::@ 'Maybe' 'Text'
--
-- * 'imuPartSize' @::@ 'Maybe' 'Text'
--
-- * 'imuVaultName' @::@ 'Text'
--
initiateMultipartUpload :: Text -- ^ 'imuAccountId'
                        -> Text -- ^ 'imuVaultName'
                        -> InitiateMultipartUpload
initiateMultipartUpload p1 p2 = InitiateMultipartUpload
    { _imuAccountId          = p1
    , _imuVaultName          = p2
    , _imuArchiveDescription = Nothing
    , _imuPartSize           = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
imuAccountId :: Lens' InitiateMultipartUpload Text
imuAccountId = lens _imuAccountId (\s a -> s { _imuAccountId = a })

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2—for
-- example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and
-- so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB (4096
-- MB).
imuArchiveDescription :: Lens' InitiateMultipartUpload (Maybe Text)
imuArchiveDescription =
    lens _imuArchiveDescription (\s a -> s { _imuArchiveDescription = a })

-- | The size of each part except the last, in bytes. The last part can be smaller
-- than this part size.
imuPartSize :: Lens' InitiateMultipartUpload (Maybe Text)
imuPartSize = lens _imuPartSize (\s a -> s { _imuPartSize = a })

-- | The name of the vault.
imuVaultName :: Lens' InitiateMultipartUpload Text
imuVaultName = lens _imuVaultName (\s a -> s { _imuVaultName = a })

data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse
    { _imurLocation :: Maybe Text
    , _imurUploadId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InitiateMultipartUploadResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imurLocation' @::@ 'Maybe' 'Text'
--
-- * 'imurUploadId' @::@ 'Maybe' 'Text'
--
initiateMultipartUploadResponse :: InitiateMultipartUploadResponse
initiateMultipartUploadResponse = InitiateMultipartUploadResponse
    { _imurLocation = Nothing
    , _imurUploadId = Nothing
    }

-- | The relative URI path of the multipart upload ID Amazon Glacier created.
imurLocation :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imurLocation = lens _imurLocation (\s a -> s { _imurLocation = a })

-- | The ID of the multipart upload. This value is also included as part of the
-- location.
imurUploadId :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imurUploadId = lens _imurUploadId (\s a -> s { _imurUploadId = a })

instance ToPath InitiateMultipartUpload where
    toPath InitiateMultipartUpload{..} = mconcat
        [ "/"
        , toText _imuAccountId
        , "/vaults/"
        , toText _imuVaultName
        , "/multipart-uploads"
        ]

instance ToQuery InitiateMultipartUpload where
    toQuery = const mempty

instance ToHeaders InitiateMultipartUpload where
    toHeaders InitiateMultipartUpload{..} = mconcat
        [ "x-amz-archive-description" =: _imuArchiveDescription
        , "x-amz-part-size"           =: _imuPartSize
        ]

instance ToJSON InitiateMultipartUpload where
    toJSON = const (toJSON Empty)

instance AWSRequest InitiateMultipartUpload where
    type Sv InitiateMultipartUpload = Glacier
    type Rs InitiateMultipartUpload = InitiateMultipartUploadResponse

    request  = post
    response = headerResponse $ \h -> InitiateMultipartUploadResponse
        <$> h ~:? "Location"
        <*> h ~:? "x-amz-multipart-upload-id"

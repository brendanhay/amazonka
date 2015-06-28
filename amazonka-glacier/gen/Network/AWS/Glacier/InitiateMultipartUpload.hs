{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
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

-- | This operation initiates a multipart upload. Amazon Glacier creates a
-- multipart upload resource and returns its ID in the response. The
-- multipart upload ID is used in subsequent requests to upload parts of an
-- archive (see UploadMultipartPart).
--
-- When you initiate a multipart upload, you specify the part size in
-- number of bytes. The part size must be a megabyte (1024 KB) multiplied
-- by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4
-- MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB,
-- and the maximum is 4 GB.
--
-- Every part you upload to this resource (see UploadMultipartPart), except
-- the last one, must have the same size. The last one can be the same size
-- or smaller. For example, suppose you want to upload a 16.2 MB file. If
-- you initiate the multipart upload with a part size of 4 MB, you will
-- upload four parts of 4 MB each and one part of 0.2 MB.
--
-- You don\'t need to know the size of the archive when you start a
-- multipart upload because Amazon Glacier does not require you to specify
-- the overall archive size.
--
-- After you complete the multipart upload, Amazon Glacier removes the
-- multipart upload resource referenced by the ID. Amazon Glacier also
-- removes the multipart upload resource if you cancel the multipart upload
-- or it may be removed if there is no activity for a period of 24 hours.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-InitiateMultipartUpload.html>
module Network.AWS.Glacier.InitiateMultipartUpload
    (
    -- * Request
      InitiateMultipartUpload
    -- ** Request constructor
    , initiateMultipartUpload
    -- ** Request lenses
    , imuPartSize
    , imuArchiveDescription
    , imuAccountId
    , imuVaultName

    -- * Response
    , InitiateMultipartUploadResponse
    -- ** Response constructor
    , initiateMultipartUploadResponse
    -- ** Response lenses
    , imurLocation
    , imurUploadId
    , imurStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for initiating a multipart upload to an Amazon Glacier
-- vault.
--
-- /See:/ 'initiateMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imuPartSize'
--
-- * 'imuArchiveDescription'
--
-- * 'imuAccountId'
--
-- * 'imuVaultName'
data InitiateMultipartUpload = InitiateMultipartUpload'
    { _imuPartSize           :: !(Maybe Text)
    , _imuArchiveDescription :: !(Maybe Text)
    , _imuAccountId          :: !Text
    , _imuVaultName          :: !Text
    } deriving (Eq,Read,Show)

-- | 'InitiateMultipartUpload' smart constructor.
initiateMultipartUpload :: Text -> Text -> InitiateMultipartUpload
initiateMultipartUpload pAccountId pVaultName =
    InitiateMultipartUpload'
    { _imuPartSize = Nothing
    , _imuArchiveDescription = Nothing
    , _imuAccountId = pAccountId
    , _imuVaultName = pVaultName
    }

-- | The size of each part except the last, in bytes. The last part can be
-- smaller than this part size.
imuPartSize :: Lens' InitiateMultipartUpload (Maybe Text)
imuPartSize = lens _imuPartSize (\ s a -> s{_imuPartSize = a});

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
-- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
-- is 4 GB (4096 MB).
imuArchiveDescription :: Lens' InitiateMultipartUpload (Maybe Text)
imuArchiveDescription = lens _imuArchiveDescription (\ s a -> s{_imuArchiveDescription = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
imuAccountId :: Lens' InitiateMultipartUpload Text
imuAccountId = lens _imuAccountId (\ s a -> s{_imuAccountId = a});

-- | The name of the vault.
imuVaultName :: Lens' InitiateMultipartUpload Text
imuVaultName = lens _imuVaultName (\ s a -> s{_imuVaultName = a});

instance AWSRequest InitiateMultipartUpload where
        type Sv InitiateMultipartUpload = Glacier
        type Rs InitiateMultipartUpload =
             InitiateMultipartUploadResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 InitiateMultipartUploadResponse' <$>
                   (h .#? "Location") <*>
                     (h .#? "x-amz-multipart-upload-id")
                     <*> (pure s))

instance ToHeaders InitiateMultipartUpload where
        toHeaders InitiateMultipartUpload'{..}
          = mconcat
              ["x-amz-part-size" =# _imuPartSize,
               "x-amz-archive-description" =#
                 _imuArchiveDescription]

instance ToJSON InitiateMultipartUpload where
        toJSON = const (Object mempty)

instance ToPath InitiateMultipartUpload where
        toPath InitiateMultipartUpload'{..}
          = mconcat
              ["/", toText _imuAccountId, "/vaults/",
               toText _imuVaultName, "/multipart-uploads"]

instance ToQuery InitiateMultipartUpload where
        toQuery = const mempty

-- | The Amazon Glacier response to your request.
--
-- /See:/ 'initiateMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imurLocation'
--
-- * 'imurUploadId'
--
-- * 'imurStatus'
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
    { _imurLocation :: !(Maybe Text)
    , _imurUploadId :: !(Maybe Text)
    , _imurStatus   :: !Status
    } deriving (Eq,Show)

-- | 'InitiateMultipartUploadResponse' smart constructor.
initiateMultipartUploadResponse :: Status -> InitiateMultipartUploadResponse
initiateMultipartUploadResponse pStatus =
    InitiateMultipartUploadResponse'
    { _imurLocation = Nothing
    , _imurUploadId = Nothing
    , _imurStatus = pStatus
    }

-- | The relative URI path of the multipart upload ID Amazon Glacier created.
imurLocation :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imurLocation = lens _imurLocation (\ s a -> s{_imurLocation = a});

-- | The ID of the multipart upload. This value is also included as part of
-- the location.
imurUploadId :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imurUploadId = lens _imurUploadId (\ s a -> s{_imurUploadId = a});

-- | FIXME: Undocumented member.
imurStatus :: Lens' InitiateMultipartUploadResponse Status
imurStatus = lens _imurStatus (\ s a -> s{_imurStatus = a});

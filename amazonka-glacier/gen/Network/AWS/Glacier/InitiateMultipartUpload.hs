{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload. Amazon Glacier creates a
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
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-InitiateMultipartUpload.html AWS API Reference> for InitiateMultipartUpload.
module Network.AWS.Glacier.InitiateMultipartUpload
    (
    -- * Creating a Request
      initiateMultipartUpload
    , InitiateMultipartUpload
    -- * Request Lenses
    , imuPartSize
    , imuArchiveDescription
    , imuAccountId
    , imuVaultName

    -- * Destructuring the Response
    , initiateMultipartUploadResponse
    , InitiateMultipartUploadResponse
    -- * Response Lenses
    , imursLocation
    , imursUploadId
    , imursResponseStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for initiating a multipart upload to an Amazon Glacier
-- vault.
--
-- /See:/ 'initiateMultipartUpload' smart constructor.
data InitiateMultipartUpload = InitiateMultipartUpload'
    { _imuPartSize           :: !(Maybe Text)
    , _imuArchiveDescription :: !(Maybe Text)
    , _imuAccountId          :: !Text
    , _imuVaultName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InitiateMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imuPartSize'
--
-- * 'imuArchiveDescription'
--
-- * 'imuAccountId'
--
-- * 'imuVaultName'
initiateMultipartUpload
    :: Text -- ^ 'imuAccountId'
    -> Text -- ^ 'imuVaultName'
    -> InitiateMultipartUpload
initiateMultipartUpload pAccountId_ pVaultName_ =
    InitiateMultipartUpload'
    { _imuPartSize = Nothing
    , _imuArchiveDescription = Nothing
    , _imuAccountId = pAccountId_
    , _imuVaultName = pVaultName_
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

-- | The 'AccountId' value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos'-'apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
imuAccountId :: Lens' InitiateMultipartUpload Text
imuAccountId = lens _imuAccountId (\ s a -> s{_imuAccountId = a});

-- | The name of the vault.
imuVaultName :: Lens' InitiateMultipartUpload Text
imuVaultName = lens _imuVaultName (\ s a -> s{_imuVaultName = a});

instance AWSRequest InitiateMultipartUpload where
        type Rs InitiateMultipartUpload =
             InitiateMultipartUploadResponse
        request = postJSON glacier
        response
          = receiveEmpty
              (\ s h x ->
                 InitiateMultipartUploadResponse' <$>
                   (h .#? "Location") <*>
                     (h .#? "x-amz-multipart-upload-id")
                     <*> (pure (fromEnum s)))

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
              ["/", toBS _imuAccountId, "/vaults/",
               toBS _imuVaultName, "/multipart-uploads"]

instance ToQuery InitiateMultipartUpload where
        toQuery = const mempty

-- | The Amazon Glacier response to your request.
--
-- /See:/ 'initiateMultipartUploadResponse' smart constructor.
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
    { _imursLocation       :: !(Maybe Text)
    , _imursUploadId       :: !(Maybe Text)
    , _imursResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InitiateMultipartUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imursLocation'
--
-- * 'imursUploadId'
--
-- * 'imursResponseStatus'
initiateMultipartUploadResponse
    :: Int -- ^ 'imursResponseStatus'
    -> InitiateMultipartUploadResponse
initiateMultipartUploadResponse pResponseStatus_ =
    InitiateMultipartUploadResponse'
    { _imursLocation = Nothing
    , _imursUploadId = Nothing
    , _imursResponseStatus = pResponseStatus_
    }

-- | The relative URI path of the multipart upload ID Amazon Glacier created.
imursLocation :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imursLocation = lens _imursLocation (\ s a -> s{_imursLocation = a});

-- | The ID of the multipart upload. This value is also included as part of
-- the location.
imursUploadId :: Lens' InitiateMultipartUploadResponse (Maybe Text)
imursUploadId = lens _imursUploadId (\ s a -> s{_imursUploadId = a});

-- | The response status code.
imursResponseStatus :: Lens' InitiateMultipartUploadResponse Int
imursResponseStatus = lens _imursResponseStatus (\ s a -> s{_imursResponseStatus = a});

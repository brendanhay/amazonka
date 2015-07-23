{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload identified by the upload ID.
--
-- After the Abort Multipart Upload request succeeds, you cannot upload any
-- more parts to the multipart upload or complete the multipart upload.
-- Aborting a completed upload fails. However, aborting an already-aborted
-- upload will succeed, for a short time. For more information about
-- uploading a part and completing a multipart upload, see
-- UploadMultipartPart and CompleteMultipartUpload.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-AbortMultipartUpload.html>
module Network.AWS.Glacier.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amurqAccountId
    , amurqVaultName
    , amurqUploadId

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , abortMultipartUploadResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to abort a multipart upload identified by the upload
-- ID.
--
-- For information about the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload>.
-- For conceptual information, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon Glacier>.
--
-- /See:/ 'abortMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amurqAccountId'
--
-- * 'amurqVaultName'
--
-- * 'amurqUploadId'
data AbortMultipartUpload = AbortMultipartUpload'
    { _amurqAccountId :: !Text
    , _amurqVaultName :: !Text
    , _amurqUploadId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUpload' smart constructor.
abortMultipartUpload :: Text -> Text -> Text -> AbortMultipartUpload
abortMultipartUpload pAccountId_ pVaultName_ pUploadId_ =
    AbortMultipartUpload'
    { _amurqAccountId = pAccountId_
    , _amurqVaultName = pVaultName_
    , _amurqUploadId = pUploadId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
amurqAccountId :: Lens' AbortMultipartUpload Text
amurqAccountId = lens _amurqAccountId (\ s a -> s{_amurqAccountId = a});

-- | The name of the vault.
amurqVaultName :: Lens' AbortMultipartUpload Text
amurqVaultName = lens _amurqVaultName (\ s a -> s{_amurqVaultName = a});

-- | The upload ID of the multipart upload to delete.
amurqUploadId :: Lens' AbortMultipartUpload Text
amurqUploadId = lens _amurqUploadId (\ s a -> s{_amurqUploadId = a});

instance AWSRequest AbortMultipartUpload where
        type Sv AbortMultipartUpload = Glacier
        type Rs AbortMultipartUpload =
             AbortMultipartUploadResponse
        request = delete
        response = receiveNull AbortMultipartUploadResponse'

instance ToHeaders AbortMultipartUpload where
        toHeaders = const mempty

instance ToPath AbortMultipartUpload where
        toPath AbortMultipartUpload'{..}
          = mconcat
              ["/", toText _amurqAccountId, "/vaults/",
               toText _amurqVaultName, "/multipart-uploads/",
               toText _amurqUploadId]

instance ToQuery AbortMultipartUpload where
        toQuery = const mempty

-- | /See:/ 'abortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse =
    AbortMultipartUploadResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUploadResponse' smart constructor.
abortMultipartUploadResponse :: AbortMultipartUploadResponse
abortMultipartUploadResponse = AbortMultipartUploadResponse'

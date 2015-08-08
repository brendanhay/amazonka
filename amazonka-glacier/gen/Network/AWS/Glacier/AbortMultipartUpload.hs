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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-AbortMultipartUpload.html AWS API Reference> for AbortMultipartUpload.
module Network.AWS.Glacier.AbortMultipartUpload
    (
    -- * Creating a Request
      AbortMultipartUpload
    , abortMultipartUpload
    -- * Request Lenses
    , amuAccountId
    , amuVaultName
    , amuUploadId

    -- * Destructuring the Response
    , AbortMultipartUploadResponse
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
-- * 'amuAccountId'
--
-- * 'amuVaultName'
--
-- * 'amuUploadId'
data AbortMultipartUpload = AbortMultipartUpload'
    { _amuAccountId :: !Text
    , _amuVaultName :: !Text
    , _amuUploadId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUpload' smart constructor.
abortMultipartUpload :: Text -> Text -> Text -> AbortMultipartUpload
abortMultipartUpload pAccountId_ pVaultName_ pUploadId_ =
    AbortMultipartUpload'
    { _amuAccountId = pAccountId_
    , _amuVaultName = pVaultName_
    , _amuUploadId = pUploadId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
amuAccountId :: Lens' AbortMultipartUpload Text
amuAccountId = lens _amuAccountId (\ s a -> s{_amuAccountId = a});

-- | The name of the vault.
amuVaultName :: Lens' AbortMultipartUpload Text
amuVaultName = lens _amuVaultName (\ s a -> s{_amuVaultName = a});

-- | The upload ID of the multipart upload to delete.
amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\ s a -> s{_amuUploadId = a});

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
              ["/", toBS _amuAccountId, "/vaults/",
               toBS _amuVaultName, "/multipart-uploads/",
               toBS _amuUploadId]

instance ToQuery AbortMultipartUpload where
        toQuery = const mempty

-- | /See:/ 'abortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse =
    AbortMultipartUploadResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUploadResponse' smart constructor.
abortMultipartUploadResponse :: AbortMultipartUploadResponse
abortMultipartUploadResponse = AbortMultipartUploadResponse'

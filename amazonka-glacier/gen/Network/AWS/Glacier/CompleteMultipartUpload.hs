{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CompleteMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- You call this operation to inform Amazon Glacier that all the archive
-- parts have been uploaded and that Amazon Glacier can now assemble the
-- archive from the uploaded parts. After assembling and saving the archive
-- to the vault, Amazon Glacier returns the URI path of the newly created
-- archive resource. Using the URI path, you can then access the archive.
-- After you upload an archive, you should save the archive ID returned to
-- retrieve the archive at a later point. You can also get the vault
-- inventory to obtain a list of archive IDs in a vault. For more
-- information, see InitiateJob.
--
-- In the request, you must include the computed SHA256 tree hash of the
-- entire archive you have uploaded. For information about computing a
-- SHA256 tree hash, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
-- On the server side, Amazon Glacier also constructs the SHA256 tree hash
-- of the assembled archive. If the values match, Amazon Glacier saves the
-- archive to the vault; otherwise, it returns an error, and the operation
-- fails. The ListParts operation returns a list of parts uploaded for a
-- specific multipart upload. It includes checksum information for each
-- uploaded part that can be used to debug a bad checksum issue.
--
-- Additionally, Amazon Glacier also checks for any missing content ranges
-- when assembling the archive, if missing content ranges are found, Amazon
-- Glacier returns an error and the operation fails.
--
-- Complete Multipart Upload is an idempotent operation. After your first
-- successful complete multipart upload, if you call the operation again
-- within a short period, the operation will succeed and return the same
-- archive ID. This is useful in the event you experience a network issue
-- that causes an aborted connection or receive a 500 server error, in
-- which case you can repeat your Complete Multipart Upload request and get
-- the same archive ID without creating duplicate archives. Note, however,
-- that after the multipart upload completes, you cannot call the List
-- Parts operation and the multipart upload will not appear in List
-- Multipart Uploads response, even if idempotent complete is possible.
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
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-complete-upload.html Complete Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-CompleteMultipartUpload.html>
module Network.AWS.Glacier.CompleteMultipartUpload
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , completeMultipartUpload
    -- ** Request lenses
    , cmuChecksum
    , cmuArchiveSize
    , cmuAccountId
    , cmuVaultName
    , cmuUploadId

    -- * Response
    , ArchiveCreationOutput
    -- ** Response constructor
    , archiveCreationOutput
    -- ** Response lenses
    , acoArchiveId
    , acoChecksum
    , acoLocation
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to complete a multipart upload operation. This informs
-- Amazon Glacier that all the archive parts have been uploaded and Amazon
-- Glacier can now assemble the archive from the uploaded parts. After
-- assembling and saving the archive to the vault, Amazon Glacier returns
-- the URI path of the newly created archive resource.
--
-- /See:/ 'completeMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuChecksum'
--
-- * 'cmuArchiveSize'
--
-- * 'cmuAccountId'
--
-- * 'cmuVaultName'
--
-- * 'cmuUploadId'
data CompleteMultipartUpload = CompleteMultipartUpload'
    { _cmuChecksum    :: !(Maybe Text)
    , _cmuArchiveSize :: !(Maybe Text)
    , _cmuAccountId   :: !Text
    , _cmuVaultName   :: !Text
    , _cmuUploadId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteMultipartUpload' smart constructor.
completeMultipartUpload :: Text -> Text -> Text -> CompleteMultipartUpload
completeMultipartUpload pAccountId_ pVaultName_ pUploadId_ =
    CompleteMultipartUpload'
    { _cmuChecksum = Nothing
    , _cmuArchiveSize = Nothing
    , _cmuAccountId = pAccountId_
    , _cmuVaultName = pVaultName_
    , _cmuUploadId = pUploadId_
    }

-- | The SHA256 tree hash of the entire archive. It is the tree hash of
-- SHA256 tree hash of the individual parts. If the value you specify in
-- the request does not match the SHA256 tree hash of the final assembled
-- archive as computed by Amazon Glacier, Amazon Glacier returns an error
-- and the request fails.
cmuChecksum :: Lens' CompleteMultipartUpload (Maybe Text)
cmuChecksum = lens _cmuChecksum (\ s a -> s{_cmuChecksum = a});

-- | The total size, in bytes, of the entire archive. This value should be
-- the sum of all the sizes of the individual parts that you uploaded.
cmuArchiveSize :: Lens' CompleteMultipartUpload (Maybe Text)
cmuArchiveSize = lens _cmuArchiveSize (\ s a -> s{_cmuArchiveSize = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
cmuAccountId :: Lens' CompleteMultipartUpload Text
cmuAccountId = lens _cmuAccountId (\ s a -> s{_cmuAccountId = a});

-- | The name of the vault.
cmuVaultName :: Lens' CompleteMultipartUpload Text
cmuVaultName = lens _cmuVaultName (\ s a -> s{_cmuVaultName = a});

-- | The upload ID of the multipart upload.
cmuUploadId :: Lens' CompleteMultipartUpload Text
cmuUploadId = lens _cmuUploadId (\ s a -> s{_cmuUploadId = a});

instance AWSRequest CompleteMultipartUpload where
        type Sv CompleteMultipartUpload = Glacier
        type Rs CompleteMultipartUpload =
             ArchiveCreationOutput
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CompleteMultipartUpload where
        toHeaders CompleteMultipartUpload'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _cmuChecksum,
               "x-amz-archive-size" =# _cmuArchiveSize]

instance ToJSON CompleteMultipartUpload where
        toJSON = const (Object mempty)

instance ToPath CompleteMultipartUpload where
        toPath CompleteMultipartUpload'{..}
          = mconcat
              ["/", toBS _cmuAccountId, "/vaults/",
               toBS _cmuVaultName, "/multipart-uploads/",
               toBS _cmuUploadId]

instance ToQuery CompleteMultipartUpload where
        toQuery = const mempty

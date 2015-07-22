{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.UploadMultipartPart
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation uploads a part of an archive. You can upload archive
-- parts in any order. You can also upload them in parallel. You can upload
-- up to 10,000 parts for a multipart upload.
--
-- Amazon Glacier rejects your upload part request if any of the following
-- conditions is true:
--
-- -   __SHA256 tree hash does not match__To ensure that part data is not
--     corrupted in transmission, you compute a SHA256 tree hash of the
--     part and include it in your request. Upon receiving the part data,
--     Amazon Glacier also computes a SHA256 tree hash. If these hash
--     values don\'t match, the operation fails. For information about
--     computing a SHA256 tree hash, see
--     <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- -   __Part size does not match__The size of each part except the last
--     must match the size specified in the corresponding
--     InitiateMultipartUpload request. The size of the last part must be
--     the same size as, or smaller than, the specified size.
--
--     If you upload a part whose size is smaller than the part size you
--     specified in your initiate multipart upload request and that part is
--     not the last part, then the upload part request will succeed.
--     However, the subsequent Complete Multipart Upload request will fail.
--
-- -   __Range does not align__The byte range value in the request does not
--     align with the part size specified in the corresponding initiate
--     request. For example, if you specify a part size of 4194304 bytes (4
--     MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to
--     8388607 (8 MB - 1) are valid part ranges. However, if you set a
--     range value of 2 MB to 6 MB, the range does not align with the part
--     size and the upload will fail.
--
-- This operation is idempotent. If you upload the same part multiple
-- times, the data included in the most recent request overwrites the
-- previously uploaded data.
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
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html Upload Part>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-UploadMultipartPart.html>
module Network.AWS.Glacier.UploadMultipartPart
    (
    -- * Request
      UploadMultipartPart
    -- ** Request constructor
    , uploadMultipartPart
    -- ** Request lenses
    , umprqChecksum
    , umprqRange
    , umprqAccountId
    , umprqVaultName
    , umprqUploadId
    , umprqBody

    -- * Response
    , UploadMultipartPartResponse
    -- ** Response constructor
    , uploadMultipartPartResponse
    -- ** Response lenses
    , umprsChecksum
    , umprsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to upload a part of an archive in a multipart upload
-- operation.
--
-- /See:/ 'uploadMultipartPart' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umprqChecksum'
--
-- * 'umprqRange'
--
-- * 'umprqAccountId'
--
-- * 'umprqVaultName'
--
-- * 'umprqUploadId'
--
-- * 'umprqBody'
data UploadMultipartPart = UploadMultipartPart'
    { _umprqChecksum  :: !(Maybe Text)
    , _umprqRange     :: !(Maybe Text)
    , _umprqAccountId :: !Text
    , _umprqVaultName :: !Text
    , _umprqUploadId  :: !Text
    , _umprqBody      :: !RqBody
    } deriving (Show,Generic)

-- | 'UploadMultipartPart' smart constructor.
uploadMultipartPart :: Text -> Text -> Text -> RqBody -> UploadMultipartPart
uploadMultipartPart pAccountId_ pVaultName_ pUploadId_ pBody_ =
    UploadMultipartPart'
    { _umprqChecksum = Nothing
    , _umprqRange = Nothing
    , _umprqAccountId = pAccountId_
    , _umprqVaultName = pVaultName_
    , _umprqUploadId = pUploadId_
    , _umprqBody = pBody_
    }

-- | The SHA256 tree hash of the data being uploaded.
umprqChecksum :: Lens' UploadMultipartPart (Maybe Text)
umprqChecksum = lens _umprqChecksum (\ s a -> s{_umprqChecksum = a});

-- | Identifies the range of bytes in the assembled archive that will be
-- uploaded in this part. Amazon Glacier uses this information to assemble
-- the archive in the proper sequence. The format of this header follows
-- RFC 2616. An example header is Content-Range:bytes 0-4194303\/*.
umprqRange :: Lens' UploadMultipartPart (Maybe Text)
umprqRange = lens _umprqRange (\ s a -> s{_umprqRange = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
umprqAccountId :: Lens' UploadMultipartPart Text
umprqAccountId = lens _umprqAccountId (\ s a -> s{_umprqAccountId = a});

-- | The name of the vault.
umprqVaultName :: Lens' UploadMultipartPart Text
umprqVaultName = lens _umprqVaultName (\ s a -> s{_umprqVaultName = a});

-- | The upload ID of the multipart upload.
umprqUploadId :: Lens' UploadMultipartPart Text
umprqUploadId = lens _umprqUploadId (\ s a -> s{_umprqUploadId = a});

-- | The data to upload.
umprqBody :: Lens' UploadMultipartPart RqBody
umprqBody = lens _umprqBody (\ s a -> s{_umprqBody = a});

instance AWSRequest UploadMultipartPart where
        type Sv UploadMultipartPart = Glacier
        type Rs UploadMultipartPart =
             UploadMultipartPartResponse
        request = putBody
        response
          = receiveJSON
              (\ s h x ->
                 UploadMultipartPartResponse' <$>
                   (h .#? "x-amz-sha256-tree-hash") <*>
                     (pure (fromEnum s)))

instance ToBody UploadMultipartPart where
        toBody = _umprqBody

instance ToHeaders UploadMultipartPart where
        toHeaders UploadMultipartPart'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _umprqChecksum,
               "Content-Range" =# _umprqRange]

instance ToPath UploadMultipartPart where
        toPath UploadMultipartPart'{..}
          = mconcat
              ["/", toText _umprqAccountId, "/vaults/",
               toText _umprqVaultName, "/multipart-uploads/",
               toText _umprqUploadId]

instance ToQuery UploadMultipartPart where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'uploadMultipartPartResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umprsChecksum'
--
-- * 'umprsStatus'
data UploadMultipartPartResponse = UploadMultipartPartResponse'
    { _umprsChecksum :: !(Maybe Text)
    , _umprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadMultipartPartResponse' smart constructor.
uploadMultipartPartResponse :: Int -> UploadMultipartPartResponse
uploadMultipartPartResponse pStatus_ =
    UploadMultipartPartResponse'
    { _umprsChecksum = Nothing
    , _umprsStatus = pStatus_
    }

-- | The SHA256 tree hash that Amazon Glacier computed for the uploaded part.
umprsChecksum :: Lens' UploadMultipartPartResponse (Maybe Text)
umprsChecksum = lens _umprsChecksum (\ s a -> s{_umprsChecksum = a});

-- | FIXME: Undocumented member.
umprsStatus :: Lens' UploadMultipartPartResponse Int
umprsStatus = lens _umprsStatus (\ s a -> s{_umprsStatus = a});

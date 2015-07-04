{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Glacier.UploadMultipartPart
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation uploads a part of an archive. You can upload archive
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
    , umpChecksum
    , umpRange
    , umpAccountId
    , umpVaultName
    , umpUploadId
    , umpBody

    -- * Response
    , UploadMultipartPartResponse
    -- ** Response constructor
    , uploadMultipartPartResponse
    -- ** Response lenses
    , umprChecksum
    , umprStatus
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
-- * 'umpChecksum'
--
-- * 'umpRange'
--
-- * 'umpAccountId'
--
-- * 'umpVaultName'
--
-- * 'umpUploadId'
--
-- * 'umpBody'
data UploadMultipartPart = UploadMultipartPart'
    { _umpChecksum  :: !(Maybe Text)
    , _umpRange     :: !(Maybe Text)
    , _umpAccountId :: !Text
    , _umpVaultName :: !Text
    , _umpUploadId  :: !Text
    , _umpBody      :: !RqBody
    } deriving (Show,Data,Typeable,Generic)

-- | 'UploadMultipartPart' smart constructor.
uploadMultipartPart :: Text -> Text -> Text -> RqBody -> UploadMultipartPart
uploadMultipartPart pAccountId pVaultName pUploadId pBody =
    UploadMultipartPart'
    { _umpChecksum = Nothing
    , _umpRange = Nothing
    , _umpAccountId = pAccountId
    , _umpVaultName = pVaultName
    , _umpUploadId = pUploadId
    , _umpBody = pBody
    }

-- | The SHA256 tree hash of the data being uploaded.
umpChecksum :: Lens' UploadMultipartPart (Maybe Text)
umpChecksum = lens _umpChecksum (\ s a -> s{_umpChecksum = a});

-- | Identifies the range of bytes in the assembled archive that will be
-- uploaded in this part. Amazon Glacier uses this information to assemble
-- the archive in the proper sequence. The format of this header follows
-- RFC 2616. An example header is Content-Range:bytes 0-4194303\/*.
umpRange :: Lens' UploadMultipartPart (Maybe Text)
umpRange = lens _umpRange (\ s a -> s{_umpRange = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
umpAccountId :: Lens' UploadMultipartPart Text
umpAccountId = lens _umpAccountId (\ s a -> s{_umpAccountId = a});

-- | The name of the vault.
umpVaultName :: Lens' UploadMultipartPart Text
umpVaultName = lens _umpVaultName (\ s a -> s{_umpVaultName = a});

-- | The upload ID of the multipart upload.
umpUploadId :: Lens' UploadMultipartPart Text
umpUploadId = lens _umpUploadId (\ s a -> s{_umpUploadId = a});

-- | The data to upload.
umpBody :: Lens' UploadMultipartPart RqBody
umpBody = lens _umpBody (\ s a -> s{_umpBody = a});

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
        toBody = _umpBody

instance ToHeaders UploadMultipartPart where
        toHeaders UploadMultipartPart'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _umpChecksum,
               "Content-Range" =# _umpRange]

instance ToPath UploadMultipartPart where
        toPath UploadMultipartPart'{..}
          = mconcat
              ["/", toText _umpAccountId, "/vaults/",
               toText _umpVaultName, "/multipart-uploads/",
               toText _umpUploadId]

instance ToQuery UploadMultipartPart where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'uploadMultipartPartResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umprChecksum'
--
-- * 'umprStatus'
data UploadMultipartPartResponse = UploadMultipartPartResponse'
    { _umprChecksum :: !(Maybe Text)
    , _umprStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadMultipartPartResponse' smart constructor.
uploadMultipartPartResponse :: Int -> UploadMultipartPartResponse
uploadMultipartPartResponse pStatus =
    UploadMultipartPartResponse'
    { _umprChecksum = Nothing
    , _umprStatus = pStatus
    }

-- | The SHA256 tree hash that Amazon Glacier computed for the uploaded part.
umprChecksum :: Lens' UploadMultipartPartResponse (Maybe Text)
umprChecksum = lens _umprChecksum (\ s a -> s{_umprChecksum = a});

-- | FIXME: Undocumented member.
umprStatus :: Lens' UploadMultipartPartResponse Int
umprStatus = lens _umprStatus (\ s a -> s{_umprStatus = a});

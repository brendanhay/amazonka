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
-- Module      : Network.AWS.Glacier.UploadMultipartPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation uploads a part of an archive. You can upload archive parts in any order. You can also upload them in parallel. You can upload up to 10,000 parts for a multipart upload.
--
--
-- Amazon Glacier rejects your upload part request if any of the following conditions is true:
--
--     * __SHA256 tree hash does not match__ To ensure that part data is not corrupted in transmission, you compute a SHA256 tree hash of the part and include it in your request. Upon receiving the part data, Amazon Glacier also computes a SHA256 tree hash. If these hash values don't match, the operation fails. For information about computing a SHA256 tree hash, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums> .
--
--     * __Part size does not match__ The size of each part except the last must match the size specified in the corresponding 'InitiateMultipartUpload' request. The size of the last part must be the same size as, or smaller than, the specified size.
--
--     * __Range does not align__ The byte range value in the request does not align with the part size specified in the corresponding initiate request. For example, if you specify a part size of 4194304 bytes (4 MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to 8388607 (8 MB - 1) are valid part ranges. However, if you set a range value of 2 MB to 6 MB, the range does not align with the part size and the upload will fail.
--
--
--
-- This operation is idempotent. If you upload the same part multiple times, the data included in the most recent request overwrites the previously uploaded data.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html Upload Part > in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.UploadMultipartPart
    (
    -- * Creating a Request
      uploadMultipartPart
    , UploadMultipartPart
    -- * Request Lenses
    , umpChecksum
    , umpRange
    , umpAccountId
    , umpVaultName
    , umpUploadId
    , umpBody

    -- * Destructuring the Response
    , uploadMultipartPartResponse
    , UploadMultipartPartResponse
    -- * Response Lenses
    , umprsChecksum
    , umprsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options to upload a part of an archive in a multipart upload operation.
--
--
--
-- /See:/ 'uploadMultipartPart' smart constructor.
data UploadMultipartPart = UploadMultipartPart'
  { _umpChecksum  :: !(Maybe Text)
  , _umpRange     :: !(Maybe Text)
  , _umpAccountId :: !Text
  , _umpVaultName :: !Text
  , _umpUploadId  :: !Text
  , _umpBody      :: !HashedBody
  } deriving (Show, Generic)


-- | Creates a value of 'UploadMultipartPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umpChecksum' - The SHA256 tree hash of the data being uploaded.
--
-- * 'umpRange' - Identifies the range of bytes in the assembled archive that will be uploaded in this part. Amazon Glacier uses this information to assemble the archive in the proper sequence. The format of this header follows RFC 2616. An example header is Content-Range:bytes 0-4194303/*.
--
-- * 'umpAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'umpVaultName' - The name of the vault.
--
-- * 'umpUploadId' - The upload ID of the multipart upload.
--
-- * 'umpBody' - The data to upload.
uploadMultipartPart
    :: Text -- ^ 'umpAccountId'
    -> Text -- ^ 'umpVaultName'
    -> Text -- ^ 'umpUploadId'
    -> HashedBody -- ^ 'umpBody'
    -> UploadMultipartPart
uploadMultipartPart pAccountId_ pVaultName_ pUploadId_ pBody_ =
  UploadMultipartPart'
    { _umpChecksum = Nothing
    , _umpRange = Nothing
    , _umpAccountId = pAccountId_
    , _umpVaultName = pVaultName_
    , _umpUploadId = pUploadId_
    , _umpBody = pBody_
    }


-- | The SHA256 tree hash of the data being uploaded.
umpChecksum :: Lens' UploadMultipartPart (Maybe Text)
umpChecksum = lens _umpChecksum (\ s a -> s{_umpChecksum = a})

-- | Identifies the range of bytes in the assembled archive that will be uploaded in this part. Amazon Glacier uses this information to assemble the archive in the proper sequence. The format of this header follows RFC 2616. An example header is Content-Range:bytes 0-4194303/*.
umpRange :: Lens' UploadMultipartPart (Maybe Text)
umpRange = lens _umpRange (\ s a -> s{_umpRange = a})

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
umpAccountId :: Lens' UploadMultipartPart Text
umpAccountId = lens _umpAccountId (\ s a -> s{_umpAccountId = a})

-- | The name of the vault.
umpVaultName :: Lens' UploadMultipartPart Text
umpVaultName = lens _umpVaultName (\ s a -> s{_umpVaultName = a})

-- | The upload ID of the multipart upload.
umpUploadId :: Lens' UploadMultipartPart Text
umpUploadId = lens _umpUploadId (\ s a -> s{_umpUploadId = a})

-- | The data to upload.
umpBody :: Lens' UploadMultipartPart HashedBody
umpBody = lens _umpBody (\ s a -> s{_umpBody = a})

instance AWSRequest UploadMultipartPart where
        type Rs UploadMultipartPart =
             UploadMultipartPartResponse
        request = putBody glacier
        response
          = receiveEmpty
              (\ s h x ->
                 UploadMultipartPartResponse' <$>
                   (h .#? "x-amz-sha256-tree-hash") <*>
                     (pure (fromEnum s)))

instance ToBody UploadMultipartPart where
        toBody = toBody . _umpBody

instance ToHeaders UploadMultipartPart where
        toHeaders UploadMultipartPart'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _umpChecksum,
               "Content-Range" =# _umpRange]

instance ToPath UploadMultipartPart where
        toPath UploadMultipartPart'{..}
          = mconcat
              ["/", toBS _umpAccountId, "/vaults/",
               toBS _umpVaultName, "/multipart-uploads/",
               toBS _umpUploadId]

instance ToQuery UploadMultipartPart where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'uploadMultipartPartResponse' smart constructor.
data UploadMultipartPartResponse = UploadMultipartPartResponse'
  { _umprsChecksum       :: !(Maybe Text)
  , _umprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadMultipartPartResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umprsChecksum' - The SHA256 tree hash that Amazon Glacier computed for the uploaded part.
--
-- * 'umprsResponseStatus' - -- | The response status code.
uploadMultipartPartResponse
    :: Int -- ^ 'umprsResponseStatus'
    -> UploadMultipartPartResponse
uploadMultipartPartResponse pResponseStatus_ =
  UploadMultipartPartResponse'
    {_umprsChecksum = Nothing, _umprsResponseStatus = pResponseStatus_}


-- | The SHA256 tree hash that Amazon Glacier computed for the uploaded part.
umprsChecksum :: Lens' UploadMultipartPartResponse (Maybe Text)
umprsChecksum = lens _umprsChecksum (\ s a -> s{_umprsChecksum = a})

-- | -- | The response status code.
umprsResponseStatus :: Lens' UploadMultipartPartResponse Int
umprsResponseStatus = lens _umprsResponseStatus (\ s a -> s{_umprsResponseStatus = a})

instance NFData UploadMultipartPartResponse where

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

-- Module      : Network.AWS.Glacier.UploadMultipartPart
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

-- | This operation uploads a part of an archive. You can upload archive parts in
-- any order. You can also upload them in parallel. You can upload up to 10,000
-- parts for a multipart upload.
--
-- Amazon Glacier rejects your upload part request if any of the following
-- conditions is true:
--
-- SHA256 tree hash does not matchTo ensure that part data is not corrupted
-- in transmission, you compute a SHA256 tree hash of the part and include it in
-- your request. Upon receiving the part data, Amazon Glacier also computes a
-- SHA256 tree hash. If these hash values don't match, the operation fails. For
-- information about computing a SHA256 tree hash, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- Part size does not matchThe size of each part except the last must match
-- the size specified in the corresponding 'InitiateMultipartUpload' request. The
-- size of the last part must be the same size as, or smaller than, the
-- specified size.
--
-- If you upload a part whose size is smaller than the part size you specified
-- in your initiate multipart upload request and that part is not the last part,
-- then the upload part request will succeed. However, the subsequent Complete
-- Multipart Upload request will fail.
--
-- Range does not alignThe byte range value in the request does not align
-- with the part size specified in the corresponding initiate request. For
-- example, if you specify a part size of 4194304 bytes (4 MB), then 0 to
-- 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to 8388607 (8 MB - 1) are valid
-- part ranges. However, if you set a range value of 2 MB to 6 MB, the range
-- does not align with the part size and the upload will fail.   This operation
-- is idempotent. If you upload the same part multiple times, the data included
-- in the most recent request overwrites the previously uploaded data.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading LargeArchives in Parts (Multipart Upload)> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html Upload Part > in the /Amazon GlacierDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-UploadMultipartPart.html>
module Network.AWS.Glacier.UploadMultipartPart
    (
    -- * Request
      UploadMultipartPart
    -- ** Request constructor
    , uploadMultipartPart
    -- ** Request lenses
    , umpAccountId
    , umpBody
    , umpChecksum
    , umpRange
    , umpUploadId
    , umpVaultName

    -- * Response
    , UploadMultipartPartResponse
    -- ** Response constructor
    , uploadMultipartPartResponse
    -- ** Response lenses
    , umprChecksum
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data UploadMultipartPart = UploadMultipartPart
    { _umpAccountId :: Text
    , _umpBody      :: RqBody
    , _umpChecksum  :: Maybe Text
    , _umpRange     :: Maybe Text
    , _umpUploadId  :: Text
    , _umpVaultName :: Text
    } deriving (Show)

-- | 'UploadMultipartPart' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umpAccountId' @::@ 'Text'
--
-- * 'umpBody' @::@ 'RqBody'
--
-- * 'umpChecksum' @::@ 'Maybe' 'Text'
--
-- * 'umpRange' @::@ 'Maybe' 'Text'
--
-- * 'umpUploadId' @::@ 'Text'
--
-- * 'umpVaultName' @::@ 'Text'
--
uploadMultipartPart :: Text -- ^ 'umpAccountId'
                    -> Text -- ^ 'umpVaultName'
                    -> Text -- ^ 'umpUploadId'
                    -> RqBody -- ^ 'umpBody'
                    -> UploadMultipartPart
uploadMultipartPart p1 p2 p3 p4 = UploadMultipartPart
    { _umpAccountId = p1
    , _umpVaultName = p2
    , _umpUploadId  = p3
    , _umpBody      = p4
    , _umpChecksum  = Nothing
    , _umpRange     = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
umpAccountId :: Lens' UploadMultipartPart Text
umpAccountId = lens _umpAccountId (\s a -> s { _umpAccountId = a })

-- | The data to upload.
umpBody :: Lens' UploadMultipartPart RqBody
umpBody = lens _umpBody (\s a -> s { _umpBody = a })

-- | The SHA256 tree hash of the data being uploaded.
umpChecksum :: Lens' UploadMultipartPart (Maybe Text)
umpChecksum = lens _umpChecksum (\s a -> s { _umpChecksum = a })

-- | Identifies the range of bytes in the assembled archive that will be uploaded
-- in this part. Amazon Glacier uses this information to assemble the archive in
-- the proper sequence. The format of this header follows RFC 2616. An example
-- header is Content-Range:bytes 0-4194303/*.
umpRange :: Lens' UploadMultipartPart (Maybe Text)
umpRange = lens _umpRange (\s a -> s { _umpRange = a })

-- | The upload ID of the multipart upload.
umpUploadId :: Lens' UploadMultipartPart Text
umpUploadId = lens _umpUploadId (\s a -> s { _umpUploadId = a })

-- | The name of the vault.
umpVaultName :: Lens' UploadMultipartPart Text
umpVaultName = lens _umpVaultName (\s a -> s { _umpVaultName = a })

newtype UploadMultipartPartResponse = UploadMultipartPartResponse
    { _umprChecksum :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UploadMultipartPartResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umprChecksum' @::@ 'Maybe' 'Text'
--
uploadMultipartPartResponse :: UploadMultipartPartResponse
uploadMultipartPartResponse = UploadMultipartPartResponse
    { _umprChecksum = Nothing
    }

-- | The SHA256 tree hash that Amazon Glacier computed for the uploaded part.
umprChecksum :: Lens' UploadMultipartPartResponse (Maybe Text)
umprChecksum = lens _umprChecksum (\s a -> s { _umprChecksum = a })

instance ToPath UploadMultipartPart where
    toPath UploadMultipartPart{..} = mconcat
        [ "/"
        , toText _umpAccountId
        , "/vaults/"
        , toText _umpVaultName
        , "/multipart-uploads/"
        , toText _umpUploadId
        ]

instance ToQuery UploadMultipartPart where
    toQuery = const mempty

instance ToHeaders UploadMultipartPart where
    toHeaders UploadMultipartPart{..} = mconcat
        [ "x-amz-sha256-tree-hash" =: _umpChecksum
        , "Content-Range"          =: _umpRange
        ]

instance ToBody UploadMultipartPart where
    toBody = toBody . _umpBody

instance AWSRequest UploadMultipartPart where
    type Sv UploadMultipartPart = Glacier
    type Rs UploadMultipartPart = UploadMultipartPartResponse

    request  = stream PUT
    response = headerResponse $ \h -> UploadMultipartPartResponse
        <$> h ~:? "x-amz-sha256-tree-hash"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation uses the @encryption@ subresource to set the default encryption state of an existing bucket.
--
--
-- This implementation of the @PUT@ operation sets default encryption for a bucket using server-side encryption with Amazon S3-managed keys SSE-S3 or AWS KMS customer master keys (CMKs) (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
--
-- /Important:/ This operation requires AWS Signature Version 4. For more information, see <sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> .
--
-- To use this operation, you must have permissions to perform the @s3:PutEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the Amazon Simple Storage Service Developer Guide.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.PutBucketEncryption
  ( -- * Creating a Request
    putBucketEncryption,
    PutBucketEncryption,

    -- * Request Lenses
    pbeContentMD5,
    pbeExpectedBucketOwner,
    pbeBucket,
    pbeServerSideEncryptionConfiguration,

    -- * Destructuring the Response
    putBucketEncryptionResponse,
    PutBucketEncryptionResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
  { _pbeContentMD5 ::
      !(Maybe Text),
    _pbeExpectedBucketOwner :: !(Maybe Text),
    _pbeBucket :: !BucketName,
    _pbeServerSideEncryptionConfiguration ::
      !ServerSideEncryptionConfiguration
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbeContentMD5' - The base64-encoded 128-bit MD5 digest of the server-side encryption configuration. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbeExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbeBucket' - Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'pbeServerSideEncryptionConfiguration' - Undocumented member.
putBucketEncryption ::
  -- | 'pbeBucket'
  BucketName ->
  -- | 'pbeServerSideEncryptionConfiguration'
  ServerSideEncryptionConfiguration ->
  PutBucketEncryption
putBucketEncryption pBucket_ pServerSideEncryptionConfiguration_ =
  PutBucketEncryption'
    { _pbeContentMD5 = Nothing,
      _pbeExpectedBucketOwner = Nothing,
      _pbeBucket = pBucket_,
      _pbeServerSideEncryptionConfiguration =
        pServerSideEncryptionConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the server-side encryption configuration. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbeContentMD5 :: Lens' PutBucketEncryption (Maybe Text)
pbeContentMD5 = lens _pbeContentMD5 (\s a -> s {_pbeContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbeExpectedBucketOwner :: Lens' PutBucketEncryption (Maybe Text)
pbeExpectedBucketOwner = lens _pbeExpectedBucketOwner (\s a -> s {_pbeExpectedBucketOwner = a})

-- | Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
pbeBucket :: Lens' PutBucketEncryption BucketName
pbeBucket = lens _pbeBucket (\s a -> s {_pbeBucket = a})

-- | Undocumented member.
pbeServerSideEncryptionConfiguration :: Lens' PutBucketEncryption ServerSideEncryptionConfiguration
pbeServerSideEncryptionConfiguration = lens _pbeServerSideEncryptionConfiguration (\s a -> s {_pbeServerSideEncryptionConfiguration = a})

instance AWSRequest PutBucketEncryption where
  type Rs PutBucketEncryption = PutBucketEncryptionResponse
  request = putXML s3
  response = receiveNull PutBucketEncryptionResponse'

instance Hashable PutBucketEncryption

instance NFData PutBucketEncryption

instance ToElement PutBucketEncryption where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ServerSideEncryptionConfiguration"
      . _pbeServerSideEncryptionConfiguration

instance ToHeaders PutBucketEncryption where
  toHeaders PutBucketEncryption' {..} =
    mconcat
      [ "Content-MD5" =# _pbeContentMD5,
        "x-amz-expected-bucket-owner" =# _pbeExpectedBucketOwner
      ]

instance ToPath PutBucketEncryption where
  toPath PutBucketEncryption' {..} = mconcat ["/", toBS _pbeBucket]

instance ToQuery PutBucketEncryption where
  toQuery = const (mconcat ["encryption"])

-- | /See:/ 'putBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse = PutBucketEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketEncryptionResponse' with the minimum fields required to make a request.
putBucketEncryptionResponse ::
  PutBucketEncryptionResponse
putBucketEncryptionResponse = PutBucketEncryptionResponse'

instance NFData PutBucketEncryptionResponse

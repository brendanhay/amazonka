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
-- Module      : Network.AWS.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default encryption configuration for an Amazon S3 bucket. For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
--
--
-- To use this operation, you must have permission to perform the @s3:GetEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- The following operations are related to @GetBucketEncryption@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.GetBucketEncryption
  ( -- * Creating a Request
    getBucketEncryption,
    GetBucketEncryption,

    -- * Request Lenses
    gbeExpectedBucketOwner,
    gbeBucket,

    -- * Destructuring the Response
    getBucketEncryptionResponse,
    GetBucketEncryptionResponse,

    -- * Response Lenses
    gbersServerSideEncryptionConfiguration,
    gbersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketEncryption' smart constructor.
data GetBucketEncryption = GetBucketEncryption'
  { _gbeExpectedBucketOwner ::
      !(Maybe Text),
    _gbeBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbeExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbeBucket' - The name of the bucket from which the server-side encryption configuration is retrieved.
getBucketEncryption ::
  -- | 'gbeBucket'
  BucketName ->
  GetBucketEncryption
getBucketEncryption pBucket_ =
  GetBucketEncryption'
    { _gbeExpectedBucketOwner = Nothing,
      _gbeBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbeExpectedBucketOwner :: Lens' GetBucketEncryption (Maybe Text)
gbeExpectedBucketOwner = lens _gbeExpectedBucketOwner (\s a -> s {_gbeExpectedBucketOwner = a})

-- | The name of the bucket from which the server-side encryption configuration is retrieved.
gbeBucket :: Lens' GetBucketEncryption BucketName
gbeBucket = lens _gbeBucket (\s a -> s {_gbeBucket = a})

instance AWSRequest GetBucketEncryption where
  type Rs GetBucketEncryption = GetBucketEncryptionResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketEncryptionResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketEncryption

instance NFData GetBucketEncryption

instance ToHeaders GetBucketEncryption where
  toHeaders GetBucketEncryption' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbeExpectedBucketOwner]

instance ToPath GetBucketEncryption where
  toPath GetBucketEncryption' {..} = mconcat ["/", toBS _gbeBucket]

instance ToQuery GetBucketEncryption where
  toQuery = const (mconcat ["encryption"])

-- | /See:/ 'getBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { _gbersServerSideEncryptionConfiguration ::
      !( Maybe
           ServerSideEncryptionConfiguration
       ),
    _gbersResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketEncryptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbersServerSideEncryptionConfiguration' - Undocumented member.
--
-- * 'gbersResponseStatus' - -- | The response status code.
getBucketEncryptionResponse ::
  -- | 'gbersResponseStatus'
  Int ->
  GetBucketEncryptionResponse
getBucketEncryptionResponse pResponseStatus_ =
  GetBucketEncryptionResponse'
    { _gbersServerSideEncryptionConfiguration =
        Nothing,
      _gbersResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gbersServerSideEncryptionConfiguration :: Lens' GetBucketEncryptionResponse (Maybe ServerSideEncryptionConfiguration)
gbersServerSideEncryptionConfiguration = lens _gbersServerSideEncryptionConfiguration (\s a -> s {_gbersServerSideEncryptionConfiguration = a})

-- | -- | The response status code.
gbersResponseStatus :: Lens' GetBucketEncryptionResponse Int
gbersResponseStatus = lens _gbersResponseStatus (\s a -> s {_gbersResponseStatus = a})

instance NFData GetBucketEncryptionResponse

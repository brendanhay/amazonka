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
-- Module      : Network.AWS.S3.GetPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ settings are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
--
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
--
-- The following operations are related to @GetPublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetPublicAccessBlock
  ( -- * Creating a Request
    getPublicAccessBlock,
    GetPublicAccessBlock,

    -- * Request Lenses
    gpabExpectedBucketOwner,
    gpabBucket,

    -- * Destructuring the Response
    getPublicAccessBlockResponse,
    GetPublicAccessBlockResponse,

    -- * Response Lenses
    gpabrsPublicAccessBlockConfiguration,
    gpabrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getPublicAccessBlock' smart constructor.
data GetPublicAccessBlock = GetPublicAccessBlock'
  { _gpabExpectedBucketOwner ::
      !(Maybe Text),
    _gpabBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicAccessBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpabExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gpabBucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
getPublicAccessBlock ::
  -- | 'gpabBucket'
  BucketName ->
  GetPublicAccessBlock
getPublicAccessBlock pBucket_ =
  GetPublicAccessBlock'
    { _gpabExpectedBucketOwner = Nothing,
      _gpabBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gpabExpectedBucketOwner :: Lens' GetPublicAccessBlock (Maybe Text)
gpabExpectedBucketOwner = lens _gpabExpectedBucketOwner (\s a -> s {_gpabExpectedBucketOwner = a})

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
gpabBucket :: Lens' GetPublicAccessBlock BucketName
gpabBucket = lens _gpabBucket (\s a -> s {_gpabBucket = a})

instance AWSRequest GetPublicAccessBlock where
  type Rs GetPublicAccessBlock = GetPublicAccessBlockResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetPublicAccessBlockResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetPublicAccessBlock

instance NFData GetPublicAccessBlock

instance ToHeaders GetPublicAccessBlock where
  toHeaders GetPublicAccessBlock' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gpabExpectedBucketOwner]

instance ToPath GetPublicAccessBlock where
  toPath GetPublicAccessBlock' {..} = mconcat ["/", toBS _gpabBucket]

instance ToQuery GetPublicAccessBlock where
  toQuery = const (mconcat ["publicAccessBlock"])

-- | /See:/ 'getPublicAccessBlockResponse' smart constructor.
data GetPublicAccessBlockResponse = GetPublicAccessBlockResponse'
  { _gpabrsPublicAccessBlockConfiguration ::
      !( Maybe
           PublicAccessBlockConfiguration
       ),
    _gpabrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicAccessBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpabrsPublicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
--
-- * 'gpabrsResponseStatus' - -- | The response status code.
getPublicAccessBlockResponse ::
  -- | 'gpabrsResponseStatus'
  Int ->
  GetPublicAccessBlockResponse
getPublicAccessBlockResponse pResponseStatus_ =
  GetPublicAccessBlockResponse'
    { _gpabrsPublicAccessBlockConfiguration =
        Nothing,
      _gpabrsResponseStatus = pResponseStatus_
    }

-- | The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
gpabrsPublicAccessBlockConfiguration :: Lens' GetPublicAccessBlockResponse (Maybe PublicAccessBlockConfiguration)
gpabrsPublicAccessBlockConfiguration = lens _gpabrsPublicAccessBlockConfiguration (\s a -> s {_gpabrsPublicAccessBlockConfiguration = a})

-- | -- | The response status code.
gpabrsResponseStatus :: Lens' GetPublicAccessBlockResponse Int
gpabrsResponseStatus = lens _gpabrsResponseStatus (\s a -> s {_gpabrsResponseStatus = a})

instance NFData GetPublicAccessBlockResponse

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
-- Module      : Network.AWS.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ configurations are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
--
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
module Network.AWS.S3.PutPublicAccessBlock
  ( -- * Creating a Request
    putPublicAccessBlock,
    PutPublicAccessBlock,

    -- * Request Lenses
    ppabContentMD5,
    ppabExpectedBucketOwner,
    ppabBucket,
    ppabPublicAccessBlockConfiguration,

    -- * Destructuring the Response
    putPublicAccessBlockResponse,
    PutPublicAccessBlockResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
  { _ppabContentMD5 ::
      !(Maybe Text),
    _ppabExpectedBucketOwner :: !(Maybe Text),
    _ppabBucket :: !BucketName,
    _ppabPublicAccessBlockConfiguration ::
      !PublicAccessBlockConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPublicAccessBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppabContentMD5' - The MD5 hash of the @PutPublicAccessBlock@ request body.  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'ppabExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'ppabBucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
--
-- * 'ppabPublicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
putPublicAccessBlock ::
  -- | 'ppabBucket'
  BucketName ->
  -- | 'ppabPublicAccessBlockConfiguration'
  PublicAccessBlockConfiguration ->
  PutPublicAccessBlock
putPublicAccessBlock pBucket_ pPublicAccessBlockConfiguration_ =
  PutPublicAccessBlock'
    { _ppabContentMD5 = Nothing,
      _ppabExpectedBucketOwner = Nothing,
      _ppabBucket = pBucket_,
      _ppabPublicAccessBlockConfiguration =
        pPublicAccessBlockConfiguration_
    }

-- | The MD5 hash of the @PutPublicAccessBlock@ request body.  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
ppabContentMD5 :: Lens' PutPublicAccessBlock (Maybe Text)
ppabContentMD5 = lens _ppabContentMD5 (\s a -> s {_ppabContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
ppabExpectedBucketOwner :: Lens' PutPublicAccessBlock (Maybe Text)
ppabExpectedBucketOwner = lens _ppabExpectedBucketOwner (\s a -> s {_ppabExpectedBucketOwner = a})

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
ppabBucket :: Lens' PutPublicAccessBlock BucketName
ppabBucket = lens _ppabBucket (\s a -> s {_ppabBucket = a})

-- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
ppabPublicAccessBlockConfiguration :: Lens' PutPublicAccessBlock PublicAccessBlockConfiguration
ppabPublicAccessBlockConfiguration = lens _ppabPublicAccessBlockConfiguration (\s a -> s {_ppabPublicAccessBlockConfiguration = a})

instance AWSRequest PutPublicAccessBlock where
  type Rs PutPublicAccessBlock = PutPublicAccessBlockResponse
  request = putXML s3
  response = receiveNull PutPublicAccessBlockResponse'

instance Hashable PutPublicAccessBlock

instance NFData PutPublicAccessBlock

instance ToElement PutPublicAccessBlock where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}PublicAccessBlockConfiguration"
      . _ppabPublicAccessBlockConfiguration

instance ToHeaders PutPublicAccessBlock where
  toHeaders PutPublicAccessBlock' {..} =
    mconcat
      [ "Content-MD5" =# _ppabContentMD5,
        "x-amz-expected-bucket-owner" =# _ppabExpectedBucketOwner
      ]

instance ToPath PutPublicAccessBlock where
  toPath PutPublicAccessBlock' {..} = mconcat ["/", toBS _ppabBucket]

instance ToQuery PutPublicAccessBlock where
  toQuery = const (mconcat ["publicAccessBlock"])

-- | /See:/ 'putPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPublicAccessBlockResponse' with the minimum fields required to make a request.
putPublicAccessBlockResponse ::
  PutPublicAccessBlockResponse
putPublicAccessBlockResponse = PutPublicAccessBlockResponse'

instance NFData PutPublicAccessBlockResponse

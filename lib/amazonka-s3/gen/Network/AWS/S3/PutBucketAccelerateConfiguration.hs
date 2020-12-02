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
-- Module      : Network.AWS.S3.PutBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the accelerate configuration of an existing bucket. Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to Amazon S3.
--
--
-- To use this operation, you must have permission to perform the s3:PutAccelerateConfiguration action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- The Transfer Acceleration state of a bucket can be set to one of the following two values:
--
--     * Enabled – Enables accelerated data transfers to the bucket.
--
--     * Suspended – Disables accelerated data transfers to the bucket.
--
--
--
-- The <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration> operation returns the transfer acceleration state of a bucket.
--
-- After setting the Transfer Acceleration state of a bucket to Enabled, it might take up to thirty minutes before the data transfer rates to the bucket increase.
--
-- The name of the bucket used for Transfer Acceleration must be DNS-compliant and must not contain periods (".").
--
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> .
--
-- The following operations are related to @PutBucketAccelerateConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.PutBucketAccelerateConfiguration
  ( -- * Creating a Request
    putBucketAccelerateConfiguration,
    PutBucketAccelerateConfiguration,

    -- * Request Lenses
    pbacExpectedBucketOwner,
    pbacBucket,
    pbacAccelerateConfiguration,

    -- * Destructuring the Response
    putBucketAccelerateConfigurationResponse,
    PutBucketAccelerateConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketAccelerateConfiguration' smart constructor.
data PutBucketAccelerateConfiguration = PutBucketAccelerateConfiguration'
  { _pbacExpectedBucketOwner ::
      !(Maybe Text),
    _pbacBucket ::
      !BucketName,
    _pbacAccelerateConfiguration ::
      !AccelerateConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbacExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbacBucket' - The name of the bucket for which the accelerate configuration is set.
--
-- * 'pbacAccelerateConfiguration' - Container for setting the transfer acceleration state.
putBucketAccelerateConfiguration ::
  -- | 'pbacBucket'
  BucketName ->
  -- | 'pbacAccelerateConfiguration'
  AccelerateConfiguration ->
  PutBucketAccelerateConfiguration
putBucketAccelerateConfiguration pBucket_ pAccelerateConfiguration_ =
  PutBucketAccelerateConfiguration'
    { _pbacExpectedBucketOwner =
        Nothing,
      _pbacBucket = pBucket_,
      _pbacAccelerateConfiguration = pAccelerateConfiguration_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbacExpectedBucketOwner :: Lens' PutBucketAccelerateConfiguration (Maybe Text)
pbacExpectedBucketOwner = lens _pbacExpectedBucketOwner (\s a -> s {_pbacExpectedBucketOwner = a})

-- | The name of the bucket for which the accelerate configuration is set.
pbacBucket :: Lens' PutBucketAccelerateConfiguration BucketName
pbacBucket = lens _pbacBucket (\s a -> s {_pbacBucket = a})

-- | Container for setting the transfer acceleration state.
pbacAccelerateConfiguration :: Lens' PutBucketAccelerateConfiguration AccelerateConfiguration
pbacAccelerateConfiguration = lens _pbacAccelerateConfiguration (\s a -> s {_pbacAccelerateConfiguration = a})

instance AWSRequest PutBucketAccelerateConfiguration where
  type
    Rs PutBucketAccelerateConfiguration =
      PutBucketAccelerateConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketAccelerateConfigurationResponse'

instance Hashable PutBucketAccelerateConfiguration

instance NFData PutBucketAccelerateConfiguration

instance ToElement PutBucketAccelerateConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccelerateConfiguration"
      . _pbacAccelerateConfiguration

instance ToHeaders PutBucketAccelerateConfiguration where
  toHeaders PutBucketAccelerateConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _pbacExpectedBucketOwner]

instance ToPath PutBucketAccelerateConfiguration where
  toPath PutBucketAccelerateConfiguration' {..} =
    mconcat ["/", toBS _pbacBucket]

instance ToQuery PutBucketAccelerateConfiguration where
  toQuery = const (mconcat ["accelerate"])

-- | /See:/ 'putBucketAccelerateConfigurationResponse' smart constructor.
data PutBucketAccelerateConfigurationResponse = PutBucketAccelerateConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
putBucketAccelerateConfigurationResponse ::
  PutBucketAccelerateConfigurationResponse
putBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'

instance NFData PutBucketAccelerateConfigurationResponse

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
-- Module      : Network.AWS.S3.PutBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation adds an inventory configuration (identified by the inventory ID) to the bucket. You can have up to 1,000 inventory configurations per bucket.
--
--
-- Amazon S3 inventory generates inventories of the objects in the bucket on a daily or weekly basis, and the results are published to a flat file. The bucket that is inventoried is called the /source/ bucket, and the bucket where the inventory flat file is stored is called the /destination/ bucket. The /destination/ bucket must be in the same AWS Region as the /source/ bucket.
--
-- When you configure an inventory for a /source/ bucket, you specify the /destination/ bucket where you want the inventory to be stored, and whether to generate the inventory daily or weekly. You can also configure what object metadata to include and whether to inventory all object versions or only current versions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> in the Amazon Simple Storage Service Developer Guide.
--
-- /Important:/ You must create a bucket policy on the /destination/ bucket to grant permissions to Amazon S3 to write objects to the bucket in the defined location. For an example policy, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis> .
--
-- To use this operation, you must have permissions to perform the @s3:PutInventoryConfiguration@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the Amazon Simple Storage Service Developer Guide.
--
-- __Special Errors__
--
--     * __HTTP 400 Bad Request Error__
--
--     * /Code:/ InvalidArgument
--
--     * /Cause:/ Invalid Argument
--
--
--
--     * __HTTP 400 Bad Request Error__
--
--     * /Code:/ TooManyConfigurations
--
--     * /Cause:/ You are attempting to create a new configuration but have already reached the 1,000-configuration limit.
--
--
--
--     * __HTTP 403 Forbidden Error__
--
--     * /Code:/ AccessDenied
--
--     * /Cause:/ You are not the owner of the specified bucket, or you do not have the @s3:PutInventoryConfiguration@ bucket permission to set the configuration on the bucket.
--
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Network.AWS.S3.PutBucketInventoryConfiguration
  ( -- * Creating a Request
    putBucketInventoryConfiguration,
    PutBucketInventoryConfiguration,

    -- * Request Lenses
    pbicExpectedBucketOwner,
    pbicBucket,
    pbicId,
    pbicInventoryConfiguration,

    -- * Destructuring the Response
    putBucketInventoryConfigurationResponse,
    PutBucketInventoryConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketInventoryConfiguration' smart constructor.
data PutBucketInventoryConfiguration = PutBucketInventoryConfiguration'
  { _pbicExpectedBucketOwner ::
      !(Maybe Text),
    _pbicBucket :: !BucketName,
    _pbicId :: !Text,
    _pbicInventoryConfiguration ::
      !InventoryConfiguration
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbicExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbicBucket' - The name of the bucket where the inventory configuration will be stored.
--
-- * 'pbicId' - The ID used to identify the inventory configuration.
--
-- * 'pbicInventoryConfiguration' - Specifies the inventory configuration.
putBucketInventoryConfiguration ::
  -- | 'pbicBucket'
  BucketName ->
  -- | 'pbicId'
  Text ->
  -- | 'pbicInventoryConfiguration'
  InventoryConfiguration ->
  PutBucketInventoryConfiguration
putBucketInventoryConfiguration
  pBucket_
  pId_
  pInventoryConfiguration_ =
    PutBucketInventoryConfiguration'
      { _pbicExpectedBucketOwner =
          Nothing,
        _pbicBucket = pBucket_,
        _pbicId = pId_,
        _pbicInventoryConfiguration = pInventoryConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbicExpectedBucketOwner :: Lens' PutBucketInventoryConfiguration (Maybe Text)
pbicExpectedBucketOwner = lens _pbicExpectedBucketOwner (\s a -> s {_pbicExpectedBucketOwner = a})

-- | The name of the bucket where the inventory configuration will be stored.
pbicBucket :: Lens' PutBucketInventoryConfiguration BucketName
pbicBucket = lens _pbicBucket (\s a -> s {_pbicBucket = a})

-- | The ID used to identify the inventory configuration.
pbicId :: Lens' PutBucketInventoryConfiguration Text
pbicId = lens _pbicId (\s a -> s {_pbicId = a})

-- | Specifies the inventory configuration.
pbicInventoryConfiguration :: Lens' PutBucketInventoryConfiguration InventoryConfiguration
pbicInventoryConfiguration = lens _pbicInventoryConfiguration (\s a -> s {_pbicInventoryConfiguration = a})

instance AWSRequest PutBucketInventoryConfiguration where
  type
    Rs PutBucketInventoryConfiguration =
      PutBucketInventoryConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketInventoryConfigurationResponse'

instance Hashable PutBucketInventoryConfiguration

instance NFData PutBucketInventoryConfiguration

instance ToElement PutBucketInventoryConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}InventoryConfiguration"
      . _pbicInventoryConfiguration

instance ToHeaders PutBucketInventoryConfiguration where
  toHeaders PutBucketInventoryConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _pbicExpectedBucketOwner]

instance ToPath PutBucketInventoryConfiguration where
  toPath PutBucketInventoryConfiguration' {..} =
    mconcat ["/", toBS _pbicBucket]

instance ToQuery PutBucketInventoryConfiguration where
  toQuery PutBucketInventoryConfiguration' {..} =
    mconcat ["id" =: _pbicId, "inventory"]

-- | /See:/ 'putBucketInventoryConfigurationResponse' smart constructor.
data PutBucketInventoryConfigurationResponse = PutBucketInventoryConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
putBucketInventoryConfigurationResponse ::
  PutBucketInventoryConfigurationResponse
putBucketInventoryConfigurationResponse =
  PutBucketInventoryConfigurationResponse'

instance NFData PutBucketInventoryConfigurationResponse

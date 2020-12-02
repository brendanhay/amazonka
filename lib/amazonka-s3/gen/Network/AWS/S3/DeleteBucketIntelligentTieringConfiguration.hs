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
-- Module      : Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 Intelligent-Tiering configuration from the specified bucket.
--
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- Operations related to @DeleteBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    deleteBucketIntelligentTieringConfiguration,
    DeleteBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    dbitcBucket,
    dbitcId,

    -- * Destructuring the Response
    deleteBucketIntelligentTieringConfigurationResponse,
    DeleteBucketIntelligentTieringConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketIntelligentTieringConfiguration' smart constructor.
data DeleteBucketIntelligentTieringConfiguration = DeleteBucketIntelligentTieringConfiguration'
  { _dbitcBucket ::
      !BucketName,
    _dbitcId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbitcBucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- * 'dbitcId' - The ID used to identify the S3 Intelligent-Tiering configuration.
deleteBucketIntelligentTieringConfiguration ::
  -- | 'dbitcBucket'
  BucketName ->
  -- | 'dbitcId'
  Text ->
  DeleteBucketIntelligentTieringConfiguration
deleteBucketIntelligentTieringConfiguration pBucket_ pId_ =
  DeleteBucketIntelligentTieringConfiguration'
    { _dbitcBucket =
        pBucket_,
      _dbitcId = pId_
    }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
dbitcBucket :: Lens' DeleteBucketIntelligentTieringConfiguration BucketName
dbitcBucket = lens _dbitcBucket (\s a -> s {_dbitcBucket = a})

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
dbitcId :: Lens' DeleteBucketIntelligentTieringConfiguration Text
dbitcId = lens _dbitcId (\s a -> s {_dbitcId = a})

instance AWSRequest DeleteBucketIntelligentTieringConfiguration where
  type
    Rs DeleteBucketIntelligentTieringConfiguration =
      DeleteBucketIntelligentTieringConfigurationResponse
  request = delete s3
  response =
    receiveNull DeleteBucketIntelligentTieringConfigurationResponse'

instance Hashable DeleteBucketIntelligentTieringConfiguration

instance NFData DeleteBucketIntelligentTieringConfiguration

instance ToHeaders DeleteBucketIntelligentTieringConfiguration where
  toHeaders = const mempty

instance ToPath DeleteBucketIntelligentTieringConfiguration where
  toPath DeleteBucketIntelligentTieringConfiguration' {..} =
    mconcat ["/", toBS _dbitcBucket]

instance ToQuery DeleteBucketIntelligentTieringConfiguration where
  toQuery DeleteBucketIntelligentTieringConfiguration' {..} =
    mconcat ["id" =: _dbitcId, "intelligent-tiering"]

-- | /See:/ 'deleteBucketIntelligentTieringConfigurationResponse' smart constructor.
data DeleteBucketIntelligentTieringConfigurationResponse = DeleteBucketIntelligentTieringConfigurationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
deleteBucketIntelligentTieringConfigurationResponse ::
  DeleteBucketIntelligentTieringConfigurationResponse
deleteBucketIntelligentTieringConfigurationResponse =
  DeleteBucketIntelligentTieringConfigurationResponse'

instance NFData DeleteBucketIntelligentTieringConfigurationResponse

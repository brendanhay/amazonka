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
-- Module      : Network.AWS.S3.GetBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the S3 Intelligent-Tiering configuration from the specified bucket.
--
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- Operations related to @GetBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.GetBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    getBucketIntelligentTieringConfiguration,
    GetBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    gbitcBucket,
    gbitcId,

    -- * Destructuring the Response
    getBucketIntelligentTieringConfigurationResponse,
    GetBucketIntelligentTieringConfigurationResponse,

    -- * Response Lenses
    gbitcrsIntelligentTieringConfiguration,
    gbitcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketIntelligentTieringConfiguration' smart constructor.
data GetBucketIntelligentTieringConfiguration = GetBucketIntelligentTieringConfiguration'
  { _gbitcBucket ::
      !BucketName,
    _gbitcId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbitcBucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- * 'gbitcId' - The ID used to identify the S3 Intelligent-Tiering configuration.
getBucketIntelligentTieringConfiguration ::
  -- | 'gbitcBucket'
  BucketName ->
  -- | 'gbitcId'
  Text ->
  GetBucketIntelligentTieringConfiguration
getBucketIntelligentTieringConfiguration pBucket_ pId_ =
  GetBucketIntelligentTieringConfiguration'
    { _gbitcBucket =
        pBucket_,
      _gbitcId = pId_
    }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
gbitcBucket :: Lens' GetBucketIntelligentTieringConfiguration BucketName
gbitcBucket = lens _gbitcBucket (\s a -> s {_gbitcBucket = a})

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
gbitcId :: Lens' GetBucketIntelligentTieringConfiguration Text
gbitcId = lens _gbitcId (\s a -> s {_gbitcId = a})

instance AWSRequest GetBucketIntelligentTieringConfiguration where
  type
    Rs GetBucketIntelligentTieringConfiguration =
      GetBucketIntelligentTieringConfigurationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketIntelligentTieringConfigurationResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketIntelligentTieringConfiguration

instance NFData GetBucketIntelligentTieringConfiguration

instance ToHeaders GetBucketIntelligentTieringConfiguration where
  toHeaders = const mempty

instance ToPath GetBucketIntelligentTieringConfiguration where
  toPath GetBucketIntelligentTieringConfiguration' {..} =
    mconcat ["/", toBS _gbitcBucket]

instance ToQuery GetBucketIntelligentTieringConfiguration where
  toQuery GetBucketIntelligentTieringConfiguration' {..} =
    mconcat ["id" =: _gbitcId, "intelligent-tiering"]

-- | /See:/ 'getBucketIntelligentTieringConfigurationResponse' smart constructor.
data GetBucketIntelligentTieringConfigurationResponse = GetBucketIntelligentTieringConfigurationResponse'
  { _gbitcrsIntelligentTieringConfiguration ::
      !( Maybe
           IntelligentTieringConfiguration
       ),
    _gbitcrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbitcrsIntelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
--
-- * 'gbitcrsResponseStatus' - -- | The response status code.
getBucketIntelligentTieringConfigurationResponse ::
  -- | 'gbitcrsResponseStatus'
  Int ->
  GetBucketIntelligentTieringConfigurationResponse
getBucketIntelligentTieringConfigurationResponse pResponseStatus_ =
  GetBucketIntelligentTieringConfigurationResponse'
    { _gbitcrsIntelligentTieringConfiguration =
        Nothing,
      _gbitcrsResponseStatus = pResponseStatus_
    }

-- | Container for S3 Intelligent-Tiering configuration.
gbitcrsIntelligentTieringConfiguration :: Lens' GetBucketIntelligentTieringConfigurationResponse (Maybe IntelligentTieringConfiguration)
gbitcrsIntelligentTieringConfiguration = lens _gbitcrsIntelligentTieringConfiguration (\s a -> s {_gbitcrsIntelligentTieringConfiguration = a})

-- | -- | The response status code.
gbitcrsResponseStatus :: Lens' GetBucketIntelligentTieringConfigurationResponse Int
gbitcrsResponseStatus = lens _gbitcrsResponseStatus (\s a -> s {_gbitcrsResponseStatus = a})

instance NFData GetBucketIntelligentTieringConfigurationResponse

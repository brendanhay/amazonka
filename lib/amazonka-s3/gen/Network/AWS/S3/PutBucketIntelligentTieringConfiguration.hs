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
-- Module      : Network.AWS.S3.PutBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a S3 Intelligent-Tiering configuration to the specified bucket.
--
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- Operations related to @PutBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.PutBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    putBucketIntelligentTieringConfiguration,
    PutBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    pbitcBucket,
    pbitcId,
    pbitcIntelligentTieringConfiguration,

    -- * Destructuring the Response
    putBucketIntelligentTieringConfigurationResponse,
    PutBucketIntelligentTieringConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketIntelligentTieringConfiguration' smart constructor.
data PutBucketIntelligentTieringConfiguration = PutBucketIntelligentTieringConfiguration'
  { _pbitcBucket ::
      !BucketName,
    _pbitcId ::
      !Text,
    _pbitcIntelligentTieringConfiguration ::
      !IntelligentTieringConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbitcBucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- * 'pbitcId' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- * 'pbitcIntelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
putBucketIntelligentTieringConfiguration ::
  -- | 'pbitcBucket'
  BucketName ->
  -- | 'pbitcId'
  Text ->
  -- | 'pbitcIntelligentTieringConfiguration'
  IntelligentTieringConfiguration ->
  PutBucketIntelligentTieringConfiguration
putBucketIntelligentTieringConfiguration
  pBucket_
  pId_
  pIntelligentTieringConfiguration_ =
    PutBucketIntelligentTieringConfiguration'
      { _pbitcBucket =
          pBucket_,
        _pbitcId = pId_,
        _pbitcIntelligentTieringConfiguration =
          pIntelligentTieringConfiguration_
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
pbitcBucket :: Lens' PutBucketIntelligentTieringConfiguration BucketName
pbitcBucket = lens _pbitcBucket (\s a -> s {_pbitcBucket = a})

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
pbitcId :: Lens' PutBucketIntelligentTieringConfiguration Text
pbitcId = lens _pbitcId (\s a -> s {_pbitcId = a})

-- | Container for S3 Intelligent-Tiering configuration.
pbitcIntelligentTieringConfiguration :: Lens' PutBucketIntelligentTieringConfiguration IntelligentTieringConfiguration
pbitcIntelligentTieringConfiguration = lens _pbitcIntelligentTieringConfiguration (\s a -> s {_pbitcIntelligentTieringConfiguration = a})

instance AWSRequest PutBucketIntelligentTieringConfiguration where
  type
    Rs PutBucketIntelligentTieringConfiguration =
      PutBucketIntelligentTieringConfigurationResponse
  request = putXML s3
  response =
    receiveNull PutBucketIntelligentTieringConfigurationResponse'

instance Hashable PutBucketIntelligentTieringConfiguration

instance NFData PutBucketIntelligentTieringConfiguration

instance ToElement PutBucketIntelligentTieringConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}IntelligentTieringConfiguration"
      . _pbitcIntelligentTieringConfiguration

instance ToHeaders PutBucketIntelligentTieringConfiguration where
  toHeaders = const mempty

instance ToPath PutBucketIntelligentTieringConfiguration where
  toPath PutBucketIntelligentTieringConfiguration' {..} =
    mconcat ["/", toBS _pbitcBucket]

instance ToQuery PutBucketIntelligentTieringConfiguration where
  toQuery PutBucketIntelligentTieringConfiguration' {..} =
    mconcat ["id" =: _pbitcId, "intelligent-tiering"]

-- | /See:/ 'putBucketIntelligentTieringConfigurationResponse' smart constructor.
data PutBucketIntelligentTieringConfigurationResponse = PutBucketIntelligentTieringConfigurationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'PutBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
putBucketIntelligentTieringConfigurationResponse ::
  PutBucketIntelligentTieringConfigurationResponse
putBucketIntelligentTieringConfigurationResponse =
  PutBucketIntelligentTieringConfigurationResponse'

instance NFData PutBucketIntelligentTieringConfigurationResponse

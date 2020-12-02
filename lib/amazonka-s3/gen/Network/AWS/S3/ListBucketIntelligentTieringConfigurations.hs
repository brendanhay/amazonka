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
-- Module      : Network.AWS.S3.ListBucketIntelligentTieringConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the S3 Intelligent-Tiering configuration from the specified bucket.
--
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- Operations related to @ListBucketIntelligentTieringConfigurations@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
module Network.AWS.S3.ListBucketIntelligentTieringConfigurations
  ( -- * Creating a Request
    listBucketIntelligentTieringConfigurations,
    ListBucketIntelligentTieringConfigurations,

    -- * Request Lenses
    lbitcContinuationToken,
    lbitcBucket,

    -- * Destructuring the Response
    listBucketIntelligentTieringConfigurationsResponse,
    ListBucketIntelligentTieringConfigurationsResponse,

    -- * Response Lenses
    lbitcrsIntelligentTieringConfigurationList,
    lbitcrsContinuationToken,
    lbitcrsNextContinuationToken,
    lbitcrsIsTruncated,
    lbitcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listBucketIntelligentTieringConfigurations' smart constructor.
data ListBucketIntelligentTieringConfigurations = ListBucketIntelligentTieringConfigurations'
  { _lbitcContinuationToken ::
      !( Maybe
           Text
       ),
    _lbitcBucket ::
      !BucketName
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListBucketIntelligentTieringConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbitcContinuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
--
-- * 'lbitcBucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
listBucketIntelligentTieringConfigurations ::
  -- | 'lbitcBucket'
  BucketName ->
  ListBucketIntelligentTieringConfigurations
listBucketIntelligentTieringConfigurations pBucket_ =
  ListBucketIntelligentTieringConfigurations'
    { _lbitcContinuationToken =
        Nothing,
      _lbitcBucket = pBucket_
    }

-- | The ContinuationToken that represents a placeholder from where this request should begin.
lbitcContinuationToken :: Lens' ListBucketIntelligentTieringConfigurations (Maybe Text)
lbitcContinuationToken = lens _lbitcContinuationToken (\s a -> s {_lbitcContinuationToken = a})

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
lbitcBucket :: Lens' ListBucketIntelligentTieringConfigurations BucketName
lbitcBucket = lens _lbitcBucket (\s a -> s {_lbitcBucket = a})

instance AWSRequest ListBucketIntelligentTieringConfigurations where
  type
    Rs ListBucketIntelligentTieringConfigurations =
      ListBucketIntelligentTieringConfigurationsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListBucketIntelligentTieringConfigurationsResponse'
            <$> (may (parseXMLList "IntelligentTieringConfiguration") x)
            <*> (x .@? "ContinuationToken")
            <*> (x .@? "NextContinuationToken")
            <*> (x .@? "IsTruncated")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBucketIntelligentTieringConfigurations

instance NFData ListBucketIntelligentTieringConfigurations

instance ToHeaders ListBucketIntelligentTieringConfigurations where
  toHeaders = const mempty

instance ToPath ListBucketIntelligentTieringConfigurations where
  toPath ListBucketIntelligentTieringConfigurations' {..} =
    mconcat ["/", toBS _lbitcBucket]

instance ToQuery ListBucketIntelligentTieringConfigurations where
  toQuery ListBucketIntelligentTieringConfigurations' {..} =
    mconcat
      [ "continuation-token" =: _lbitcContinuationToken,
        "intelligent-tiering"
      ]

-- | /See:/ 'listBucketIntelligentTieringConfigurationsResponse' smart constructor.
data ListBucketIntelligentTieringConfigurationsResponse = ListBucketIntelligentTieringConfigurationsResponse'
  { _lbitcrsIntelligentTieringConfigurationList ::
      !( Maybe
           [IntelligentTieringConfiguration]
       ),
    _lbitcrsContinuationToken ::
      !( Maybe
           Text
       ),
    _lbitcrsNextContinuationToken ::
      !( Maybe
           Text
       ),
    _lbitcrsIsTruncated ::
      !( Maybe
           Bool
       ),
    _lbitcrsResponseStatus ::
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

-- | Creates a value of 'ListBucketIntelligentTieringConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbitcrsIntelligentTieringConfigurationList' - The list of S3 Intelligent-Tiering configurations for a bucket.
--
-- * 'lbitcrsContinuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
--
-- * 'lbitcrsNextContinuationToken' - The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- * 'lbitcrsIsTruncated' - Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- * 'lbitcrsResponseStatus' - -- | The response status code.
listBucketIntelligentTieringConfigurationsResponse ::
  -- | 'lbitcrsResponseStatus'
  Int ->
  ListBucketIntelligentTieringConfigurationsResponse
listBucketIntelligentTieringConfigurationsResponse pResponseStatus_ =
  ListBucketIntelligentTieringConfigurationsResponse'
    { _lbitcrsIntelligentTieringConfigurationList =
        Nothing,
      _lbitcrsContinuationToken = Nothing,
      _lbitcrsNextContinuationToken = Nothing,
      _lbitcrsIsTruncated = Nothing,
      _lbitcrsResponseStatus = pResponseStatus_
    }

-- | The list of S3 Intelligent-Tiering configurations for a bucket.
lbitcrsIntelligentTieringConfigurationList :: Lens' ListBucketIntelligentTieringConfigurationsResponse [IntelligentTieringConfiguration]
lbitcrsIntelligentTieringConfigurationList = lens _lbitcrsIntelligentTieringConfigurationList (\s a -> s {_lbitcrsIntelligentTieringConfigurationList = a}) . _Default . _Coerce

-- | The ContinuationToken that represents a placeholder from where this request should begin.
lbitcrsContinuationToken :: Lens' ListBucketIntelligentTieringConfigurationsResponse (Maybe Text)
lbitcrsContinuationToken = lens _lbitcrsContinuationToken (\s a -> s {_lbitcrsContinuationToken = a})

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
lbitcrsNextContinuationToken :: Lens' ListBucketIntelligentTieringConfigurationsResponse (Maybe Text)
lbitcrsNextContinuationToken = lens _lbitcrsNextContinuationToken (\s a -> s {_lbitcrsNextContinuationToken = a})

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
lbitcrsIsTruncated :: Lens' ListBucketIntelligentTieringConfigurationsResponse (Maybe Bool)
lbitcrsIsTruncated = lens _lbitcrsIsTruncated (\s a -> s {_lbitcrsIsTruncated = a})

-- | -- | The response status code.
lbitcrsResponseStatus :: Lens' ListBucketIntelligentTieringConfigurationsResponse Int
lbitcrsResponseStatus = lens _lbitcrsResponseStatus (\s a -> s {_lbitcrsResponseStatus = a})

instance NFData ListBucketIntelligentTieringConfigurationsResponse

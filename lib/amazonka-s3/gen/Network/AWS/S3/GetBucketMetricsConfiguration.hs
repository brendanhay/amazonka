{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketMetricsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a metrics configuration (specified by the metrics configuration ID) from the bucket.
module Network.AWS.S3.GetBucketMetricsConfiguration
    (
    -- * Creating a Request
      getBucketMetricsConfiguration
    , GetBucketMetricsConfiguration
    -- * Request Lenses
    , gbmcBucket
    , gbmcId

    -- * Destructuring the Response
    , getBucketMetricsConfigurationResponse
    , GetBucketMetricsConfigurationResponse
    -- * Response Lenses
    , gbmcrsMetricsConfiguration
    , gbmcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketMetricsConfiguration' smart constructor.
data GetBucketMetricsConfiguration = GetBucketMetricsConfiguration'
  { _gbmcBucket :: !BucketName
  , _gbmcId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbmcBucket' - The name of the bucket containing the metrics configuration to retrieve.
--
-- * 'gbmcId' - The ID used to identify the metrics configuration.
getBucketMetricsConfiguration
    :: BucketName -- ^ 'gbmcBucket'
    -> Text -- ^ 'gbmcId'
    -> GetBucketMetricsConfiguration
getBucketMetricsConfiguration pBucket_ pId_ =
  GetBucketMetricsConfiguration' {_gbmcBucket = pBucket_, _gbmcId = pId_}


-- | The name of the bucket containing the metrics configuration to retrieve.
gbmcBucket :: Lens' GetBucketMetricsConfiguration BucketName
gbmcBucket = lens _gbmcBucket (\ s a -> s{_gbmcBucket = a})

-- | The ID used to identify the metrics configuration.
gbmcId :: Lens' GetBucketMetricsConfiguration Text
gbmcId = lens _gbmcId (\ s a -> s{_gbmcId = a})

instance AWSRequest GetBucketMetricsConfiguration
         where
        type Rs GetBucketMetricsConfiguration =
             GetBucketMetricsConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketMetricsConfigurationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketMetricsConfiguration where

instance NFData GetBucketMetricsConfiguration where

instance ToHeaders GetBucketMetricsConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketMetricsConfiguration where
        toPath GetBucketMetricsConfiguration'{..}
          = mconcat ["/", toBS _gbmcBucket]

instance ToQuery GetBucketMetricsConfiguration where
        toQuery GetBucketMetricsConfiguration'{..}
          = mconcat ["id" =: _gbmcId, "metrics"]

-- | /See:/ 'getBucketMetricsConfigurationResponse' smart constructor.
data GetBucketMetricsConfigurationResponse = GetBucketMetricsConfigurationResponse'
  { _gbmcrsMetricsConfiguration :: !(Maybe MetricsConfiguration)
  , _gbmcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbmcrsMetricsConfiguration' - Specifies the metrics configuration.
--
-- * 'gbmcrsResponseStatus' - -- | The response status code.
getBucketMetricsConfigurationResponse
    :: Int -- ^ 'gbmcrsResponseStatus'
    -> GetBucketMetricsConfigurationResponse
getBucketMetricsConfigurationResponse pResponseStatus_ =
  GetBucketMetricsConfigurationResponse'
    { _gbmcrsMetricsConfiguration = Nothing
    , _gbmcrsResponseStatus = pResponseStatus_
    }


-- | Specifies the metrics configuration.
gbmcrsMetricsConfiguration :: Lens' GetBucketMetricsConfigurationResponse (Maybe MetricsConfiguration)
gbmcrsMetricsConfiguration = lens _gbmcrsMetricsConfiguration (\ s a -> s{_gbmcrsMetricsConfiguration = a})

-- | -- | The response status code.
gbmcrsResponseStatus :: Lens' GetBucketMetricsConfigurationResponse Int
gbmcrsResponseStatus = lens _gbmcrsResponseStatus (\ s a -> s{_gbmcrsResponseStatus = a})

instance NFData GetBucketMetricsConfigurationResponse
         where

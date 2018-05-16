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
-- Module      : Network.AWS.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an analytics configuration for the bucket (specified by the analytics configuration ID).
module Network.AWS.S3.GetBucketAnalyticsConfiguration
    (
    -- * Creating a Request
      getBucketAnalyticsConfiguration
    , GetBucketAnalyticsConfiguration
    -- * Request Lenses
    , getBucket
    , getId

    -- * Destructuring the Response
    , getBucketAnalyticsConfigurationResponse
    , GetBucketAnalyticsConfigurationResponse
    -- * Response Lenses
    , gbacrsAnalyticsConfiguration
    , gbacrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
  { _getBucket :: !BucketName
  , _getId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getBucket' - The name of the bucket from which an analytics configuration is retrieved.
--
-- * 'getId' - The identifier used to represent an analytics configuration.
getBucketAnalyticsConfiguration
    :: BucketName -- ^ 'getBucket'
    -> Text -- ^ 'getId'
    -> GetBucketAnalyticsConfiguration
getBucketAnalyticsConfiguration pBucket_ pId_ =
  GetBucketAnalyticsConfiguration' {_getBucket = pBucket_, _getId = pId_}


-- | The name of the bucket from which an analytics configuration is retrieved.
getBucket :: Lens' GetBucketAnalyticsConfiguration BucketName
getBucket = lens _getBucket (\ s a -> s{_getBucket = a})

-- | The identifier used to represent an analytics configuration.
getId :: Lens' GetBucketAnalyticsConfiguration Text
getId = lens _getId (\ s a -> s{_getId = a})

instance AWSRequest GetBucketAnalyticsConfiguration
         where
        type Rs GetBucketAnalyticsConfiguration =
             GetBucketAnalyticsConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketAnalyticsConfigurationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketAnalyticsConfiguration
         where

instance NFData GetBucketAnalyticsConfiguration where

instance ToHeaders GetBucketAnalyticsConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketAnalyticsConfiguration where
        toPath GetBucketAnalyticsConfiguration'{..}
          = mconcat ["/", toBS _getBucket]

instance ToQuery GetBucketAnalyticsConfiguration
         where
        toQuery GetBucketAnalyticsConfiguration'{..}
          = mconcat ["id" =: _getId, "analytics"]

-- | /See:/ 'getBucketAnalyticsConfigurationResponse' smart constructor.
data GetBucketAnalyticsConfigurationResponse = GetBucketAnalyticsConfigurationResponse'
  { _gbacrsAnalyticsConfiguration :: !(Maybe AnalyticsConfiguration)
  , _gbacrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbacrsAnalyticsConfiguration' - The configuration and any analyses for the analytics filter.
--
-- * 'gbacrsResponseStatus' - -- | The response status code.
getBucketAnalyticsConfigurationResponse
    :: Int -- ^ 'gbacrsResponseStatus'
    -> GetBucketAnalyticsConfigurationResponse
getBucketAnalyticsConfigurationResponse pResponseStatus_ =
  GetBucketAnalyticsConfigurationResponse'
    { _gbacrsAnalyticsConfiguration = Nothing
    , _gbacrsResponseStatus = pResponseStatus_
    }


-- | The configuration and any analyses for the analytics filter.
gbacrsAnalyticsConfiguration :: Lens' GetBucketAnalyticsConfigurationResponse (Maybe AnalyticsConfiguration)
gbacrsAnalyticsConfiguration = lens _gbacrsAnalyticsConfiguration (\ s a -> s{_gbacrsAnalyticsConfiguration = a})

-- | -- | The response status code.
gbacrsResponseStatus :: Lens' GetBucketAnalyticsConfigurationResponse Int
gbacrsResponseStatus = lens _gbacrsResponseStatus (\ s a -> s{_gbacrsResponseStatus = a})

instance NFData
           GetBucketAnalyticsConfigurationResponse
         where

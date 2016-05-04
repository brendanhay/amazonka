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
-- Module      : Network.AWS.S3.GetBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the accelerate configuration of a bucket.
module Network.AWS.S3.GetBucketAccelerateConfiguration
    (
    -- * Creating a Request
      getBucketAccelerateConfiguration
    , GetBucketAccelerateConfiguration
    -- * Request Lenses
    , gbacBucket

    -- * Destructuring the Response
    , getBucketAccelerateConfigurationResponse
    , GetBucketAccelerateConfigurationResponse
    -- * Response Lenses
    , gbacrsStatus
    , gbacrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketAccelerateConfiguration' smart constructor.
newtype GetBucketAccelerateConfiguration = GetBucketAccelerateConfiguration'
    { _gbacBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbacBucket'
getBucketAccelerateConfiguration
    :: BucketName -- ^ 'gbacBucket'
    -> GetBucketAccelerateConfiguration
getBucketAccelerateConfiguration pBucket_ =
    GetBucketAccelerateConfiguration'
    { _gbacBucket = pBucket_
    }

-- | Name of the bucket for which the accelerate configuration is retrieved.
gbacBucket :: Lens' GetBucketAccelerateConfiguration BucketName
gbacBucket = lens _gbacBucket (\ s a -> s{_gbacBucket = a});

instance AWSRequest GetBucketAccelerateConfiguration
         where
        type Rs GetBucketAccelerateConfiguration =
             GetBucketAccelerateConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketAccelerateConfigurationResponse' <$>
                   (x .@? "Status") <*> (pure (fromEnum s)))

instance Hashable GetBucketAccelerateConfiguration

instance NFData GetBucketAccelerateConfiguration

instance ToHeaders GetBucketAccelerateConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketAccelerateConfiguration
         where
        toPath GetBucketAccelerateConfiguration'{..}
          = mconcat ["/", toBS _gbacBucket]

instance ToQuery GetBucketAccelerateConfiguration
         where
        toQuery = const (mconcat ["accelerate"])

-- | /See:/ 'getBucketAccelerateConfigurationResponse' smart constructor.
data GetBucketAccelerateConfigurationResponse = GetBucketAccelerateConfigurationResponse'
    { _gbacrsStatus         :: !(Maybe BucketAccelerateStatus)
    , _gbacrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbacrsStatus'
--
-- * 'gbacrsResponseStatus'
getBucketAccelerateConfigurationResponse
    :: Int -- ^ 'gbacrsResponseStatus'
    -> GetBucketAccelerateConfigurationResponse
getBucketAccelerateConfigurationResponse pResponseStatus_ =
    GetBucketAccelerateConfigurationResponse'
    { _gbacrsStatus = Nothing
    , _gbacrsResponseStatus = pResponseStatus_
    }

-- | The accelerate configuration of the bucket.
gbacrsStatus :: Lens' GetBucketAccelerateConfigurationResponse (Maybe BucketAccelerateStatus)
gbacrsStatus = lens _gbacrsStatus (\ s a -> s{_gbacrsStatus = a});

-- | The response status code.
gbacrsResponseStatus :: Lens' GetBucketAccelerateConfigurationResponse Int
gbacrsResponseStatus = lens _gbacrsResponseStatus (\ s a -> s{_gbacrsResponseStatus = a});

instance NFData
         GetBucketAccelerateConfigurationResponse

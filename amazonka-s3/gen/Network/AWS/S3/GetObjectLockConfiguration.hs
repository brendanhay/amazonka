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
-- Module      : Network.AWS.S3.GetObjectLockConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Object Lock configuration for a bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket.
--
--
module Network.AWS.S3.GetObjectLockConfiguration
    (
    -- * Creating a Request
      getObjectLockConfiguration
    , GetObjectLockConfiguration
    -- * Request Lenses
    , golcBucket

    -- * Destructuring the Response
    , getObjectLockConfigurationResponse
    , GetObjectLockConfigurationResponse
    -- * Response Lenses
    , golcrsObjectLockConfiguration
    , golcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectLockConfiguration' smart constructor.
newtype GetObjectLockConfiguration = GetObjectLockConfiguration'
  { _golcBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectLockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'golcBucket' - The bucket whose Object Lock configuration you want to retrieve.
getObjectLockConfiguration
    :: BucketName -- ^ 'golcBucket'
    -> GetObjectLockConfiguration
getObjectLockConfiguration pBucket_ =
  GetObjectLockConfiguration' {_golcBucket = pBucket_}


-- | The bucket whose Object Lock configuration you want to retrieve.
golcBucket :: Lens' GetObjectLockConfiguration BucketName
golcBucket = lens _golcBucket (\ s a -> s{_golcBucket = a})

instance AWSRequest GetObjectLockConfiguration where
        type Rs GetObjectLockConfiguration =
             GetObjectLockConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetObjectLockConfigurationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetObjectLockConfiguration where

instance NFData GetObjectLockConfiguration where

instance ToHeaders GetObjectLockConfiguration where
        toHeaders = const mempty

instance ToPath GetObjectLockConfiguration where
        toPath GetObjectLockConfiguration'{..}
          = mconcat ["/", toBS _golcBucket]

instance ToQuery GetObjectLockConfiguration where
        toQuery = const (mconcat ["object-lock"])

-- | /See:/ 'getObjectLockConfigurationResponse' smart constructor.
data GetObjectLockConfigurationResponse = GetObjectLockConfigurationResponse'
  { _golcrsObjectLockConfiguration :: !(Maybe ObjectLockConfiguration)
  , _golcrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectLockConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'golcrsObjectLockConfiguration' - The specified bucket's Object Lock configuration.
--
-- * 'golcrsResponseStatus' - -- | The response status code.
getObjectLockConfigurationResponse
    :: Int -- ^ 'golcrsResponseStatus'
    -> GetObjectLockConfigurationResponse
getObjectLockConfigurationResponse pResponseStatus_ =
  GetObjectLockConfigurationResponse'
    { _golcrsObjectLockConfiguration = Nothing
    , _golcrsResponseStatus = pResponseStatus_
    }


-- | The specified bucket's Object Lock configuration.
golcrsObjectLockConfiguration :: Lens' GetObjectLockConfigurationResponse (Maybe ObjectLockConfiguration)
golcrsObjectLockConfiguration = lens _golcrsObjectLockConfiguration (\ s a -> s{_golcrsObjectLockConfiguration = a})

-- | -- | The response status code.
golcrsResponseStatus :: Lens' GetObjectLockConfigurationResponse Int
golcrsResponseStatus = lens _golcrsResponseStatus (\ s a -> s{_golcrsResponseStatus = a})

instance NFData GetObjectLockConfigurationResponse
         where

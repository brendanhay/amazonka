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
-- Module      : Network.AWS.S3.GetBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.GetBucketLifecycleConfiguration
    (
    -- * Creating a Request
      getBucketLifecycleConfiguration
    , GetBucketLifecycleConfiguration
    -- * Request Lenses
    , gblcBucket

    -- * Destructuring the Response
    , getBucketLifecycleConfigurationResponse
    , GetBucketLifecycleConfigurationResponse
    -- * Response Lenses
    , gblcrsRules
    , gblcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketLifecycleConfiguration' smart constructor.
newtype GetBucketLifecycleConfiguration = GetBucketLifecycleConfiguration'
  { _gblcBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblcBucket' - Undocumented member.
getBucketLifecycleConfiguration
    :: BucketName -- ^ 'gblcBucket'
    -> GetBucketLifecycleConfiguration
getBucketLifecycleConfiguration pBucket_ =
  GetBucketLifecycleConfiguration' {_gblcBucket = pBucket_}


-- | Undocumented member.
gblcBucket :: Lens' GetBucketLifecycleConfiguration BucketName
gblcBucket = lens _gblcBucket (\ s a -> s{_gblcBucket = a})

instance AWSRequest GetBucketLifecycleConfiguration
         where
        type Rs GetBucketLifecycleConfiguration =
             GetBucketLifecycleConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLifecycleConfigurationResponse' <$>
                   (may (parseXMLList "Rule") x) <*>
                     (pure (fromEnum s)))

instance Hashable GetBucketLifecycleConfiguration
         where

instance NFData GetBucketLifecycleConfiguration where

instance ToHeaders GetBucketLifecycleConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketLifecycleConfiguration where
        toPath GetBucketLifecycleConfiguration'{..}
          = mconcat ["/", toBS _gblcBucket]

instance ToQuery GetBucketLifecycleConfiguration
         where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'getBucketLifecycleConfigurationResponse' smart constructor.
data GetBucketLifecycleConfigurationResponse = GetBucketLifecycleConfigurationResponse'
  { _gblcrsRules          :: !(Maybe [LifecycleRule])
  , _gblcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketLifecycleConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblcrsRules' - Undocumented member.
--
-- * 'gblcrsResponseStatus' - -- | The response status code.
getBucketLifecycleConfigurationResponse
    :: Int -- ^ 'gblcrsResponseStatus'
    -> GetBucketLifecycleConfigurationResponse
getBucketLifecycleConfigurationResponse pResponseStatus_ =
  GetBucketLifecycleConfigurationResponse'
    {_gblcrsRules = Nothing, _gblcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gblcrsRules :: Lens' GetBucketLifecycleConfigurationResponse [LifecycleRule]
gblcrsRules = lens _gblcrsRules (\ s a -> s{_gblcrsRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gblcrsResponseStatus :: Lens' GetBucketLifecycleConfigurationResponse Int
gblcrsResponseStatus = lens _gblcrsResponseStatus (\ s a -> s{_gblcrsResponseStatus = a})

instance NFData
           GetBucketLifecycleConfigurationResponse
         where

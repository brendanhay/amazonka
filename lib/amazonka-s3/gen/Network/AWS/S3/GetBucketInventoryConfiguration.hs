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
-- Module      : Network.AWS.S3.GetBucketInventoryConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an inventory configuration (identified by the inventory ID) from the bucket.
module Network.AWS.S3.GetBucketInventoryConfiguration
    (
    -- * Creating a Request
      getBucketInventoryConfiguration
    , GetBucketInventoryConfiguration
    -- * Request Lenses
    , gbicBucket
    , gbicId

    -- * Destructuring the Response
    , getBucketInventoryConfigurationResponse
    , GetBucketInventoryConfigurationResponse
    -- * Response Lenses
    , gbicrsInventoryConfiguration
    , gbicrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketInventoryConfiguration' smart constructor.
data GetBucketInventoryConfiguration = GetBucketInventoryConfiguration'
  { _gbicBucket :: !BucketName
  , _gbicId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbicBucket' - The name of the bucket containing the inventory configuration to retrieve.
--
-- * 'gbicId' - The ID used to identify the inventory configuration.
getBucketInventoryConfiguration
    :: BucketName -- ^ 'gbicBucket'
    -> Text -- ^ 'gbicId'
    -> GetBucketInventoryConfiguration
getBucketInventoryConfiguration pBucket_ pId_ =
  GetBucketInventoryConfiguration' {_gbicBucket = pBucket_, _gbicId = pId_}


-- | The name of the bucket containing the inventory configuration to retrieve.
gbicBucket :: Lens' GetBucketInventoryConfiguration BucketName
gbicBucket = lens _gbicBucket (\ s a -> s{_gbicBucket = a})

-- | The ID used to identify the inventory configuration.
gbicId :: Lens' GetBucketInventoryConfiguration Text
gbicId = lens _gbicId (\ s a -> s{_gbicId = a})

instance AWSRequest GetBucketInventoryConfiguration
         where
        type Rs GetBucketInventoryConfiguration =
             GetBucketInventoryConfigurationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketInventoryConfigurationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketInventoryConfiguration
         where

instance NFData GetBucketInventoryConfiguration where

instance ToHeaders GetBucketInventoryConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketInventoryConfiguration where
        toPath GetBucketInventoryConfiguration'{..}
          = mconcat ["/", toBS _gbicBucket]

instance ToQuery GetBucketInventoryConfiguration
         where
        toQuery GetBucketInventoryConfiguration'{..}
          = mconcat ["id" =: _gbicId, "inventory"]

-- | /See:/ 'getBucketInventoryConfigurationResponse' smart constructor.
data GetBucketInventoryConfigurationResponse = GetBucketInventoryConfigurationResponse'
  { _gbicrsInventoryConfiguration :: !(Maybe InventoryConfiguration)
  , _gbicrsResponseStatus         :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbicrsInventoryConfiguration' - Specifies the inventory configuration.
--
-- * 'gbicrsResponseStatus' - -- | The response status code.
getBucketInventoryConfigurationResponse
    :: Int -- ^ 'gbicrsResponseStatus'
    -> GetBucketInventoryConfigurationResponse
getBucketInventoryConfigurationResponse pResponseStatus_ =
  GetBucketInventoryConfigurationResponse'
    { _gbicrsInventoryConfiguration = Nothing
    , _gbicrsResponseStatus = pResponseStatus_
    }


-- | Specifies the inventory configuration.
gbicrsInventoryConfiguration :: Lens' GetBucketInventoryConfigurationResponse (Maybe InventoryConfiguration)
gbicrsInventoryConfiguration = lens _gbicrsInventoryConfiguration (\ s a -> s{_gbicrsInventoryConfiguration = a})

-- | -- | The response status code.
gbicrsResponseStatus :: Lens' GetBucketInventoryConfigurationResponse Int
gbicrsResponseStatus = lens _gbicrsResponseStatus (\ s a -> s{_gbicrsResponseStatus = a})

instance NFData
           GetBucketInventoryConfigurationResponse
         where

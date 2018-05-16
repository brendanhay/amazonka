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
-- Module      : Network.AWS.S3.PutBucketInventoryConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an inventory configuration (identified by the inventory ID) from the bucket.
module Network.AWS.S3.PutBucketInventoryConfiguration
    (
    -- * Creating a Request
      putBucketInventoryConfiguration
    , PutBucketInventoryConfiguration
    -- * Request Lenses
    , pbicBucket
    , pbicId
    , pbicInventoryConfiguration

    -- * Destructuring the Response
    , putBucketInventoryConfigurationResponse
    , PutBucketInventoryConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketInventoryConfiguration' smart constructor.
data PutBucketInventoryConfiguration = PutBucketInventoryConfiguration'
  { _pbicBucket                 :: !BucketName
  , _pbicId                     :: !Text
  , _pbicInventoryConfiguration :: !InventoryConfiguration
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbicBucket' - The name of the bucket where the inventory configuration will be stored.
--
-- * 'pbicId' - The ID used to identify the inventory configuration.
--
-- * 'pbicInventoryConfiguration' - Specifies the inventory configuration.
putBucketInventoryConfiguration
    :: BucketName -- ^ 'pbicBucket'
    -> Text -- ^ 'pbicId'
    -> InventoryConfiguration -- ^ 'pbicInventoryConfiguration'
    -> PutBucketInventoryConfiguration
putBucketInventoryConfiguration pBucket_ pId_ pInventoryConfiguration_ =
  PutBucketInventoryConfiguration'
    { _pbicBucket = pBucket_
    , _pbicId = pId_
    , _pbicInventoryConfiguration = pInventoryConfiguration_
    }


-- | The name of the bucket where the inventory configuration will be stored.
pbicBucket :: Lens' PutBucketInventoryConfiguration BucketName
pbicBucket = lens _pbicBucket (\ s a -> s{_pbicBucket = a})

-- | The ID used to identify the inventory configuration.
pbicId :: Lens' PutBucketInventoryConfiguration Text
pbicId = lens _pbicId (\ s a -> s{_pbicId = a})

-- | Specifies the inventory configuration.
pbicInventoryConfiguration :: Lens' PutBucketInventoryConfiguration InventoryConfiguration
pbicInventoryConfiguration = lens _pbicInventoryConfiguration (\ s a -> s{_pbicInventoryConfiguration = a})

instance AWSRequest PutBucketInventoryConfiguration
         where
        type Rs PutBucketInventoryConfiguration =
             PutBucketInventoryConfigurationResponse
        request = putXML s3
        response
          = receiveNull
              PutBucketInventoryConfigurationResponse'

instance Hashable PutBucketInventoryConfiguration
         where

instance NFData PutBucketInventoryConfiguration where

instance ToElement PutBucketInventoryConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}InventoryConfiguration"
              .
              _pbicInventoryConfiguration

instance ToHeaders PutBucketInventoryConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketInventoryConfiguration where
        toPath PutBucketInventoryConfiguration'{..}
          = mconcat ["/", toBS _pbicBucket]

instance ToQuery PutBucketInventoryConfiguration
         where
        toQuery PutBucketInventoryConfiguration'{..}
          = mconcat ["id" =: _pbicId, "inventory"]

-- | /See:/ 'putBucketInventoryConfigurationResponse' smart constructor.
data PutBucketInventoryConfigurationResponse =
  PutBucketInventoryConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
--
putBucketInventoryConfigurationResponse
    :: PutBucketInventoryConfigurationResponse
putBucketInventoryConfigurationResponse =
  PutBucketInventoryConfigurationResponse'


instance NFData
           PutBucketInventoryConfigurationResponse
         where

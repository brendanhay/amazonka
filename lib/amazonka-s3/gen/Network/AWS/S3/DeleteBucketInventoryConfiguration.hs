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
-- Module      : Network.AWS.S3.DeleteBucketInventoryConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inventory configuration (identified by the inventory ID) from the bucket.
module Network.AWS.S3.DeleteBucketInventoryConfiguration
    (
    -- * Creating a Request
      deleteBucketInventoryConfiguration
    , DeleteBucketInventoryConfiguration
    -- * Request Lenses
    , dbicBucket
    , dbicId

    -- * Destructuring the Response
    , deleteBucketInventoryConfigurationResponse
    , DeleteBucketInventoryConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketInventoryConfiguration' smart constructor.
data DeleteBucketInventoryConfiguration = DeleteBucketInventoryConfiguration'
  { _dbicBucket :: !BucketName
  , _dbicId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbicBucket' - The name of the bucket containing the inventory configuration to delete.
--
-- * 'dbicId' - The ID used to identify the inventory configuration.
deleteBucketInventoryConfiguration
    :: BucketName -- ^ 'dbicBucket'
    -> Text -- ^ 'dbicId'
    -> DeleteBucketInventoryConfiguration
deleteBucketInventoryConfiguration pBucket_ pId_ =
  DeleteBucketInventoryConfiguration' {_dbicBucket = pBucket_, _dbicId = pId_}


-- | The name of the bucket containing the inventory configuration to delete.
dbicBucket :: Lens' DeleteBucketInventoryConfiguration BucketName
dbicBucket = lens _dbicBucket (\ s a -> s{_dbicBucket = a})

-- | The ID used to identify the inventory configuration.
dbicId :: Lens' DeleteBucketInventoryConfiguration Text
dbicId = lens _dbicId (\ s a -> s{_dbicId = a})

instance AWSRequest
           DeleteBucketInventoryConfiguration
         where
        type Rs DeleteBucketInventoryConfiguration =
             DeleteBucketInventoryConfigurationResponse
        request = delete s3
        response
          = receiveNull
              DeleteBucketInventoryConfigurationResponse'

instance Hashable DeleteBucketInventoryConfiguration
         where

instance NFData DeleteBucketInventoryConfiguration
         where

instance ToHeaders DeleteBucketInventoryConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteBucketInventoryConfiguration
         where
        toPath DeleteBucketInventoryConfiguration'{..}
          = mconcat ["/", toBS _dbicBucket]

instance ToQuery DeleteBucketInventoryConfiguration
         where
        toQuery DeleteBucketInventoryConfiguration'{..}
          = mconcat ["id" =: _dbicId, "inventory"]

-- | /See:/ 'deleteBucketInventoryConfigurationResponse' smart constructor.
data DeleteBucketInventoryConfigurationResponse =
  DeleteBucketInventoryConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
--
deleteBucketInventoryConfigurationResponse
    :: DeleteBucketInventoryConfigurationResponse
deleteBucketInventoryConfigurationResponse =
  DeleteBucketInventoryConfigurationResponse'


instance NFData
           DeleteBucketInventoryConfigurationResponse
         where

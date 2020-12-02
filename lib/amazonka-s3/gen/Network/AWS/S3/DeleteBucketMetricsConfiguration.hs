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
-- Module      : Network.AWS.S3.DeleteBucketMetricsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metrics configuration (specified by the metrics configuration ID) from the bucket.
module Network.AWS.S3.DeleteBucketMetricsConfiguration
    (
    -- * Creating a Request
      deleteBucketMetricsConfiguration
    , DeleteBucketMetricsConfiguration
    -- * Request Lenses
    , dbmcBucket
    , dbmcId

    -- * Destructuring the Response
    , deleteBucketMetricsConfigurationResponse
    , DeleteBucketMetricsConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketMetricsConfiguration' smart constructor.
data DeleteBucketMetricsConfiguration = DeleteBucketMetricsConfiguration'
  { _dbmcBucket :: !BucketName
  , _dbmcId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbmcBucket' - The name of the bucket containing the metrics configuration to delete.
--
-- * 'dbmcId' - The ID used to identify the metrics configuration.
deleteBucketMetricsConfiguration
    :: BucketName -- ^ 'dbmcBucket'
    -> Text -- ^ 'dbmcId'
    -> DeleteBucketMetricsConfiguration
deleteBucketMetricsConfiguration pBucket_ pId_ =
  DeleteBucketMetricsConfiguration' {_dbmcBucket = pBucket_, _dbmcId = pId_}


-- | The name of the bucket containing the metrics configuration to delete.
dbmcBucket :: Lens' DeleteBucketMetricsConfiguration BucketName
dbmcBucket = lens _dbmcBucket (\ s a -> s{_dbmcBucket = a})

-- | The ID used to identify the metrics configuration.
dbmcId :: Lens' DeleteBucketMetricsConfiguration Text
dbmcId = lens _dbmcId (\ s a -> s{_dbmcId = a})

instance AWSRequest DeleteBucketMetricsConfiguration
         where
        type Rs DeleteBucketMetricsConfiguration =
             DeleteBucketMetricsConfigurationResponse
        request = delete s3
        response
          = receiveNull
              DeleteBucketMetricsConfigurationResponse'

instance Hashable DeleteBucketMetricsConfiguration
         where

instance NFData DeleteBucketMetricsConfiguration
         where

instance ToHeaders DeleteBucketMetricsConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteBucketMetricsConfiguration
         where
        toPath DeleteBucketMetricsConfiguration'{..}
          = mconcat ["/", toBS _dbmcBucket]

instance ToQuery DeleteBucketMetricsConfiguration
         where
        toQuery DeleteBucketMetricsConfiguration'{..}
          = mconcat ["id" =: _dbmcId, "metrics"]

-- | /See:/ 'deleteBucketMetricsConfigurationResponse' smart constructor.
data DeleteBucketMetricsConfigurationResponse =
  DeleteBucketMetricsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
--
deleteBucketMetricsConfigurationResponse
    :: DeleteBucketMetricsConfigurationResponse
deleteBucketMetricsConfigurationResponse =
  DeleteBucketMetricsConfigurationResponse'


instance NFData
           DeleteBucketMetricsConfigurationResponse
         where

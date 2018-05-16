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
-- Module      : Network.AWS.S3.DeleteBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
module Network.AWS.S3.DeleteBucketAnalyticsConfiguration
    (
    -- * Creating a Request
      deleteBucketAnalyticsConfiguration
    , DeleteBucketAnalyticsConfiguration
    -- * Request Lenses
    , dbacBucket
    , dbacId

    -- * Destructuring the Response
    , deleteBucketAnalyticsConfigurationResponse
    , DeleteBucketAnalyticsConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketAnalyticsConfiguration' smart constructor.
data DeleteBucketAnalyticsConfiguration = DeleteBucketAnalyticsConfiguration'
  { _dbacBucket :: !BucketName
  , _dbacId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbacBucket' - The name of the bucket from which an analytics configuration is deleted.
--
-- * 'dbacId' - The identifier used to represent an analytics configuration.
deleteBucketAnalyticsConfiguration
    :: BucketName -- ^ 'dbacBucket'
    -> Text -- ^ 'dbacId'
    -> DeleteBucketAnalyticsConfiguration
deleteBucketAnalyticsConfiguration pBucket_ pId_ =
  DeleteBucketAnalyticsConfiguration' {_dbacBucket = pBucket_, _dbacId = pId_}


-- | The name of the bucket from which an analytics configuration is deleted.
dbacBucket :: Lens' DeleteBucketAnalyticsConfiguration BucketName
dbacBucket = lens _dbacBucket (\ s a -> s{_dbacBucket = a})

-- | The identifier used to represent an analytics configuration.
dbacId :: Lens' DeleteBucketAnalyticsConfiguration Text
dbacId = lens _dbacId (\ s a -> s{_dbacId = a})

instance AWSRequest
           DeleteBucketAnalyticsConfiguration
         where
        type Rs DeleteBucketAnalyticsConfiguration =
             DeleteBucketAnalyticsConfigurationResponse
        request = delete s3
        response
          = receiveNull
              DeleteBucketAnalyticsConfigurationResponse'

instance Hashable DeleteBucketAnalyticsConfiguration
         where

instance NFData DeleteBucketAnalyticsConfiguration
         where

instance ToHeaders DeleteBucketAnalyticsConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteBucketAnalyticsConfiguration
         where
        toPath DeleteBucketAnalyticsConfiguration'{..}
          = mconcat ["/", toBS _dbacBucket]

instance ToQuery DeleteBucketAnalyticsConfiguration
         where
        toQuery DeleteBucketAnalyticsConfiguration'{..}
          = mconcat ["id" =: _dbacId, "analytics"]

-- | /See:/ 'deleteBucketAnalyticsConfigurationResponse' smart constructor.
data DeleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
--
deleteBucketAnalyticsConfigurationResponse
    :: DeleteBucketAnalyticsConfigurationResponse
deleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'


instance NFData
           DeleteBucketAnalyticsConfigurationResponse
         where

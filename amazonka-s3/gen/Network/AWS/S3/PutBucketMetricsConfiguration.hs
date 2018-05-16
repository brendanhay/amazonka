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
-- Module      : Network.AWS.S3.PutBucketMetricsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a metrics configuration (specified by the metrics configuration ID) for the bucket.
module Network.AWS.S3.PutBucketMetricsConfiguration
    (
    -- * Creating a Request
      putBucketMetricsConfiguration
    , PutBucketMetricsConfiguration
    -- * Request Lenses
    , pbmcBucket
    , pbmcId
    , pbmcMetricsConfiguration

    -- * Destructuring the Response
    , putBucketMetricsConfigurationResponse
    , PutBucketMetricsConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketMetricsConfiguration' smart constructor.
data PutBucketMetricsConfiguration = PutBucketMetricsConfiguration'
  { _pbmcBucket               :: !BucketName
  , _pbmcId                   :: !Text
  , _pbmcMetricsConfiguration :: !MetricsConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbmcBucket' - The name of the bucket for which the metrics configuration is set.
--
-- * 'pbmcId' - The ID used to identify the metrics configuration.
--
-- * 'pbmcMetricsConfiguration' - Specifies the metrics configuration.
putBucketMetricsConfiguration
    :: BucketName -- ^ 'pbmcBucket'
    -> Text -- ^ 'pbmcId'
    -> MetricsConfiguration -- ^ 'pbmcMetricsConfiguration'
    -> PutBucketMetricsConfiguration
putBucketMetricsConfiguration pBucket_ pId_ pMetricsConfiguration_ =
  PutBucketMetricsConfiguration'
    { _pbmcBucket = pBucket_
    , _pbmcId = pId_
    , _pbmcMetricsConfiguration = pMetricsConfiguration_
    }


-- | The name of the bucket for which the metrics configuration is set.
pbmcBucket :: Lens' PutBucketMetricsConfiguration BucketName
pbmcBucket = lens _pbmcBucket (\ s a -> s{_pbmcBucket = a})

-- | The ID used to identify the metrics configuration.
pbmcId :: Lens' PutBucketMetricsConfiguration Text
pbmcId = lens _pbmcId (\ s a -> s{_pbmcId = a})

-- | Specifies the metrics configuration.
pbmcMetricsConfiguration :: Lens' PutBucketMetricsConfiguration MetricsConfiguration
pbmcMetricsConfiguration = lens _pbmcMetricsConfiguration (\ s a -> s{_pbmcMetricsConfiguration = a})

instance AWSRequest PutBucketMetricsConfiguration
         where
        type Rs PutBucketMetricsConfiguration =
             PutBucketMetricsConfigurationResponse
        request = putXML s3
        response
          = receiveNull PutBucketMetricsConfigurationResponse'

instance Hashable PutBucketMetricsConfiguration where

instance NFData PutBucketMetricsConfiguration where

instance ToElement PutBucketMetricsConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}MetricsConfiguration"
              .
              _pbmcMetricsConfiguration

instance ToHeaders PutBucketMetricsConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketMetricsConfiguration where
        toPath PutBucketMetricsConfiguration'{..}
          = mconcat ["/", toBS _pbmcBucket]

instance ToQuery PutBucketMetricsConfiguration where
        toQuery PutBucketMetricsConfiguration'{..}
          = mconcat ["id" =: _pbmcId, "metrics"]

-- | /See:/ 'putBucketMetricsConfigurationResponse' smart constructor.
data PutBucketMetricsConfigurationResponse =
  PutBucketMetricsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
--
putBucketMetricsConfigurationResponse
    :: PutBucketMetricsConfigurationResponse
putBucketMetricsConfigurationResponse = PutBucketMetricsConfigurationResponse'


instance NFData PutBucketMetricsConfigurationResponse
         where

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
-- Module      : Network.AWS.S3.PutBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an analytics configuration for the bucket (specified by the analytics configuration ID).
module Network.AWS.S3.PutBucketAnalyticsConfiguration
    (
    -- * Creating a Request
      putBucketAnalyticsConfiguration
    , PutBucketAnalyticsConfiguration
    -- * Request Lenses
    , pBucket
    , pId
    , pAnalyticsConfiguration

    -- * Destructuring the Response
    , putBucketAnalyticsConfigurationResponse
    , PutBucketAnalyticsConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketAnalyticsConfiguration' smart constructor.
data PutBucketAnalyticsConfiguration = PutBucketAnalyticsConfiguration'
  { _pBucket                 :: !BucketName
  , _pId                     :: !Text
  , _pAnalyticsConfiguration :: !AnalyticsConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pBucket' - The name of the bucket to which an analytics configuration is stored.
--
-- * 'pId' - The identifier used to represent an analytics configuration.
--
-- * 'pAnalyticsConfiguration' - The configuration and any analyses for the analytics filter.
putBucketAnalyticsConfiguration
    :: BucketName -- ^ 'pBucket'
    -> Text -- ^ 'pId'
    -> AnalyticsConfiguration -- ^ 'pAnalyticsConfiguration'
    -> PutBucketAnalyticsConfiguration
putBucketAnalyticsConfiguration pBucket_ pId_ pAnalyticsConfiguration_ =
  PutBucketAnalyticsConfiguration'
    { _pBucket = pBucket_
    , _pId = pId_
    , _pAnalyticsConfiguration = pAnalyticsConfiguration_
    }


-- | The name of the bucket to which an analytics configuration is stored.
pBucket :: Lens' PutBucketAnalyticsConfiguration BucketName
pBucket = lens _pBucket (\ s a -> s{_pBucket = a})

-- | The identifier used to represent an analytics configuration.
pId :: Lens' PutBucketAnalyticsConfiguration Text
pId = lens _pId (\ s a -> s{_pId = a})

-- | The configuration and any analyses for the analytics filter.
pAnalyticsConfiguration :: Lens' PutBucketAnalyticsConfiguration AnalyticsConfiguration
pAnalyticsConfiguration = lens _pAnalyticsConfiguration (\ s a -> s{_pAnalyticsConfiguration = a})

instance AWSRequest PutBucketAnalyticsConfiguration
         where
        type Rs PutBucketAnalyticsConfiguration =
             PutBucketAnalyticsConfigurationResponse
        request = putXML s3
        response
          = receiveNull
              PutBucketAnalyticsConfigurationResponse'

instance Hashable PutBucketAnalyticsConfiguration
         where

instance NFData PutBucketAnalyticsConfiguration where

instance ToElement PutBucketAnalyticsConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}AnalyticsConfiguration"
              .
              _pAnalyticsConfiguration

instance ToHeaders PutBucketAnalyticsConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketAnalyticsConfiguration where
        toPath PutBucketAnalyticsConfiguration'{..}
          = mconcat ["/", toBS _pBucket]

instance ToQuery PutBucketAnalyticsConfiguration
         where
        toQuery PutBucketAnalyticsConfiguration'{..}
          = mconcat ["id" =: _pId, "analytics"]

-- | /See:/ 'putBucketAnalyticsConfigurationResponse' smart constructor.
data PutBucketAnalyticsConfigurationResponse =
  PutBucketAnalyticsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
--
putBucketAnalyticsConfigurationResponse
    :: PutBucketAnalyticsConfigurationResponse
putBucketAnalyticsConfigurationResponse =
  PutBucketAnalyticsConfigurationResponse'


instance NFData
           PutBucketAnalyticsConfigurationResponse
         where

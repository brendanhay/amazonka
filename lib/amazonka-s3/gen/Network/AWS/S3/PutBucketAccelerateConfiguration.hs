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
-- Module      : Network.AWS.S3.PutBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the accelerate configuration of an existing bucket.
module Network.AWS.S3.PutBucketAccelerateConfiguration
    (
    -- * Creating a Request
      putBucketAccelerateConfiguration
    , PutBucketAccelerateConfiguration
    -- * Request Lenses
    , pbacBucket
    , pbacAccelerateConfiguration

    -- * Destructuring the Response
    , putBucketAccelerateConfigurationResponse
    , PutBucketAccelerateConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketAccelerateConfiguration' smart constructor.
data PutBucketAccelerateConfiguration = PutBucketAccelerateConfiguration'
  { _pbacBucket                  :: !BucketName
  , _pbacAccelerateConfiguration :: !AccelerateConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbacBucket' - Name of the bucket for which the accelerate configuration is set.
--
-- * 'pbacAccelerateConfiguration' - Specifies the Accelerate Configuration you want to set for the bucket.
putBucketAccelerateConfiguration
    :: BucketName -- ^ 'pbacBucket'
    -> AccelerateConfiguration -- ^ 'pbacAccelerateConfiguration'
    -> PutBucketAccelerateConfiguration
putBucketAccelerateConfiguration pBucket_ pAccelerateConfiguration_ =
  PutBucketAccelerateConfiguration'
    { _pbacBucket = pBucket_
    , _pbacAccelerateConfiguration = pAccelerateConfiguration_
    }


-- | Name of the bucket for which the accelerate configuration is set.
pbacBucket :: Lens' PutBucketAccelerateConfiguration BucketName
pbacBucket = lens _pbacBucket (\ s a -> s{_pbacBucket = a})

-- | Specifies the Accelerate Configuration you want to set for the bucket.
pbacAccelerateConfiguration :: Lens' PutBucketAccelerateConfiguration AccelerateConfiguration
pbacAccelerateConfiguration = lens _pbacAccelerateConfiguration (\ s a -> s{_pbacAccelerateConfiguration = a})

instance AWSRequest PutBucketAccelerateConfiguration
         where
        type Rs PutBucketAccelerateConfiguration =
             PutBucketAccelerateConfigurationResponse
        request = putXML s3
        response
          = receiveNull
              PutBucketAccelerateConfigurationResponse'

instance Hashable PutBucketAccelerateConfiguration
         where

instance NFData PutBucketAccelerateConfiguration
         where

instance ToElement PutBucketAccelerateConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}AccelerateConfiguration"
              .
              _pbacAccelerateConfiguration

instance ToHeaders PutBucketAccelerateConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketAccelerateConfiguration
         where
        toPath PutBucketAccelerateConfiguration'{..}
          = mconcat ["/", toBS _pbacBucket]

instance ToQuery PutBucketAccelerateConfiguration
         where
        toQuery = const (mconcat ["accelerate"])

-- | /See:/ 'putBucketAccelerateConfigurationResponse' smart constructor.
data PutBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
--
putBucketAccelerateConfigurationResponse
    :: PutBucketAccelerateConfigurationResponse
putBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'


instance NFData
           PutBucketAccelerateConfigurationResponse
         where

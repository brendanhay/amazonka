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
-- Module      : Network.AWS.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables notifications of specified events for a bucket.
module Network.AWS.S3.PutBucketNotificationConfiguration
    (
    -- * Creating a Request
      putBucketNotificationConfiguration
    , PutBucketNotificationConfiguration
    -- * Request Lenses
    , pbncBucket
    , pbncNotificationConfiguration

    -- * Destructuring the Response
    , putBucketNotificationConfigurationResponse
    , PutBucketNotificationConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { _pbncBucket                    :: !BucketName
  , _pbncNotificationConfiguration :: !NotificationConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbncBucket' - Undocumented member.
--
-- * 'pbncNotificationConfiguration' - Undocumented member.
putBucketNotificationConfiguration
    :: BucketName -- ^ 'pbncBucket'
    -> NotificationConfiguration -- ^ 'pbncNotificationConfiguration'
    -> PutBucketNotificationConfiguration
putBucketNotificationConfiguration pBucket_ pNotificationConfiguration_ =
  PutBucketNotificationConfiguration'
    { _pbncBucket = pBucket_
    , _pbncNotificationConfiguration = pNotificationConfiguration_
    }


-- | Undocumented member.
pbncBucket :: Lens' PutBucketNotificationConfiguration BucketName
pbncBucket = lens _pbncBucket (\ s a -> s{_pbncBucket = a})

-- | Undocumented member.
pbncNotificationConfiguration :: Lens' PutBucketNotificationConfiguration NotificationConfiguration
pbncNotificationConfiguration = lens _pbncNotificationConfiguration (\ s a -> s{_pbncNotificationConfiguration = a})

instance AWSRequest
           PutBucketNotificationConfiguration
         where
        type Rs PutBucketNotificationConfiguration =
             PutBucketNotificationConfigurationResponse
        request = putXML s3
        response
          = receiveNull
              PutBucketNotificationConfigurationResponse'

instance Hashable PutBucketNotificationConfiguration
         where

instance NFData PutBucketNotificationConfiguration
         where

instance ToElement PutBucketNotificationConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}NotificationConfiguration"
              .
              _pbncNotificationConfiguration

instance ToHeaders PutBucketNotificationConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketNotificationConfiguration
         where
        toPath PutBucketNotificationConfiguration'{..}
          = mconcat ["/", toBS _pbncBucket]

instance ToQuery PutBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])

-- | /See:/ 'putBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse =
  PutBucketNotificationConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketNotificationConfigurationResponse' with the minimum fields required to make a request.
--
putBucketNotificationConfigurationResponse
    :: PutBucketNotificationConfigurationResponse
putBucketNotificationConfigurationResponse =
  PutBucketNotificationConfigurationResponse'


instance NFData
           PutBucketNotificationConfigurationResponse
         where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables notifications of specified events for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketNotificationConfiguration.html>
module Network.AWS.S3.PutBucketNotificationConfiguration
    (
    -- * Request
      PutBucketNotificationConfiguration
    -- ** Request constructor
    , putBucketNotificationConfiguration
    -- ** Request lenses
    , pbncBucket
    , pbncNotificationConfiguration

    -- * Response
    , PutBucketNotificationConfigurationResponse
    -- ** Response constructor
    , putBucketNotificationConfigurationResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketNotificationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbncBucket'
--
-- * 'pbncNotificationConfiguration'
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
    { _pbncBucket                    :: !BucketName
    , _pbncNotificationConfiguration :: !NotificationConfiguration
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketNotificationConfiguration' smart constructor.
putBucketNotificationConfiguration :: BucketName -> NotificationConfiguration -> PutBucketNotificationConfiguration
putBucketNotificationConfiguration pBucket pNotificationConfiguration =
    PutBucketNotificationConfiguration'
    { _pbncBucket = pBucket
    , _pbncNotificationConfiguration = pNotificationConfiguration
    }

-- | FIXME: Undocumented member.
pbncBucket :: Lens' PutBucketNotificationConfiguration BucketName
pbncBucket = lens _pbncBucket (\ s a -> s{_pbncBucket = a});

-- | FIXME: Undocumented member.
pbncNotificationConfiguration :: Lens' PutBucketNotificationConfiguration NotificationConfiguration
pbncNotificationConfiguration = lens _pbncNotificationConfiguration (\ s a -> s{_pbncNotificationConfiguration = a});

instance AWSRequest
         PutBucketNotificationConfiguration where
        type Sv PutBucketNotificationConfiguration = S3
        type Rs PutBucketNotificationConfiguration =
             PutBucketNotificationConfigurationResponse
        request = putXML
        response
          = receiveNull
              PutBucketNotificationConfigurationResponse'

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
          = mconcat ["/", toText _pbncBucket]

instance ToQuery PutBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])

-- | /See:/ 'putBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse =
    PutBucketNotificationConfigurationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketNotificationConfigurationResponse' smart constructor.
putBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse
putBucketNotificationConfigurationResponse =
    PutBucketNotificationConfigurationResponse'

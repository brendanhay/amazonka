{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables notifications of specified events for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketNotification
    (
    -- * Request
      PutBucketNotification
    -- ** Request constructor
    , mkPutBucketNotificationRequest
    -- ** Request lenses
    , pbnrBucket
    , pbnrContentMD5
    , pbnrNotificationConfiguration

    -- * Response
    , PutBucketNotificationResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketNotification' request.
mkPutBucketNotificationRequest :: BucketName -- ^ 'pbnrBucket'
                               -> NotificationConfiguration -- ^ 'pbnrNotificationConfiguration'
                               -> PutBucketNotification
mkPutBucketNotificationRequest p1 p2 = PutBucketNotification
    { _pbnrBucket = p1
    , _pbnrContentMD5 = Nothing
    , _pbnrNotificationConfiguration = p3
    }
{-# INLINE mkPutBucketNotificationRequest #-}

data PutBucketNotification = PutBucketNotification
    { _pbnrBucket :: BucketName
    , _pbnrContentMD5 :: Maybe Text
    , _pbnrNotificationConfiguration :: NotificationConfiguration
    } deriving (Show, Generic)

pbnrBucket :: Lens' PutBucketNotification (BucketName)
pbnrBucket = lens _pbnrBucket (\s a -> s { _pbnrBucket = a })
{-# INLINE pbnrBucket #-}

pbnrContentMD5 :: Lens' PutBucketNotification (Maybe Text)
pbnrContentMD5 = lens _pbnrContentMD5 (\s a -> s { _pbnrContentMD5 = a })
{-# INLINE pbnrContentMD5 #-}

pbnrNotificationConfiguration :: Lens' PutBucketNotification (NotificationConfiguration)
pbnrNotificationConfiguration = lens _pbnrNotificationConfiguration (\s a -> s { _pbnrNotificationConfiguration = a })
{-# INLINE pbnrNotificationConfiguration #-}

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = mconcat
        [ "/"
        , toBS _pbnrBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery PutBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders PutBucketNotification

instance ToBody PutBucketNotification

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = PutBucketNotificationResponse

    request = put
    response _ = nullaryResponse PutBucketNotificationResponse

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
    , putBucketNotification
    -- ** Request lenses
    , pbnrNotificationConfiguration
    , pbnrBucket
    , pbnrContentMD5

    -- * Response
    , PutBucketNotificationResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketNotification' request.
putBucketNotification :: NotificationConfiguration -- ^ 'pbnrNotificationConfiguration'
                      -> BucketName -- ^ 'pbnrBucket'
                      -> PutBucketNotification
putBucketNotification p1 p2 = PutBucketNotification
    { _pbnrNotificationConfiguration = p1
    , _pbnrBucket = p2
    , _pbnrContentMD5 = Nothing
    }

data PutBucketNotification = PutBucketNotification
    { _pbnrNotificationConfiguration :: NotificationConfiguration
    , _pbnrBucket :: BucketName
    , _pbnrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pbnrNotificationConfiguration
    :: Functor f
    => (NotificationConfiguration
    -> f (NotificationConfiguration))
    -> PutBucketNotification
    -> f PutBucketNotification
pbnrNotificationConfiguration f x =
    (\y -> x { _pbnrNotificationConfiguration = y })
       <$> f (_pbnrNotificationConfiguration x)
{-# INLINE pbnrNotificationConfiguration #-}

pbnrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> PutBucketNotification
    -> f PutBucketNotification
pbnrBucket f x =
    (\y -> x { _pbnrBucket = y })
       <$> f (_pbnrBucket x)
{-# INLINE pbnrBucket #-}

pbnrContentMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutBucketNotification
    -> f PutBucketNotification
pbnrContentMD5 f x =
    (\y -> x { _pbnrContentMD5 = y })
       <$> f (_pbnrContentMD5 x)
{-# INLINE pbnrContentMD5 #-}

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = mconcat
        [ "/"
        , toBS _pbnrBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery PutBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} = concat
        [ "Content-MD5" =: _pbnrContentMD5
        ]

instance ToBody PutBucketNotification where
    toBody = toBody . encodeXML . _pbnrNotificationConfiguration

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = PutBucketNotificationResponse

    request = put
    response _ = nullaryResponse PutBucketNotificationResponse

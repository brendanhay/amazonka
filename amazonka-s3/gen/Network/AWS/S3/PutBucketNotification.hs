{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables notifications of specified events for a bucket.
module Network.AWS.S3.PutBucketNotification
    (
    -- * Request
      PutBucketNotification
    -- ** Request constructor
    , putBucketNotification
    -- ** Request lenses
    , pbnrBucket
    , pbnrContentMD5
    , pbnrNotificationConfiguration

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data PutBucketNotification = PutBucketNotification
    { _pbnrBucket                    :: BucketName
    , _pbnrContentMD5                :: Maybe Text
    , _pbnrNotificationConfiguration :: NotificationConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketNotification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbnrBucket' @::@ 'BucketName'
--
-- * 'pbnrContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbnrNotificationConfiguration' @::@ 'NotificationConfiguration'
--
putBucketNotification :: BucketName -- ^ 'pbnrBucket'
                      -> NotificationConfiguration -- ^ 'pbnrNotificationConfiguration'
                      -> PutBucketNotification
putBucketNotification p1 p2 = PutBucketNotification
    { _pbnrBucket                    = p1
    , _pbnrNotificationConfiguration = p2
    , _pbnrContentMD5                = Nothing
    }

pbnrBucket :: Lens' PutBucketNotification BucketName
pbnrBucket = lens _pbnrBucket (\s a -> s { _pbnrBucket = a })

pbnrContentMD5 :: Lens' PutBucketNotification (Maybe Text)
pbnrContentMD5 = lens _pbnrContentMD5 (\s a -> s { _pbnrContentMD5 = a })

pbnrNotificationConfiguration :: Lens' PutBucketNotification NotificationConfiguration
pbnrNotificationConfiguration =
    lens _pbnrNotificationConfiguration
        (\s a -> s { _pbnrNotificationConfiguration = a })

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = mconcat
        [ "/"
        , toText _pbnrBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery = const "notification"

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} = mconcat
        [ "Content-MD5" =: _pbnrContentMD5
        ]

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = Empty

    request  = put
    response = const (nullaryResponse Empty)

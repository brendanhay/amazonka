{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketNotification
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketNotification.html>
module Network.AWS.S3.PutBucketNotification
    (
    -- * Request
      PutBucketNotification
    -- ** Request constructor
    , putBucketNotification
    -- ** Request lenses
    , pbnBucket
    , pbnContentMD5
    , pbnNotificationConfiguration

    -- * Response
    , PutBucketNotificationResponse
    -- ** Response constructor
    , putBucketNotificationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketNotification = PutBucketNotification
    { _pbnBucket                    :: Text
    , _pbnContentMD5                :: Maybe Text
    , _pbnNotificationConfiguration :: NotificationConfiguration
    } deriving (Eq, Read, Show)

-- | 'PutBucketNotification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbnBucket' @::@ 'Text'
--
-- * 'pbnContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbnNotificationConfiguration' @::@ 'NotificationConfiguration'
--
putBucketNotification :: Text -- ^ 'pbnBucket'
                      -> NotificationConfiguration -- ^ 'pbnNotificationConfiguration'
                      -> PutBucketNotification
putBucketNotification p1 p2 = PutBucketNotification
    { _pbnBucket                    = p1
    , _pbnNotificationConfiguration = p2
    , _pbnContentMD5                = Nothing
    }

pbnBucket :: Lens' PutBucketNotification Text
pbnBucket = lens _pbnBucket (\s a -> s { _pbnBucket = a })

pbnContentMD5 :: Lens' PutBucketNotification (Maybe Text)
pbnContentMD5 = lens _pbnContentMD5 (\s a -> s { _pbnContentMD5 = a })

pbnNotificationConfiguration :: Lens' PutBucketNotification NotificationConfiguration
pbnNotificationConfiguration =
    lens _pbnNotificationConfiguration
        (\s a -> s { _pbnNotificationConfiguration = a })

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutBucketNotificationResponse' constructor.
putBucketNotificationResponse :: PutBucketNotificationResponse
putBucketNotificationResponse = PutBucketNotificationResponse

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = mconcat
        [ "/"
        , toText _pbnBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery = const "notification"

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} = mconcat
        [ "Content-MD5" =: _pbnContentMD5
        ]

instance ToXMLRoot PutBucketNotification where
    toXMLRoot PutBucketNotification{..} = namespaced ns "PutBucketNotification"
        [ "NotificationConfiguration" =@ _pbnNotificationConfiguration
        ]

instance ToXML PutBucketNotification

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = PutBucketNotificationResponse

    request  = put
    response = nullResponse PutBucketNotificationResponse

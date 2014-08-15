{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return the notification configuration of a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketNotification where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data GetBucketNotification = GetBucketNotification
    { _gbnrBucket :: BucketName
    } deriving (Show, Generic)

makeLenses ''GetBucketNotification

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toBS _gbnrBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery GetBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders GetBucketNotification

instance ToBody GetBucketNotification

data GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Show, Generic)

makeLenses ''GetBucketNotificationResponse

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request = get
    response _ = xmlResponse

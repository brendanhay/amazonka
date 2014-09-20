{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , pbnBucket
    , pbnContentMD5
    , pbnNotificationConfiguration

    -- * Response
    , PutBucketNotificationResponse
    -- ** Response constructor
    , putBucketNotificationResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketNotification = PutBucketNotification
    { _pbnBucket :: BucketName
    , _pbnContentMD5 :: Maybe Text
    , _pbnNotificationConfiguration :: NotificationConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketNotification' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
-- * @NotificationConfiguration ::@ @NotificationConfiguration@
--
putBucketNotification :: BucketName -- ^ 'pbnBucket'
                      -> NotificationConfiguration -- ^ 'pbnNotificationConfiguration'
                      -> PutBucketNotification
putBucketNotification p1 p3 = PutBucketNotification
    { _pbnBucket = p1
    , _pbnContentMD5 = Nothing
    , _pbnNotificationConfiguration = p3
    }

pbnBucket :: Lens' PutBucketNotification BucketName
pbnBucket = lens _pbnBucket (\s a -> s { _pbnBucket = a })

pbnContentMD5 :: Lens' PutBucketNotification (Maybe Text)
pbnContentMD5 = lens _pbnContentMD5 (\s a -> s { _pbnContentMD5 = a })

pbnNotificationConfiguration :: Lens' PutBucketNotification NotificationConfiguration
pbnNotificationConfiguration =
    lens _pbnNotificationConfiguration
         (\s a -> s { _pbnNotificationConfiguration = a })

instance ToPath PutBucketNotification

instance ToQuery PutBucketNotification

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} = concat
        [ "Content-MD5" =: _pbnContentMD5
        ]

instance ToBody PutBucketNotification where
    toBody = toBody . encodeXML . _pbnNotificationConfiguration

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketNotificationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putBucketNotificationResponse :: PutBucketNotificationResponse
putBucketNotificationResponse = PutBucketNotificationResponse

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = PutBucketNotificationResponse

    request = get
    response _ = nullaryResponse PutBucketNotificationResponse

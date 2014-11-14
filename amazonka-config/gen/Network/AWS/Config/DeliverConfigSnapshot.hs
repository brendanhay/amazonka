{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Schedules delivery of a configuration snapshot to the Amazon S3 bucket in
-- the specified delivery channel. After the delivery has started, AWS Config
-- sends following notifications using an Amazon SNS topic that you have
-- specified. Notification of starting the delivery. Notification of delivery
-- completed, if the delivery was successfully completed. Notification of
-- delivery failure, if the delivery failed to complete.
module Network.AWS.Config.DeliverConfigSnapshot
    (
    -- * Request
      DeliverConfigSnapshot
    -- ** Request constructor
    , deliverConfigSnapshot
    -- ** Request lenses
    , dcsDeliveryChannelName

    -- * Response
    , DeliverConfigSnapshotResponse
    -- ** Response constructor
    , deliverConfigSnapshotResponse
    -- ** Response lenses
    , dcsrConfigSnapshotId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype DeliverConfigSnapshot = DeliverConfigSnapshot
    { _dcsDeliveryChannelName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeliverConfigSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsDeliveryChannelName' @::@ 'Text'
--
deliverConfigSnapshot :: Text -- ^ 'dcsDeliveryChannelName'
                      -> DeliverConfigSnapshot
deliverConfigSnapshot p1 = DeliverConfigSnapshot
    { _dcsDeliveryChannelName = p1
    }

-- | The name of the delivery channel through which the snapshot is delivered.
dcsDeliveryChannelName :: Lens' DeliverConfigSnapshot Text
dcsDeliveryChannelName =
    lens _dcsDeliveryChannelName (\s a -> s { _dcsDeliveryChannelName = a })

instance ToPath DeliverConfigSnapshot where
    toPath = const "/"

instance ToQuery DeliverConfigSnapshot where
    toQuery = const mempty

instance ToHeaders DeliverConfigSnapshot

instance ToBody DeliverConfigSnapshot where
    toBody = toBody . encode . _dcsDeliveryChannelName

newtype DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse
    { _dcsrConfigSnapshotId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeliverConfigSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrConfigSnapshotId' @::@ 'Maybe' 'Text'
--
deliverConfigSnapshotResponse :: DeliverConfigSnapshotResponse
deliverConfigSnapshotResponse = DeliverConfigSnapshotResponse
    { _dcsrConfigSnapshotId = Nothing
    }

-- | The ID of the snapshot that is being created.
dcsrConfigSnapshotId :: Lens' DeliverConfigSnapshotResponse (Maybe Text)
dcsrConfigSnapshotId =
    lens _dcsrConfigSnapshotId (\s a -> s { _dcsrConfigSnapshotId = a })

instance AWSRequest DeliverConfigSnapshot where
    type Sv DeliverConfigSnapshot = Config
    type Rs DeliverConfigSnapshot = DeliverConfigSnapshotResponse

    request  = post
    response = jsonResponse $ \h o -> DeliverConfigSnapshotResponse
        <$> o .: "configSnapshotId"

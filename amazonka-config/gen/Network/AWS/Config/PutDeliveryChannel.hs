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

-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new delivery channel object to deliver the configuration
-- information to an Amazon S3 bucket, and to an Amazon SNS topic. You can use
-- this action to change the Amazon S3 bucket or an Amazon SNS topic of the
-- existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS
-- topic, call this action and specify the changed values for the S3 bucket
-- and the SNS topic. If you specify a different value for either the S3
-- bucket or the SNS topic, this action will keep the existing value for the
-- parameter that is not changed.
module Network.AWS.Config.PutDeliveryChannel
    (
    -- * Request
      PutDeliveryChannel
    -- ** Request constructor
    , putDeliveryChannel
    -- ** Request lenses
    , pdcDeliveryChannel

    -- * Response
    , PutDeliveryChannelResponse
    -- ** Response constructor
    , putDeliveryChannelResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype PutDeliveryChannel = PutDeliveryChannel
    { _pdcDeliveryChannel :: DeliveryChannel
    } deriving (Eq, Show, Generic)

-- | 'PutDeliveryChannel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdcDeliveryChannel' @::@ 'DeliveryChannel'
--
putDeliveryChannel :: DeliveryChannel -- ^ 'pdcDeliveryChannel'
                   -> PutDeliveryChannel
putDeliveryChannel p1 = PutDeliveryChannel
    { _pdcDeliveryChannel = p1
    }

-- | The configuration delivery channel object that delivers the configuration
-- information to an Amazon S3 bucket, and to an Amazon SNS topic.
pdcDeliveryChannel :: Lens' PutDeliveryChannel DeliveryChannel
pdcDeliveryChannel =
    lens _pdcDeliveryChannel (\s a -> s { _pdcDeliveryChannel = a })

instance ToPath PutDeliveryChannel where
    toPath = const "/"

instance ToQuery PutDeliveryChannel where
    toQuery = const mempty

instance ToHeaders PutDeliveryChannel

instance ToBody PutDeliveryChannel where
    toBody = toBody . encode . _pdcDeliveryChannel

data PutDeliveryChannelResponse = PutDeliveryChannelResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutDeliveryChannelResponse' constructor.
putDeliveryChannelResponse :: PutDeliveryChannelResponse
putDeliveryChannelResponse = PutDeliveryChannelResponse

-- FromJSON

instance AWSRequest PutDeliveryChannel where
    type Sv PutDeliveryChannel = Config
    type Rs PutDeliveryChannel = PutDeliveryChannelResponse

    request  = post'
    response = nullaryResponse PutDeliveryChannelResponse

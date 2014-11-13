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

-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns details about the specified delivery channel. If a delivery channel
-- is not specified, this action returns the details of all delivery channels
-- associated with the account.
module Network.AWS.Config.DescribeDeliveryChannels
    (
    -- * Request
      DescribeDeliveryChannels
    -- ** Request constructor
    , describeDeliveryChannels
    -- ** Request lenses
    , ddcDeliveryChannelNames

    -- * Response
    , DescribeDeliveryChannelsResponse
    -- ** Response constructor
    , describeDeliveryChannelsResponse
    -- ** Response lenses
    , ddcrDeliveryChannels
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype DescribeDeliveryChannels = DescribeDeliveryChannels
    { _ddcDeliveryChannelNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDeliveryChannels where
    type Item DescribeDeliveryChannels = Text

    fromList = DescribeDeliveryChannels . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddcDeliveryChannelNames

-- | 'DescribeDeliveryChannels' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeliveryChannelNames' @::@ ['Text']
--
describeDeliveryChannels :: DescribeDeliveryChannels
describeDeliveryChannels = DescribeDeliveryChannels
    { _ddcDeliveryChannelNames = mempty
    }

-- | A list of delivery channel names.
ddcDeliveryChannelNames :: Lens' DescribeDeliveryChannels [Text]
ddcDeliveryChannelNames =
    lens _ddcDeliveryChannelNames (\s a -> s { _ddcDeliveryChannelNames = a })

instance ToPath DescribeDeliveryChannels where
    toPath = const "/"

instance ToQuery DescribeDeliveryChannels where
    toQuery = const mempty

instance ToHeaders DescribeDeliveryChannels

instance ToBody DescribeDeliveryChannels where
    toBody = toBody . encode . _ddcDeliveryChannelNames

newtype DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse
    { _ddcrDeliveryChannels :: [DeliveryChannel]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDeliveryChannelsResponse where
    type Item DescribeDeliveryChannelsResponse = DeliveryChannel

    fromList = DescribeDeliveryChannelsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddcrDeliveryChannels

-- | 'DescribeDeliveryChannelsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcrDeliveryChannels' @::@ ['DeliveryChannel']
--
describeDeliveryChannelsResponse :: DescribeDeliveryChannelsResponse
describeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse
    { _ddcrDeliveryChannels = mempty
    }

-- | A list that contains the descriptions of the specified delivery channel.
ddcrDeliveryChannels :: Lens' DescribeDeliveryChannelsResponse [DeliveryChannel]
ddcrDeliveryChannels =
    lens _ddcrDeliveryChannels (\s a -> s { _ddcrDeliveryChannels = a })

-- FromJSON

instance AWSRequest DescribeDeliveryChannels where
    type Sv DescribeDeliveryChannels = Config
    type Rs DescribeDeliveryChannels = DescribeDeliveryChannelsResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeDeliveryChannelsResponse
        <$> o .: "DeliveryChannels"

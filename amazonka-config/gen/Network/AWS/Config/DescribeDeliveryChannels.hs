{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeDeliveryChannels.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

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

instance ToPath DescribeDeliveryChannels where
    toPath = const "/"

instance ToQuery DescribeDeliveryChannels where
    toQuery = const mempty

instance ToHeaders DescribeDeliveryChannels
instance ToJSON DescribeDeliveryChannels where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeDeliveryChannels where
    type Sv DescribeDeliveryChannels = Config
    type Rs DescribeDeliveryChannels = DescribeDeliveryChannelsResponse

    request  = post "DescribeDeliveryChannels"
    response = jsonResponse

instance FromJSON DescribeDeliveryChannelsResponse where
    parseJSON = genericParseJSON jsonOptions

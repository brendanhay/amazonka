{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current status of the specified delivery channel. If a delivery
-- channel is not specified, this action returns the current status of all
-- delivery channels associated with the account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeDeliveryChannelStatus.html>
module Network.AWS.Config.DescribeDeliveryChannelStatus
    (
    -- * Request
      DescribeDeliveryChannelStatus
    -- ** Request constructor
    , describeDeliveryChannelStatus
    -- ** Request lenses
    , ddcsDeliveryChannelNames

    -- * Response
    , DescribeDeliveryChannelStatusResponse
    -- ** Response constructor
    , describeDeliveryChannelStatusResponse
    -- ** Response lenses
    , ddcsrDeliveryChannelsStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus
    { _ddcsDeliveryChannelNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDeliveryChannelStatus where
    type Item DescribeDeliveryChannelStatus = Text

    fromList = DescribeDeliveryChannelStatus . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddcsDeliveryChannelNames

-- | 'DescribeDeliveryChannelStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsDeliveryChannelNames' @::@ ['Text']
--
describeDeliveryChannelStatus :: DescribeDeliveryChannelStatus
describeDeliveryChannelStatus = DescribeDeliveryChannelStatus
    { _ddcsDeliveryChannelNames = mempty
    }

-- | A list of delivery channel names.
ddcsDeliveryChannelNames :: Lens' DescribeDeliveryChannelStatus [Text]
ddcsDeliveryChannelNames =
    lens _ddcsDeliveryChannelNames
        (\s a -> s { _ddcsDeliveryChannelNames = a })

newtype DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse
    { _ddcsrDeliveryChannelsStatus :: [DeliveryChannelStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDeliveryChannelStatusResponse where
    type Item DescribeDeliveryChannelStatusResponse = DeliveryChannelStatus

    fromList = DescribeDeliveryChannelStatusResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddcsrDeliveryChannelsStatus

-- | 'DescribeDeliveryChannelStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsrDeliveryChannelsStatus' @::@ ['DeliveryChannelStatus']
--
describeDeliveryChannelStatusResponse :: DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse
    { _ddcsrDeliveryChannelsStatus = mempty
    }

-- | A list that contains the status of a specified delivery channel.
ddcsrDeliveryChannelsStatus :: Lens' DescribeDeliveryChannelStatusResponse [DeliveryChannelStatus]
ddcsrDeliveryChannelsStatus =
    lens _ddcsrDeliveryChannelsStatus
        (\s a -> s { _ddcsrDeliveryChannelsStatus = a })

instance AWSRequest DescribeDeliveryChannelStatus where
    type Sv DescribeDeliveryChannelStatus = Config
    type Rs DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatusResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeDeliveryChannelStatusResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeDeliveryChannelStatus where
    toPath = const "/"

instance ToHeaders DescribeDeliveryChannelStatus

instance ToQuery DescribeDeliveryChannelStatus where
    toQuery = const mempty

instance ToJSON DescribeDeliveryChannelStatus where
    toJSON = genericToJSON jsonOptions

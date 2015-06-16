{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Config.DescribeDeliveryChannels
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

-- | Returns details about the specified delivery channel. If a delivery
-- channel is not specified, this action returns the details of all
-- delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per account.
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Config.Types

-- | /See:/ 'describeDeliveryChannels' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeliveryChannelNames'
newtype DescribeDeliveryChannels = DescribeDeliveryChannels'{_ddcDeliveryChannelNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeDeliveryChannels' smart constructor.
describeDeliveryChannels :: DescribeDeliveryChannels
describeDeliveryChannels = DescribeDeliveryChannels'{_ddcDeliveryChannelNames = Nothing};

-- | A list of delivery channel names.
ddcDeliveryChannelNames :: Lens' DescribeDeliveryChannels [Text]
ddcDeliveryChannelNames = lens _ddcDeliveryChannelNames (\ s a -> s{_ddcDeliveryChannelNames = a}) . _Default;

instance AWSRequest DescribeDeliveryChannels where
        type Sv DescribeDeliveryChannels = Config
        type Rs DescribeDeliveryChannels =
             DescribeDeliveryChannelsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelsResponse' <$>
                   (x .?> "DeliveryChannels" .!@ mempty))

instance ToHeaders DescribeDeliveryChannels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeDeliveryChannels" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDeliveryChannels where
        toJSON DescribeDeliveryChannels'{..}
          = object
              ["DeliveryChannelNames" .= _ddcDeliveryChannelNames]

instance ToPath DescribeDeliveryChannels where
        toPath = const "/"

instance ToQuery DescribeDeliveryChannels where
        toQuery = const mempty

-- | /See:/ 'describeDeliveryChannelsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcrDeliveryChannels'
newtype DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'{_ddcrDeliveryChannels :: Maybe [DeliveryChannel]} deriving (Eq, Read, Show)

-- | 'DescribeDeliveryChannelsResponse' smart constructor.
describeDeliveryChannelsResponse :: DescribeDeliveryChannelsResponse
describeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'{_ddcrDeliveryChannels = Nothing};

-- | A list that contains the descriptions of the specified delivery channel.
ddcrDeliveryChannels :: Lens' DescribeDeliveryChannelsResponse [DeliveryChannel]
ddcrDeliveryChannels = lens _ddcrDeliveryChannels (\ s a -> s{_ddcrDeliveryChannels = a}) . _Default;

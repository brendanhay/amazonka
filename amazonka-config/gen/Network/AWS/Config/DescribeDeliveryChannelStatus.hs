{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
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

-- | Returns the current status of the specified delivery channel. If a
-- delivery channel is not specified, this action returns the current
-- status of all delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per account.
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

import Network.AWS.Config.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDeliveryChannelStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsDeliveryChannelNames'
newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'{_ddcsDeliveryChannelNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeDeliveryChannelStatus' smart constructor.
describeDeliveryChannelStatus :: DescribeDeliveryChannelStatus
describeDeliveryChannelStatus = DescribeDeliveryChannelStatus'{_ddcsDeliveryChannelNames = Nothing};

-- | A list of delivery channel names.
ddcsDeliveryChannelNames :: Lens' DescribeDeliveryChannelStatus [Text]
ddcsDeliveryChannelNames = lens _ddcsDeliveryChannelNames (\ s a -> s{_ddcsDeliveryChannelNames = a}) . _Default;

instance AWSRequest DescribeDeliveryChannelStatus
         where
        type Sv DescribeDeliveryChannelStatus = Config
        type Rs DescribeDeliveryChannelStatus =
             DescribeDeliveryChannelStatusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelStatusResponse' <$>
                   (x .?> "DeliveryChannelsStatus" .!@ mempty))

instance ToHeaders DescribeDeliveryChannelStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeDeliveryChannelStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDeliveryChannelStatus where
        toJSON DescribeDeliveryChannelStatus'{..}
          = object
              ["DeliveryChannelNames" .= _ddcsDeliveryChannelNames]

instance ToPath DescribeDeliveryChannelStatus where
        toPath = const "/"

instance ToQuery DescribeDeliveryChannelStatus where
        toQuery = const mempty

-- | /See:/ 'describeDeliveryChannelStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsrDeliveryChannelsStatus'
newtype DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'{_ddcsrDeliveryChannelsStatus :: Maybe [DeliveryChannelStatus]} deriving (Eq, Read, Show)

-- | 'DescribeDeliveryChannelStatusResponse' smart constructor.
describeDeliveryChannelStatusResponse :: DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'{_ddcsrDeliveryChannelsStatus = Nothing};

-- | A list that contains the status of a specified delivery channel.
ddcsrDeliveryChannelsStatus :: Lens' DescribeDeliveryChannelStatusResponse [DeliveryChannelStatus]
ddcsrDeliveryChannelsStatus = lens _ddcsrDeliveryChannelsStatus (\ s a -> s{_ddcsrDeliveryChannelsStatus = a}) . _Default;

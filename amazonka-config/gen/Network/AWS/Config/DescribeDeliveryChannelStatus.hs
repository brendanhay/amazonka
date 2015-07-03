{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , ddcsrStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DeliveryChannelStatus action.
--
-- /See:/ 'describeDeliveryChannelStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsDeliveryChannelNames'
newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
    { _ddcsDeliveryChannelNames :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeDeliveryChannelStatus' smart constructor.
describeDeliveryChannelStatus :: DescribeDeliveryChannelStatus
describeDeliveryChannelStatus =
    DescribeDeliveryChannelStatus'
    { _ddcsDeliveryChannelNames = Nothing
    }

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
                   (x .?> "DeliveryChannelsStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | The output for the DescribeDeliveryChannelStatus action.
--
-- /See:/ 'describeDeliveryChannelStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsrDeliveryChannelsStatus'
--
-- * 'ddcsrStatus'
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
    { _ddcsrDeliveryChannelsStatus :: !(Maybe [DeliveryChannelStatus])
    , _ddcsrStatus                 :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeDeliveryChannelStatusResponse' smart constructor.
describeDeliveryChannelStatusResponse :: Int -> DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatusResponse pStatus =
    DescribeDeliveryChannelStatusResponse'
    { _ddcsrDeliveryChannelsStatus = Nothing
    , _ddcsrStatus = pStatus
    }

-- | A list that contains the status of a specified delivery channel.
ddcsrDeliveryChannelsStatus :: Lens' DescribeDeliveryChannelStatusResponse [DeliveryChannelStatus]
ddcsrDeliveryChannelsStatus = lens _ddcsrDeliveryChannelsStatus (\ s a -> s{_ddcsrDeliveryChannelsStatus = a}) . _Default;

-- | FIXME: Undocumented member.
ddcsrStatus :: Lens' DescribeDeliveryChannelStatusResponse Int
ddcsrStatus = lens _ddcsrStatus (\ s a -> s{_ddcsrStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery
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
    , ddcrsDeliveryChannels
    , ddcrsStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeDeliveryChannels action.
--
-- /See:/ 'describeDeliveryChannels' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeliveryChannelNames'
newtype DescribeDeliveryChannels = DescribeDeliveryChannels'
    { _ddcDeliveryChannelNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDeliveryChannels' smart constructor.
describeDeliveryChannels :: DescribeDeliveryChannels
describeDeliveryChannels =
    DescribeDeliveryChannels'
    { _ddcDeliveryChannelNames = Nothing
    }

-- | A list of delivery channel names.
ddcDeliveryChannelNames :: Lens' DescribeDeliveryChannels [Text]
ddcDeliveryChannelNames = lens _ddcDeliveryChannelNames (\ s a -> s{_ddcDeliveryChannelNames = a}) . _Default;

instance AWSRequest DescribeDeliveryChannels where
        type Sv DescribeDeliveryChannels = Config
        type Rs DescribeDeliveryChannels =
             DescribeDeliveryChannelsResponse
        request = postJSON "DescribeDeliveryChannels"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelsResponse' <$>
                   (x .?> "DeliveryChannels" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | The output for the DescribeDeliveryChannels action.
--
-- /See:/ 'describeDeliveryChannelsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcrsDeliveryChannels'
--
-- * 'ddcrsStatus'
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
    { _ddcrsDeliveryChannels :: !(Maybe [DeliveryChannel])
    , _ddcrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDeliveryChannelsResponse' smart constructor.
describeDeliveryChannelsResponse :: Int -> DescribeDeliveryChannelsResponse
describeDeliveryChannelsResponse pStatus_ =
    DescribeDeliveryChannelsResponse'
    { _ddcrsDeliveryChannels = Nothing
    , _ddcrsStatus = pStatus_
    }

-- | A list that contains the descriptions of the specified delivery channel.
ddcrsDeliveryChannels :: Lens' DescribeDeliveryChannelsResponse [DeliveryChannel]
ddcrsDeliveryChannels = lens _ddcrsDeliveryChannels (\ s a -> s{_ddcrsDeliveryChannels = a}) . _Default;

-- | FIXME: Undocumented member.
ddcrsStatus :: Lens' DescribeDeliveryChannelsResponse Int
ddcrsStatus = lens _ddcrsStatus (\ s a -> s{_ddcrsStatus = a});

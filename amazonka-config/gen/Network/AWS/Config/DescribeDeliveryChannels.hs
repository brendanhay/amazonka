{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery
-- channel is not specified, this action returns the details of all
-- delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per account.
--
-- /See:/ <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeDeliveryChannels.html AWS API Reference> for DescribeDeliveryChannels.
module Network.AWS.Config.DescribeDeliveryChannels
    (
    -- * Creating a Request
      describeDeliveryChannels
    , DescribeDeliveryChannels
    -- * Request Lenses
    , ddcDeliveryChannelNames

    -- * Destructuring the Response
    , describeDeliveryChannelsResponse
    , DescribeDeliveryChannelsResponse
    -- * Response Lenses
    , ddcrsDeliveryChannels
    , ddcrsStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeDeliveryChannels action.
--
-- /See:/ 'describeDeliveryChannels' smart constructor.
newtype DescribeDeliveryChannels = DescribeDeliveryChannels'
    { _ddcDeliveryChannelNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDeliveryChannels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDeliveryChannelNames'
describeDeliveryChannels
    :: DescribeDeliveryChannels
describeDeliveryChannels =
    DescribeDeliveryChannels'
    { _ddcDeliveryChannelNames = Nothing
    }

-- | A list of delivery channel names.
ddcDeliveryChannelNames :: Lens' DescribeDeliveryChannels [Text]
ddcDeliveryChannelNames = lens _ddcDeliveryChannelNames (\ s a -> s{_ddcDeliveryChannelNames = a}) . _Default . _Coerce;

instance AWSRequest DescribeDeliveryChannels where
        type Rs DescribeDeliveryChannels =
             DescribeDeliveryChannelsResponse
        request = postJSON config
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
              (catMaybes
                 [("DeliveryChannelNames" .=) <$>
                    _ddcDeliveryChannelNames])

instance ToPath DescribeDeliveryChannels where
        toPath = const "/"

instance ToQuery DescribeDeliveryChannels where
        toQuery = const mempty

-- | The output for the DescribeDeliveryChannels action.
--
-- /See:/ 'describeDeliveryChannelsResponse' smart constructor.
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
    { _ddcrsDeliveryChannels :: !(Maybe [DeliveryChannel])
    , _ddcrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDeliveryChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsDeliveryChannels'
--
-- * 'ddcrsStatus'
describeDeliveryChannelsResponse
    :: Int -- ^ 'ddcrsStatus'
    -> DescribeDeliveryChannelsResponse
describeDeliveryChannelsResponse pStatus_ =
    DescribeDeliveryChannelsResponse'
    { _ddcrsDeliveryChannels = Nothing
    , _ddcrsStatus = pStatus_
    }

-- | A list that contains the descriptions of the specified delivery channel.
ddcrsDeliveryChannels :: Lens' DescribeDeliveryChannelsResponse [DeliveryChannel]
ddcrsDeliveryChannels = lens _ddcrsDeliveryChannels (\ s a -> s{_ddcrsDeliveryChannels = a}) . _Default . _Coerce;

-- | The response status code.
ddcrsStatus :: Lens' DescribeDeliveryChannelsResponse Int
ddcrsStatus = lens _ddcrsStatus (\ s a -> s{_ddcrsStatus = a});

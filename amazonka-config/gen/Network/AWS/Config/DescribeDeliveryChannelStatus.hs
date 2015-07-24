{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified delivery channel. If a
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
    , ddcsrsDeliveryChannelsStatus
    , ddcsrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
        request = postJSON "DescribeDeliveryChannelStatus"
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
-- * 'ddcsrsDeliveryChannelsStatus'
--
-- * 'ddcsrsStatus'
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
    { _ddcsrsDeliveryChannelsStatus :: !(Maybe [DeliveryChannelStatus])
    , _ddcsrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDeliveryChannelStatusResponse' smart constructor.
describeDeliveryChannelStatusResponse :: Int -> DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatusResponse pStatus_ =
    DescribeDeliveryChannelStatusResponse'
    { _ddcsrsDeliveryChannelsStatus = Nothing
    , _ddcsrsStatus = pStatus_
    }

-- | A list that contains the status of a specified delivery channel.
ddcsrsDeliveryChannelsStatus :: Lens' DescribeDeliveryChannelStatusResponse [DeliveryChannelStatus]
ddcsrsDeliveryChannelsStatus = lens _ddcsrsDeliveryChannelsStatus (\ s a -> s{_ddcsrsDeliveryChannelsStatus = a}) . _Default;

-- | FIXME: Undocumented member.
ddcsrsStatus :: Lens' DescribeDeliveryChannelStatusResponse Int
ddcsrsStatus = lens _ddcsrsStatus (\ s a -> s{_ddcsrsStatus = a});

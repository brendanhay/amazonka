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
-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.
--
--
module Network.AWS.Config.DescribeDeliveryChannelStatus
    (
    -- * Creating a Request
      describeDeliveryChannelStatus
    , DescribeDeliveryChannelStatus
    -- * Request Lenses
    , ddcsDeliveryChannelNames

    -- * Destructuring the Response
    , describeDeliveryChannelStatusResponse
    , DescribeDeliveryChannelStatusResponse
    -- * Response Lenses
    , ddcsrsDeliveryChannelsStatus
    , ddcsrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DeliveryChannelStatus' action.
--
--
--
-- /See:/ 'describeDeliveryChannelStatus' smart constructor.
newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
  { _ddcsDeliveryChannelNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeliveryChannelStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcsDeliveryChannelNames' - A list of delivery channel names.
describeDeliveryChannelStatus
    :: DescribeDeliveryChannelStatus
describeDeliveryChannelStatus =
  DescribeDeliveryChannelStatus' {_ddcsDeliveryChannelNames = Nothing}


-- | A list of delivery channel names.
ddcsDeliveryChannelNames :: Lens' DescribeDeliveryChannelStatus [Text]
ddcsDeliveryChannelNames = lens _ddcsDeliveryChannelNames (\ s a -> s{_ddcsDeliveryChannelNames = a}) . _Default . _Coerce

instance AWSRequest DescribeDeliveryChannelStatus
         where
        type Rs DescribeDeliveryChannelStatus =
             DescribeDeliveryChannelStatusResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelStatusResponse' <$>
                   (x .?> "DeliveryChannelsStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDeliveryChannelStatus where

instance NFData DescribeDeliveryChannelStatus where

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
              (catMaybes
                 [("DeliveryChannelNames" .=) <$>
                    _ddcsDeliveryChannelNames])

instance ToPath DescribeDeliveryChannelStatus where
        toPath = const "/"

instance ToQuery DescribeDeliveryChannelStatus where
        toQuery = const mempty

-- | The output for the 'DescribeDeliveryChannelStatus' action.
--
--
--
-- /See:/ 'describeDeliveryChannelStatusResponse' smart constructor.
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
  { _ddcsrsDeliveryChannelsStatus :: !(Maybe [DeliveryChannelStatus])
  , _ddcsrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeliveryChannelStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcsrsDeliveryChannelsStatus' - A list that contains the status of a specified delivery channel.
--
-- * 'ddcsrsResponseStatus' - -- | The response status code.
describeDeliveryChannelStatusResponse
    :: Int -- ^ 'ddcsrsResponseStatus'
    -> DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatusResponse pResponseStatus_ =
  DescribeDeliveryChannelStatusResponse'
    { _ddcsrsDeliveryChannelsStatus = Nothing
    , _ddcsrsResponseStatus = pResponseStatus_
    }


-- | A list that contains the status of a specified delivery channel.
ddcsrsDeliveryChannelsStatus :: Lens' DescribeDeliveryChannelStatusResponse [DeliveryChannelStatus]
ddcsrsDeliveryChannelsStatus = lens _ddcsrsDeliveryChannelsStatus (\ s a -> s{_ddcsrsDeliveryChannelsStatus = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcsrsResponseStatus :: Lens' DescribeDeliveryChannelStatusResponse Int
ddcsrsResponseStatus = lens _ddcsrsResponseStatus (\ s a -> s{_ddcsrsResponseStatus = a})

instance NFData DescribeDeliveryChannelStatusResponse
         where

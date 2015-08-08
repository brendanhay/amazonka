{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnassignPrivateIPAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses from a network
-- interface.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnassignPrivateIPAddresses.html AWS API Reference> for UnassignPrivateIPAddresses.
module Network.AWS.EC2.UnassignPrivateIPAddresses
    (
    -- * Creating a Request
      UnassignPrivateIPAddresses
    , unassignPrivateIPAddresses
    -- * Request Lenses
    , upiaNetworkInterfaceId
    , upiaPrivateIPAddresses

    -- * Destructuring the Response
    , UnassignPrivateIPAddressesResponse
    , unassignPrivateIPAddressesResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'unassignPrivateIPAddresses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upiaNetworkInterfaceId'
--
-- * 'upiaPrivateIPAddresses'
data UnassignPrivateIPAddresses = UnassignPrivateIPAddresses'
    { _upiaNetworkInterfaceId :: !Text
    , _upiaPrivateIPAddresses :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnassignPrivateIPAddresses' smart constructor.
unassignPrivateIPAddresses :: Text -> UnassignPrivateIPAddresses
unassignPrivateIPAddresses pNetworkInterfaceId_ =
    UnassignPrivateIPAddresses'
    { _upiaNetworkInterfaceId = pNetworkInterfaceId_
    , _upiaPrivateIPAddresses = mempty
    }

-- | The ID of the network interface.
upiaNetworkInterfaceId :: Lens' UnassignPrivateIPAddresses Text
upiaNetworkInterfaceId = lens _upiaNetworkInterfaceId (\ s a -> s{_upiaNetworkInterfaceId = a});

-- | The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
upiaPrivateIPAddresses :: Lens' UnassignPrivateIPAddresses [Text]
upiaPrivateIPAddresses = lens _upiaPrivateIPAddresses (\ s a -> s{_upiaPrivateIPAddresses = a}) . _Coerce;

instance AWSRequest UnassignPrivateIPAddresses where
        type Sv UnassignPrivateIPAddresses = EC2
        type Rs UnassignPrivateIPAddresses =
             UnassignPrivateIPAddressesResponse
        request = post
        response
          = receiveNull UnassignPrivateIPAddressesResponse'

instance ToHeaders UnassignPrivateIPAddresses where
        toHeaders = const mempty

instance ToPath UnassignPrivateIPAddresses where
        toPath = const "/"

instance ToQuery UnassignPrivateIPAddresses where
        toQuery UnassignPrivateIPAddresses'{..}
          = mconcat
              ["Action" =:
                 ("UnassignPrivateIpAddresses" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NetworkInterfaceId" =: _upiaNetworkInterfaceId,
               toQueryList "PrivateIpAddress"
                 _upiaPrivateIPAddresses]

-- | /See:/ 'unassignPrivateIPAddressesResponse' smart constructor.
data UnassignPrivateIPAddressesResponse =
    UnassignPrivateIPAddressesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnassignPrivateIPAddressesResponse' smart constructor.
unassignPrivateIPAddressesResponse :: UnassignPrivateIPAddressesResponse
unassignPrivateIPAddressesResponse = UnassignPrivateIPAddressesResponse'

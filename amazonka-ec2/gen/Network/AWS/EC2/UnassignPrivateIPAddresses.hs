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
-- Module      : Network.AWS.EC2.UnassignPrivateIPAddresses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses from a network interface.
--
--
module Network.AWS.EC2.UnassignPrivateIPAddresses
    (
    -- * Creating a Request
      unassignPrivateIPAddresses
    , UnassignPrivateIPAddresses
    -- * Request Lenses
    , upiaNetworkInterfaceId
    , upiaPrivateIPAddresses

    -- * Destructuring the Response
    , unassignPrivateIPAddressesResponse
    , UnassignPrivateIPAddressesResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for UnassignPrivateIpAddresses.
--
--
--
-- /See:/ 'unassignPrivateIPAddresses' smart constructor.
data UnassignPrivateIPAddresses = UnassignPrivateIPAddresses'
  { _upiaNetworkInterfaceId :: !Text
  , _upiaPrivateIPAddresses :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignPrivateIPAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upiaNetworkInterfaceId' - The ID of the network interface.
--
-- * 'upiaPrivateIPAddresses' - The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
unassignPrivateIPAddresses
    :: Text -- ^ 'upiaNetworkInterfaceId'
    -> UnassignPrivateIPAddresses
unassignPrivateIPAddresses pNetworkInterfaceId_ =
  UnassignPrivateIPAddresses'
    { _upiaNetworkInterfaceId = pNetworkInterfaceId_
    , _upiaPrivateIPAddresses = mempty
    }


-- | The ID of the network interface.
upiaNetworkInterfaceId :: Lens' UnassignPrivateIPAddresses Text
upiaNetworkInterfaceId = lens _upiaNetworkInterfaceId (\ s a -> s{_upiaNetworkInterfaceId = a})

-- | The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
upiaPrivateIPAddresses :: Lens' UnassignPrivateIPAddresses [Text]
upiaPrivateIPAddresses = lens _upiaPrivateIPAddresses (\ s a -> s{_upiaPrivateIPAddresses = a}) . _Coerce

instance AWSRequest UnassignPrivateIPAddresses where
        type Rs UnassignPrivateIPAddresses =
             UnassignPrivateIPAddressesResponse
        request = postQuery ec2
        response
          = receiveNull UnassignPrivateIPAddressesResponse'

instance Hashable UnassignPrivateIPAddresses where

instance NFData UnassignPrivateIPAddresses where

instance ToHeaders UnassignPrivateIPAddresses where
        toHeaders = const mempty

instance ToPath UnassignPrivateIPAddresses where
        toPath = const "/"

instance ToQuery UnassignPrivateIPAddresses where
        toQuery UnassignPrivateIPAddresses'{..}
          = mconcat
              ["Action" =:
                 ("UnassignPrivateIpAddresses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NetworkInterfaceId" =: _upiaNetworkInterfaceId,
               toQueryList "PrivateIpAddress"
                 _upiaPrivateIPAddresses]

-- | /See:/ 'unassignPrivateIPAddressesResponse' smart constructor.
data UnassignPrivateIPAddressesResponse =
  UnassignPrivateIPAddressesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignPrivateIPAddressesResponse' with the minimum fields required to make a request.
--
unassignPrivateIPAddressesResponse
    :: UnassignPrivateIPAddressesResponse
unassignPrivateIPAddressesResponse = UnassignPrivateIPAddressesResponse'


instance NFData UnassignPrivateIPAddressesResponse
         where

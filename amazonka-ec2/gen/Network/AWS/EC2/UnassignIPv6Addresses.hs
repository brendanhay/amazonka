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
-- Module      : Network.AWS.EC2.UnassignIPv6Addresses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more IPv6 addresses from a network interface.
--
--
module Network.AWS.EC2.UnassignIPv6Addresses
    (
    -- * Creating a Request
      unassignIPv6Addresses
    , UnassignIPv6Addresses
    -- * Request Lenses
    , uiaIPv6Addresses
    , uiaNetworkInterfaceId

    -- * Destructuring the Response
    , unassignIPv6AddressesResponse
    , UnassignIPv6AddressesResponse
    -- * Response Lenses
    , uiarsNetworkInterfaceId
    , uiarsUnassignedIPv6Addresses
    , uiarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unassignIPv6Addresses' smart constructor.
data UnassignIPv6Addresses = UnassignIPv6Addresses'
  { _uiaIPv6Addresses      :: ![Text]
  , _uiaNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignIPv6Addresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiaIPv6Addresses' - The IPv6 addresses to unassign from the network interface.
--
-- * 'uiaNetworkInterfaceId' - The ID of the network interface.
unassignIPv6Addresses
    :: Text -- ^ 'uiaNetworkInterfaceId'
    -> UnassignIPv6Addresses
unassignIPv6Addresses pNetworkInterfaceId_ =
  UnassignIPv6Addresses'
    {_uiaIPv6Addresses = mempty, _uiaNetworkInterfaceId = pNetworkInterfaceId_}


-- | The IPv6 addresses to unassign from the network interface.
uiaIPv6Addresses :: Lens' UnassignIPv6Addresses [Text]
uiaIPv6Addresses = lens _uiaIPv6Addresses (\ s a -> s{_uiaIPv6Addresses = a}) . _Coerce

-- | The ID of the network interface.
uiaNetworkInterfaceId :: Lens' UnassignIPv6Addresses Text
uiaNetworkInterfaceId = lens _uiaNetworkInterfaceId (\ s a -> s{_uiaNetworkInterfaceId = a})

instance AWSRequest UnassignIPv6Addresses where
        type Rs UnassignIPv6Addresses =
             UnassignIPv6AddressesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 UnassignIPv6AddressesResponse' <$>
                   (x .@? "networkInterfaceId") <*>
                     (x .@? "unassignedIpv6Addresses" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable UnassignIPv6Addresses where

instance NFData UnassignIPv6Addresses where

instance ToHeaders UnassignIPv6Addresses where
        toHeaders = const mempty

instance ToPath UnassignIPv6Addresses where
        toPath = const "/"

instance ToQuery UnassignIPv6Addresses where
        toQuery UnassignIPv6Addresses'{..}
          = mconcat
              ["Action" =: ("UnassignIpv6Addresses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQueryList "Ipv6Addresses" _uiaIPv6Addresses,
               "NetworkInterfaceId" =: _uiaNetworkInterfaceId]

-- | /See:/ 'unassignIPv6AddressesResponse' smart constructor.
data UnassignIPv6AddressesResponse = UnassignIPv6AddressesResponse'
  { _uiarsNetworkInterfaceId      :: !(Maybe Text)
  , _uiarsUnassignedIPv6Addresses :: !(Maybe [Text])
  , _uiarsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignIPv6AddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiarsNetworkInterfaceId' - The ID of the network interface.
--
-- * 'uiarsUnassignedIPv6Addresses' - The IPv6 addresses that have been unassigned from the network interface.
--
-- * 'uiarsResponseStatus' - -- | The response status code.
unassignIPv6AddressesResponse
    :: Int -- ^ 'uiarsResponseStatus'
    -> UnassignIPv6AddressesResponse
unassignIPv6AddressesResponse pResponseStatus_ =
  UnassignIPv6AddressesResponse'
    { _uiarsNetworkInterfaceId = Nothing
    , _uiarsUnassignedIPv6Addresses = Nothing
    , _uiarsResponseStatus = pResponseStatus_
    }


-- | The ID of the network interface.
uiarsNetworkInterfaceId :: Lens' UnassignIPv6AddressesResponse (Maybe Text)
uiarsNetworkInterfaceId = lens _uiarsNetworkInterfaceId (\ s a -> s{_uiarsNetworkInterfaceId = a})

-- | The IPv6 addresses that have been unassigned from the network interface.
uiarsUnassignedIPv6Addresses :: Lens' UnassignIPv6AddressesResponse [Text]
uiarsUnassignedIPv6Addresses = lens _uiarsUnassignedIPv6Addresses (\ s a -> s{_uiarsUnassignedIPv6Addresses = a}) . _Default . _Coerce

-- | -- | The response status code.
uiarsResponseStatus :: Lens' UnassignIPv6AddressesResponse Int
uiarsResponseStatus = lens _uiarsResponseStatus (\ s a -> s{_uiarsResponseStatus = a})

instance NFData UnassignIPv6AddressesResponse where

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
-- Module      : Network.AWS.EC2.AssignIPv6Addresses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more IPv6 addresses to the specified network interface. You can specify one or more specific IPv6 addresses, or you can specify the number of IPv6 addresses to be automatically assigned from within the subnet's IPv6 CIDR block range. You can assign as many IPv6 addresses to a network interface as you can assign private IPv4 addresses, and the limit varies per instance type. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per Network Interface Per Instance Type> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
module Network.AWS.EC2.AssignIPv6Addresses
    (
    -- * Creating a Request
      assignIPv6Addresses
    , AssignIPv6Addresses
    -- * Request Lenses
    , aiaIPv6AddressCount
    , aiaIPv6Addresses
    , aiaNetworkInterfaceId

    -- * Destructuring the Response
    , assignIPv6AddressesResponse
    , AssignIPv6AddressesResponse
    -- * Response Lenses
    , aiarsNetworkInterfaceId
    , aiarsAssignedIPv6Addresses
    , aiarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'assignIPv6Addresses' smart constructor.
data AssignIPv6Addresses = AssignIPv6Addresses'
  { _aiaIPv6AddressCount   :: !(Maybe Int)
  , _aiaIPv6Addresses      :: !(Maybe [Text])
  , _aiaNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignIPv6Addresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiaIPv6AddressCount' - The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- * 'aiaIPv6Addresses' - One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- * 'aiaNetworkInterfaceId' - The ID of the network interface.
assignIPv6Addresses
    :: Text -- ^ 'aiaNetworkInterfaceId'
    -> AssignIPv6Addresses
assignIPv6Addresses pNetworkInterfaceId_ =
  AssignIPv6Addresses'
    { _aiaIPv6AddressCount = Nothing
    , _aiaIPv6Addresses = Nothing
    , _aiaNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
aiaIPv6AddressCount :: Lens' AssignIPv6Addresses (Maybe Int)
aiaIPv6AddressCount = lens _aiaIPv6AddressCount (\ s a -> s{_aiaIPv6AddressCount = a})

-- | One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
aiaIPv6Addresses :: Lens' AssignIPv6Addresses [Text]
aiaIPv6Addresses = lens _aiaIPv6Addresses (\ s a -> s{_aiaIPv6Addresses = a}) . _Default . _Coerce

-- | The ID of the network interface.
aiaNetworkInterfaceId :: Lens' AssignIPv6Addresses Text
aiaNetworkInterfaceId = lens _aiaNetworkInterfaceId (\ s a -> s{_aiaNetworkInterfaceId = a})

instance AWSRequest AssignIPv6Addresses where
        type Rs AssignIPv6Addresses =
             AssignIPv6AddressesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssignIPv6AddressesResponse' <$>
                   (x .@? "networkInterfaceId") <*>
                     (x .@? "assignedIpv6Addresses" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable AssignIPv6Addresses where

instance NFData AssignIPv6Addresses where

instance ToHeaders AssignIPv6Addresses where
        toHeaders = const mempty

instance ToPath AssignIPv6Addresses where
        toPath = const "/"

instance ToQuery AssignIPv6Addresses where
        toQuery AssignIPv6Addresses'{..}
          = mconcat
              ["Action" =: ("AssignIpv6Addresses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Ipv6AddressCount" =: _aiaIPv6AddressCount,
               toQuery
                 (toQueryList "Ipv6Addresses" <$> _aiaIPv6Addresses),
               "NetworkInterfaceId" =: _aiaNetworkInterfaceId]

-- | /See:/ 'assignIPv6AddressesResponse' smart constructor.
data AssignIPv6AddressesResponse = AssignIPv6AddressesResponse'
  { _aiarsNetworkInterfaceId    :: !(Maybe Text)
  , _aiarsAssignedIPv6Addresses :: !(Maybe [Text])
  , _aiarsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignIPv6AddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiarsNetworkInterfaceId' - The ID of the network interface.
--
-- * 'aiarsAssignedIPv6Addresses' - The IPv6 addresses assigned to the network interface.
--
-- * 'aiarsResponseStatus' - -- | The response status code.
assignIPv6AddressesResponse
    :: Int -- ^ 'aiarsResponseStatus'
    -> AssignIPv6AddressesResponse
assignIPv6AddressesResponse pResponseStatus_ =
  AssignIPv6AddressesResponse'
    { _aiarsNetworkInterfaceId = Nothing
    , _aiarsAssignedIPv6Addresses = Nothing
    , _aiarsResponseStatus = pResponseStatus_
    }


-- | The ID of the network interface.
aiarsNetworkInterfaceId :: Lens' AssignIPv6AddressesResponse (Maybe Text)
aiarsNetworkInterfaceId = lens _aiarsNetworkInterfaceId (\ s a -> s{_aiarsNetworkInterfaceId = a})

-- | The IPv6 addresses assigned to the network interface.
aiarsAssignedIPv6Addresses :: Lens' AssignIPv6AddressesResponse [Text]
aiarsAssignedIPv6Addresses = lens _aiarsAssignedIPv6Addresses (\ s a -> s{_aiarsAssignedIPv6Addresses = a}) . _Default . _Coerce

-- | -- | The response status code.
aiarsResponseStatus :: Lens' AssignIPv6AddressesResponse Int
aiarsResponseStatus = lens _aiarsResponseStatus (\ s a -> s{_aiarsResponseStatus = a})

instance NFData AssignIPv6AddressesResponse where

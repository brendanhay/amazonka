{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignPrivateIPAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more secondary private IP addresses to the specified network interface.
--
--
-- You can specify one or more specific secondary IP addresses, or you can specify the number of secondary IP addresses to be automatically assigned within the subnet's CIDR block range. The number of secondary IP addresses that you can assign to an instance varies by instance type. For information about instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . For more information about Elastic IP addresses, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- When you move a secondary private IP address to another network interface, any Elastic IP address that is associated with the IP address is also moved.
--
-- Remapping an IP address is an asynchronous operation. When you move an IP address from one network interface to another, check @network/interfaces/macs/mac/local-ipv4s@ in the instance metadata to confirm that the remapping is complete.
--
-- You must specify either the IP addresses or the IP address count in the request.
module Network.AWS.EC2.AssignPrivateIPAddresses
  ( -- * Creating a Request
    assignPrivateIPAddresses,
    AssignPrivateIPAddresses,

    -- * Request Lenses
    apiaPrivateIPAddresses,
    apiaAllowReassignment,
    apiaSecondaryPrivateIPAddressCount,
    apiaNetworkInterfaceId,

    -- * Destructuring the Response
    assignPrivateIPAddressesResponse,
    AssignPrivateIPAddressesResponse,

    -- * Response Lenses
    apiarsAssignedPrivateIPAddresses,
    apiarsNetworkInterfaceId,
    apiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AssignPrivateIpAddresses.
--
--
--
-- /See:/ 'assignPrivateIPAddresses' smart constructor.
data AssignPrivateIPAddresses = AssignPrivateIPAddresses'
  { _apiaPrivateIPAddresses ::
      !(Maybe [Text]),
    _apiaAllowReassignment :: !(Maybe Bool),
    _apiaSecondaryPrivateIPAddressCount ::
      !(Maybe Int),
    _apiaNetworkInterfaceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssignPrivateIPAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apiaPrivateIPAddresses' - One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses. If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
--
-- * 'apiaAllowReassignment' - Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
--
-- * 'apiaSecondaryPrivateIPAddressCount' - The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
--
-- * 'apiaNetworkInterfaceId' - The ID of the network interface.
assignPrivateIPAddresses ::
  -- | 'apiaNetworkInterfaceId'
  Text ->
  AssignPrivateIPAddresses
assignPrivateIPAddresses pNetworkInterfaceId_ =
  AssignPrivateIPAddresses'
    { _apiaPrivateIPAddresses = Nothing,
      _apiaAllowReassignment = Nothing,
      _apiaSecondaryPrivateIPAddressCount = Nothing,
      _apiaNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses. If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
apiaPrivateIPAddresses :: Lens' AssignPrivateIPAddresses [Text]
apiaPrivateIPAddresses = lens _apiaPrivateIPAddresses (\s a -> s {_apiaPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
apiaAllowReassignment :: Lens' AssignPrivateIPAddresses (Maybe Bool)
apiaAllowReassignment = lens _apiaAllowReassignment (\s a -> s {_apiaAllowReassignment = a})

-- | The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
apiaSecondaryPrivateIPAddressCount :: Lens' AssignPrivateIPAddresses (Maybe Int)
apiaSecondaryPrivateIPAddressCount = lens _apiaSecondaryPrivateIPAddressCount (\s a -> s {_apiaSecondaryPrivateIPAddressCount = a})

-- | The ID of the network interface.
apiaNetworkInterfaceId :: Lens' AssignPrivateIPAddresses Text
apiaNetworkInterfaceId = lens _apiaNetworkInterfaceId (\s a -> s {_apiaNetworkInterfaceId = a})

instance AWSRequest AssignPrivateIPAddresses where
  type Rs AssignPrivateIPAddresses = AssignPrivateIPAddressesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AssignPrivateIPAddressesResponse'
            <$> ( x .@? "assignedPrivateIpAddressesSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "networkInterfaceId")
            <*> (pure (fromEnum s))
      )

instance Hashable AssignPrivateIPAddresses

instance NFData AssignPrivateIPAddresses

instance ToHeaders AssignPrivateIPAddresses where
  toHeaders = const mempty

instance ToPath AssignPrivateIPAddresses where
  toPath = const "/"

instance ToQuery AssignPrivateIPAddresses where
  toQuery AssignPrivateIPAddresses' {..} =
    mconcat
      [ "Action" =: ("AssignPrivateIpAddresses" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "PrivateIpAddress" <$> _apiaPrivateIPAddresses),
        "AllowReassignment" =: _apiaAllowReassignment,
        "SecondaryPrivateIpAddressCount"
          =: _apiaSecondaryPrivateIPAddressCount,
        "NetworkInterfaceId" =: _apiaNetworkInterfaceId
      ]

-- | /See:/ 'assignPrivateIPAddressesResponse' smart constructor.
data AssignPrivateIPAddressesResponse = AssignPrivateIPAddressesResponse'
  { _apiarsAssignedPrivateIPAddresses ::
      !( Maybe
           [AssignedPrivateIPAddress]
       ),
    _apiarsNetworkInterfaceId ::
      !(Maybe Text),
    _apiarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssignPrivateIPAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apiarsAssignedPrivateIPAddresses' - The private IP addresses assigned to the network interface.
--
-- * 'apiarsNetworkInterfaceId' - The ID of the network interface.
--
-- * 'apiarsResponseStatus' - -- | The response status code.
assignPrivateIPAddressesResponse ::
  -- | 'apiarsResponseStatus'
  Int ->
  AssignPrivateIPAddressesResponse
assignPrivateIPAddressesResponse pResponseStatus_ =
  AssignPrivateIPAddressesResponse'
    { _apiarsAssignedPrivateIPAddresses =
        Nothing,
      _apiarsNetworkInterfaceId = Nothing,
      _apiarsResponseStatus = pResponseStatus_
    }

-- | The private IP addresses assigned to the network interface.
apiarsAssignedPrivateIPAddresses :: Lens' AssignPrivateIPAddressesResponse [AssignedPrivateIPAddress]
apiarsAssignedPrivateIPAddresses = lens _apiarsAssignedPrivateIPAddresses (\s a -> s {_apiarsAssignedPrivateIPAddresses = a}) . _Default . _Coerce

-- | The ID of the network interface.
apiarsNetworkInterfaceId :: Lens' AssignPrivateIPAddressesResponse (Maybe Text)
apiarsNetworkInterfaceId = lens _apiarsNetworkInterfaceId (\s a -> s {_apiarsNetworkInterfaceId = a})

-- | -- | The response status code.
apiarsResponseStatus :: Lens' AssignPrivateIPAddressesResponse Int
apiarsResponseStatus = lens _apiarsResponseStatus (\s a -> s {_apiarsResponseStatus = a})

instance NFData AssignPrivateIPAddressesResponse

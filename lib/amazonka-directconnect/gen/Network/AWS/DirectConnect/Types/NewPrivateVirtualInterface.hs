{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a private virtual interface.
--
--
--
-- /See:/ 'newPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { _nVirtualGatewayId ::
      !(Maybe Text),
    _nMtu :: !(Maybe Int),
    _nCustomerAddress :: !(Maybe Text),
    _nAmazonAddress :: !(Maybe Text),
    _nAddressFamily ::
      !(Maybe AddressFamily),
    _nDirectConnectGatewayId ::
      !(Maybe Text),
    _nAuthKey :: !(Maybe Text),
    _nTags :: !(Maybe (List1 Tag)),
    _nVirtualInterfaceName :: !Text,
    _nVlan :: !Int,
    _nAsn :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'nMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'nCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'nAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'nAddressFamily' - The address family for the BGP peer.
--
-- * 'nDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'nAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- * 'nTags' - The tags associated with the private virtual interface.
--
-- * 'nVirtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- * 'nVlan' - The ID of the VLAN.
--
-- * 'nAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
newPrivateVirtualInterface ::
  -- | 'nVirtualInterfaceName'
  Text ->
  -- | 'nVlan'
  Int ->
  -- | 'nAsn'
  Int ->
  NewPrivateVirtualInterface
newPrivateVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterface'
    { _nVirtualGatewayId = Nothing,
      _nMtu = Nothing,
      _nCustomerAddress = Nothing,
      _nAmazonAddress = Nothing,
      _nAddressFamily = Nothing,
      _nDirectConnectGatewayId = Nothing,
      _nAuthKey = Nothing,
      _nTags = Nothing,
      _nVirtualInterfaceName = pVirtualInterfaceName_,
      _nVlan = pVlan_,
      _nAsn = pAsn_
    }

-- | The ID of the virtual private gateway.
nVirtualGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nVirtualGatewayId = lens _nVirtualGatewayId (\s a -> s {_nVirtualGatewayId = a})

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
nMtu :: Lens' NewPrivateVirtualInterface (Maybe Int)
nMtu = lens _nMtu (\s a -> s {_nMtu = a})

-- | The IP address assigned to the customer interface.
nCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nCustomerAddress = lens _nCustomerAddress (\s a -> s {_nCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
nAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAmazonAddress = lens _nAmazonAddress (\s a -> s {_nAmazonAddress = a})

-- | The address family for the BGP peer.
nAddressFamily :: Lens' NewPrivateVirtualInterface (Maybe AddressFamily)
nAddressFamily = lens _nAddressFamily (\s a -> s {_nAddressFamily = a})

-- | The ID of the Direct Connect gateway.
nDirectConnectGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nDirectConnectGatewayId = lens _nDirectConnectGatewayId (\s a -> s {_nDirectConnectGatewayId = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
nAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAuthKey = lens _nAuthKey (\s a -> s {_nAuthKey = a})

-- | The tags associated with the private virtual interface.
nTags :: Lens' NewPrivateVirtualInterface (Maybe (NonEmpty Tag))
nTags = lens _nTags (\s a -> s {_nTags = a}) . mapping _List1

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
nVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
nVirtualInterfaceName = lens _nVirtualInterfaceName (\s a -> s {_nVirtualInterfaceName = a})

-- | The ID of the VLAN.
nVlan :: Lens' NewPrivateVirtualInterface Int
nVlan = lens _nVlan (\s a -> s {_nVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
nAsn :: Lens' NewPrivateVirtualInterface Int
nAsn = lens _nAsn (\s a -> s {_nAsn = a})

instance Hashable NewPrivateVirtualInterface

instance NFData NewPrivateVirtualInterface

instance ToJSON NewPrivateVirtualInterface where
  toJSON NewPrivateVirtualInterface' {..} =
    object
      ( catMaybes
          [ ("virtualGatewayId" .=) <$> _nVirtualGatewayId,
            ("mtu" .=) <$> _nMtu,
            ("customerAddress" .=) <$> _nCustomerAddress,
            ("amazonAddress" .=) <$> _nAmazonAddress,
            ("addressFamily" .=) <$> _nAddressFamily,
            ("directConnectGatewayId" .=) <$> _nDirectConnectGatewayId,
            ("authKey" .=) <$> _nAuthKey,
            ("tags" .=) <$> _nTags,
            Just ("virtualInterfaceName" .= _nVirtualInterfaceName),
            Just ("vlan" .= _nVlan),
            Just ("asn" .= _nAsn)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterface where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a transit virtual interface.
--
--
--
-- /See:/ 'newTransitVirtualInterface' smart constructor.
data NewTransitVirtualInterface = NewTransitVirtualInterface'
  { _ntviMtu ::
      !(Maybe Int),
    _ntviCustomerAddress :: !(Maybe Text),
    _ntviVlan :: !(Maybe Int),
    _ntviAmazonAddress :: !(Maybe Text),
    _ntviAddressFamily ::
      !(Maybe AddressFamily),
    _ntviDirectConnectGatewayId ::
      !(Maybe Text),
    _ntviAsn :: !(Maybe Int),
    _ntviAuthKey :: !(Maybe Text),
    _ntviVirtualInterfaceName ::
      !(Maybe Text),
    _ntviTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewTransitVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntviMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'ntviCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'ntviVlan' - The ID of the VLAN.
--
-- * 'ntviAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'ntviAddressFamily' - The address family for the BGP peer.
--
-- * 'ntviDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'ntviAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
--
-- * 'ntviAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- * 'ntviVirtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- * 'ntviTags' - The tags associated with the transitive virtual interface.
newTransitVirtualInterface ::
  NewTransitVirtualInterface
newTransitVirtualInterface =
  NewTransitVirtualInterface'
    { _ntviMtu = Nothing,
      _ntviCustomerAddress = Nothing,
      _ntviVlan = Nothing,
      _ntviAmazonAddress = Nothing,
      _ntviAddressFamily = Nothing,
      _ntviDirectConnectGatewayId = Nothing,
      _ntviAsn = Nothing,
      _ntviAuthKey = Nothing,
      _ntviVirtualInterfaceName = Nothing,
      _ntviTags = Nothing
    }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
ntviMtu :: Lens' NewTransitVirtualInterface (Maybe Int)
ntviMtu = lens _ntviMtu (\s a -> s {_ntviMtu = a})

-- | The IP address assigned to the customer interface.
ntviCustomerAddress :: Lens' NewTransitVirtualInterface (Maybe Text)
ntviCustomerAddress = lens _ntviCustomerAddress (\s a -> s {_ntviCustomerAddress = a})

-- | The ID of the VLAN.
ntviVlan :: Lens' NewTransitVirtualInterface (Maybe Int)
ntviVlan = lens _ntviVlan (\s a -> s {_ntviVlan = a})

-- | The IP address assigned to the Amazon interface.
ntviAmazonAddress :: Lens' NewTransitVirtualInterface (Maybe Text)
ntviAmazonAddress = lens _ntviAmazonAddress (\s a -> s {_ntviAmazonAddress = a})

-- | The address family for the BGP peer.
ntviAddressFamily :: Lens' NewTransitVirtualInterface (Maybe AddressFamily)
ntviAddressFamily = lens _ntviAddressFamily (\s a -> s {_ntviAddressFamily = a})

-- | The ID of the Direct Connect gateway.
ntviDirectConnectGatewayId :: Lens' NewTransitVirtualInterface (Maybe Text)
ntviDirectConnectGatewayId = lens _ntviDirectConnectGatewayId (\s a -> s {_ntviDirectConnectGatewayId = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
ntviAsn :: Lens' NewTransitVirtualInterface (Maybe Int)
ntviAsn = lens _ntviAsn (\s a -> s {_ntviAsn = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
ntviAuthKey :: Lens' NewTransitVirtualInterface (Maybe Text)
ntviAuthKey = lens _ntviAuthKey (\s a -> s {_ntviAuthKey = a})

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
ntviVirtualInterfaceName :: Lens' NewTransitVirtualInterface (Maybe Text)
ntviVirtualInterfaceName = lens _ntviVirtualInterfaceName (\s a -> s {_ntviVirtualInterfaceName = a})

-- | The tags associated with the transitive virtual interface.
ntviTags :: Lens' NewTransitVirtualInterface (Maybe (NonEmpty Tag))
ntviTags = lens _ntviTags (\s a -> s {_ntviTags = a}) . mapping _List1

instance Hashable NewTransitVirtualInterface

instance NFData NewTransitVirtualInterface

instance ToJSON NewTransitVirtualInterface where
  toJSON NewTransitVirtualInterface' {..} =
    object
      ( catMaybes
          [ ("mtu" .=) <$> _ntviMtu,
            ("customerAddress" .=) <$> _ntviCustomerAddress,
            ("vlan" .=) <$> _ntviVlan,
            ("amazonAddress" .=) <$> _ntviAmazonAddress,
            ("addressFamily" .=) <$> _ntviAddressFamily,
            ("directConnectGatewayId" .=) <$> _ntviDirectConnectGatewayId,
            ("asn" .=) <$> _ntviAsn,
            ("authKey" .=) <$> _ntviAuthKey,
            ("virtualInterfaceName" .=) <$> _ntviVirtualInterfaceName,
            ("tags" .=) <$> _ntviTags
          ]
      )

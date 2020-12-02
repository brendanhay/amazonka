{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a transit virtual interface to be provisioned on a connection.
--
--
--
-- /See:/ 'newTransitVirtualInterfaceAllocation' smart constructor.
data NewTransitVirtualInterfaceAllocation = NewTransitVirtualInterfaceAllocation'
  { _ntviaMtu ::
      !(Maybe Int),
    _ntviaCustomerAddress ::
      !(Maybe Text),
    _ntviaVlan ::
      !(Maybe Int),
    _ntviaAmazonAddress ::
      !(Maybe Text),
    _ntviaAddressFamily ::
      !( Maybe
           AddressFamily
       ),
    _ntviaAsn ::
      !(Maybe Int),
    _ntviaAuthKey ::
      !(Maybe Text),
    _ntviaVirtualInterfaceName ::
      !(Maybe Text),
    _ntviaTags ::
      !( Maybe
           (List1 Tag)
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewTransitVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntviaMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'ntviaCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'ntviaVlan' - The ID of the VLAN.
--
-- * 'ntviaAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'ntviaAddressFamily' - The address family for the BGP peer.
--
-- * 'ntviaAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
--
-- * 'ntviaAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- * 'ntviaVirtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- * 'ntviaTags' - The tags associated with the transitive virtual interface.
newTransitVirtualInterfaceAllocation ::
  NewTransitVirtualInterfaceAllocation
newTransitVirtualInterfaceAllocation =
  NewTransitVirtualInterfaceAllocation'
    { _ntviaMtu = Nothing,
      _ntviaCustomerAddress = Nothing,
      _ntviaVlan = Nothing,
      _ntviaAmazonAddress = Nothing,
      _ntviaAddressFamily = Nothing,
      _ntviaAsn = Nothing,
      _ntviaAuthKey = Nothing,
      _ntviaVirtualInterfaceName = Nothing,
      _ntviaTags = Nothing
    }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
ntviaMtu :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Int)
ntviaMtu = lens _ntviaMtu (\s a -> s {_ntviaMtu = a})

-- | The IP address assigned to the customer interface.
ntviaCustomerAddress :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Text)
ntviaCustomerAddress = lens _ntviaCustomerAddress (\s a -> s {_ntviaCustomerAddress = a})

-- | The ID of the VLAN.
ntviaVlan :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Int)
ntviaVlan = lens _ntviaVlan (\s a -> s {_ntviaVlan = a})

-- | The IP address assigned to the Amazon interface.
ntviaAmazonAddress :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Text)
ntviaAmazonAddress = lens _ntviaAmazonAddress (\s a -> s {_ntviaAmazonAddress = a})

-- | The address family for the BGP peer.
ntviaAddressFamily :: Lens' NewTransitVirtualInterfaceAllocation (Maybe AddressFamily)
ntviaAddressFamily = lens _ntviaAddressFamily (\s a -> s {_ntviaAddressFamily = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
ntviaAsn :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Int)
ntviaAsn = lens _ntviaAsn (\s a -> s {_ntviaAsn = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
ntviaAuthKey :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Text)
ntviaAuthKey = lens _ntviaAuthKey (\s a -> s {_ntviaAuthKey = a})

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
ntviaVirtualInterfaceName :: Lens' NewTransitVirtualInterfaceAllocation (Maybe Text)
ntviaVirtualInterfaceName = lens _ntviaVirtualInterfaceName (\s a -> s {_ntviaVirtualInterfaceName = a})

-- | The tags associated with the transitive virtual interface.
ntviaTags :: Lens' NewTransitVirtualInterfaceAllocation (Maybe (NonEmpty Tag))
ntviaTags = lens _ntviaTags (\s a -> s {_ntviaTags = a}) . mapping _List1

instance Hashable NewTransitVirtualInterfaceAllocation

instance NFData NewTransitVirtualInterfaceAllocation

instance ToJSON NewTransitVirtualInterfaceAllocation where
  toJSON NewTransitVirtualInterfaceAllocation' {..} =
    object
      ( catMaybes
          [ ("mtu" .=) <$> _ntviaMtu,
            ("customerAddress" .=) <$> _ntviaCustomerAddress,
            ("vlan" .=) <$> _ntviaVlan,
            ("amazonAddress" .=) <$> _ntviaAmazonAddress,
            ("addressFamily" .=) <$> _ntviaAddressFamily,
            ("asn" .=) <$> _ntviaAsn,
            ("authKey" .=) <$> _ntviaAuthKey,
            ("virtualInterfaceName" .=) <$> _ntviaVirtualInterfaceName,
            ("tags" .=) <$> _ntviaTags
          ]
      )

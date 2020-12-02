{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a private virtual interface to be provisioned on a connection.
--
--
--
-- /See:/ 'newPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { _npviaMtu ::
      !(Maybe Int),
    _npviaCustomerAddress ::
      !(Maybe Text),
    _npviaAmazonAddress ::
      !(Maybe Text),
    _npviaAddressFamily ::
      !( Maybe
           AddressFamily
       ),
    _npviaAuthKey ::
      !(Maybe Text),
    _npviaTags ::
      !( Maybe
           (List1 Tag)
       ),
    _npviaVirtualInterfaceName ::
      !Text,
    _npviaVlan ::
      !Int,
    _npviaAsn :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewPrivateVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npviaMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'npviaCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'npviaAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'npviaAddressFamily' - The address family for the BGP peer.
--
-- * 'npviaAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- * 'npviaTags' - The tags associated with the private virtual interface.
--
-- * 'npviaVirtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- * 'npviaVlan' - The ID of the VLAN.
--
-- * 'npviaAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
newPrivateVirtualInterfaceAllocation ::
  -- | 'npviaVirtualInterfaceName'
  Text ->
  -- | 'npviaVlan'
  Int ->
  -- | 'npviaAsn'
  Int ->
  NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPrivateVirtualInterfaceAllocation'
      { _npviaMtu = Nothing,
        _npviaCustomerAddress = Nothing,
        _npviaAmazonAddress = Nothing,
        _npviaAddressFamily = Nothing,
        _npviaAuthKey = Nothing,
        _npviaTags = Nothing,
        _npviaVirtualInterfaceName = pVirtualInterfaceName_,
        _npviaVlan = pVlan_,
        _npviaAsn = pAsn_
      }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
npviaMtu :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Int)
npviaMtu = lens _npviaMtu (\s a -> s {_npviaMtu = a})

-- | The IP address assigned to the customer interface.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\s a -> s {_npviaCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\s a -> s {_npviaAmazonAddress = a})

-- | The address family for the BGP peer.
npviaAddressFamily :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe AddressFamily)
npviaAddressFamily = lens _npviaAddressFamily (\s a -> s {_npviaAddressFamily = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\s a -> s {_npviaAuthKey = a})

-- | The tags associated with the private virtual interface.
npviaTags :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe (NonEmpty Tag))
npviaTags = lens _npviaTags (\s a -> s {_npviaTags = a}) . mapping _List1

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\s a -> s {_npviaVirtualInterfaceName = a})

-- | The ID of the VLAN.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\s a -> s {_npviaVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaAsn = lens _npviaAsn (\s a -> s {_npviaAsn = a})

instance Hashable NewPrivateVirtualInterfaceAllocation

instance NFData NewPrivateVirtualInterfaceAllocation

instance ToJSON NewPrivateVirtualInterfaceAllocation where
  toJSON NewPrivateVirtualInterfaceAllocation' {..} =
    object
      ( catMaybes
          [ ("mtu" .=) <$> _npviaMtu,
            ("customerAddress" .=) <$> _npviaCustomerAddress,
            ("amazonAddress" .=) <$> _npviaAmazonAddress,
            ("addressFamily" .=) <$> _npviaAddressFamily,
            ("authKey" .=) <$> _npviaAuthKey,
            ("tags" .=) <$> _npviaTags,
            Just ("virtualInterfaceName" .= _npviaVirtualInterfaceName),
            Just ("vlan" .= _npviaVlan),
            Just ("asn" .= _npviaAsn)
          ]
      )

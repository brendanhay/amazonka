{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a public virtual interface to be provisioned on a connection.
--
--
--
-- /See:/ 'newPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
  { _newRouteFilterPrefixes ::
      !( Maybe
           [RouteFilterPrefix]
       ),
    _newCustomerAddress ::
      !(Maybe Text),
    _newAmazonAddress ::
      !(Maybe Text),
    _newAddressFamily ::
      !( Maybe
           AddressFamily
       ),
    _newAuthKey ::
      !(Maybe Text),
    _newTags ::
      !( Maybe
           (List1 Tag)
       ),
    _newVirtualInterfaceName ::
      !Text,
    _newVlan :: !Int,
    _newAsn :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewPublicVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'newRouteFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- * 'newCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'newAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'newAddressFamily' - The address family for the BGP peer.
--
-- * 'newAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- * 'newTags' - The tags associated with the public virtual interface.
--
-- * 'newVirtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- * 'newVlan' - The ID of the VLAN.
--
-- * 'newAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
newPublicVirtualInterfaceAllocation ::
  -- | 'newVirtualInterfaceName'
  Text ->
  -- | 'newVlan'
  Int ->
  -- | 'newAsn'
  Int ->
  NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPublicVirtualInterfaceAllocation'
      { _newRouteFilterPrefixes =
          Nothing,
        _newCustomerAddress = Nothing,
        _newAmazonAddress = Nothing,
        _newAddressFamily = Nothing,
        _newAuthKey = Nothing,
        _newTags = Nothing,
        _newVirtualInterfaceName = pVirtualInterfaceName_,
        _newVlan = pVlan_,
        _newAsn = pAsn_
      }

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
newRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
newRouteFilterPrefixes = lens _newRouteFilterPrefixes (\s a -> s {_newRouteFilterPrefixes = a}) . _Default . _Coerce

-- | The IP address assigned to the customer interface.
newCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newCustomerAddress = lens _newCustomerAddress (\s a -> s {_newCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
newAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAmazonAddress = lens _newAmazonAddress (\s a -> s {_newAmazonAddress = a})

-- | The address family for the BGP peer.
newAddressFamily :: Lens' NewPublicVirtualInterfaceAllocation (Maybe AddressFamily)
newAddressFamily = lens _newAddressFamily (\s a -> s {_newAddressFamily = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
newAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAuthKey = lens _newAuthKey (\s a -> s {_newAuthKey = a})

-- | The tags associated with the public virtual interface.
newTags :: Lens' NewPublicVirtualInterfaceAllocation (Maybe (NonEmpty Tag))
newTags = lens _newTags (\s a -> s {_newTags = a}) . mapping _List1

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
newVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
newVirtualInterfaceName = lens _newVirtualInterfaceName (\s a -> s {_newVirtualInterfaceName = a})

-- | The ID of the VLAN.
newVlan :: Lens' NewPublicVirtualInterfaceAllocation Int
newVlan = lens _newVlan (\s a -> s {_newVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration. The valid values are 1-2147483647.
newAsn :: Lens' NewPublicVirtualInterfaceAllocation Int
newAsn = lens _newAsn (\s a -> s {_newAsn = a})

instance Hashable NewPublicVirtualInterfaceAllocation

instance NFData NewPublicVirtualInterfaceAllocation

instance ToJSON NewPublicVirtualInterfaceAllocation where
  toJSON NewPublicVirtualInterfaceAllocation' {..} =
    object
      ( catMaybes
          [ ("routeFilterPrefixes" .=) <$> _newRouteFilterPrefixes,
            ("customerAddress" .=) <$> _newCustomerAddress,
            ("amazonAddress" .=) <$> _newAmazonAddress,
            ("addressFamily" .=) <$> _newAddressFamily,
            ("authKey" .=) <$> _newAuthKey,
            ("tags" .=) <$> _newTags,
            Just ("virtualInterfaceName" .= _newVirtualInterfaceName),
            Just ("vlan" .= _newVlan),
            Just ("asn" .= _newAsn)
          ]
      )

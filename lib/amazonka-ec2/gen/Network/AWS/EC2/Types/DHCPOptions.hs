{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DHCPOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DHCPConfiguration
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a set of DHCP options.
--
--
--
-- /See:/ 'dhcpOptions' smart constructor.
data DHCPOptions = DHCPOptions'
  { _doDHCPConfigurations ::
      !(Maybe [DHCPConfiguration]),
    _doOwnerId :: !(Maybe Text),
    _doDHCPOptionsId :: !(Maybe Text),
    _doTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doDHCPConfigurations' - One or more DHCP options in the set.
--
-- * 'doOwnerId' - The ID of the AWS account that owns the DHCP options set.
--
-- * 'doDHCPOptionsId' - The ID of the set of DHCP options.
--
-- * 'doTags' - Any tags assigned to the DHCP options set.
dhcpOptions ::
  DHCPOptions
dhcpOptions =
  DHCPOptions'
    { _doDHCPConfigurations = Nothing,
      _doOwnerId = Nothing,
      _doDHCPOptionsId = Nothing,
      _doTags = Nothing
    }

-- | One or more DHCP options in the set.
doDHCPConfigurations :: Lens' DHCPOptions [DHCPConfiguration]
doDHCPConfigurations = lens _doDHCPConfigurations (\s a -> s {_doDHCPConfigurations = a}) . _Default . _Coerce

-- | The ID of the AWS account that owns the DHCP options set.
doOwnerId :: Lens' DHCPOptions (Maybe Text)
doOwnerId = lens _doOwnerId (\s a -> s {_doOwnerId = a})

-- | The ID of the set of DHCP options.
doDHCPOptionsId :: Lens' DHCPOptions (Maybe Text)
doDHCPOptionsId = lens _doDHCPOptionsId (\s a -> s {_doDHCPOptionsId = a})

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DHCPOptions [Tag]
doTags = lens _doTags (\s a -> s {_doTags = a}) . _Default . _Coerce

instance FromXML DHCPOptions where
  parseXML x =
    DHCPOptions'
      <$> ( x .@? "dhcpConfigurationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "ownerId")
      <*> (x .@? "dhcpOptionsId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable DHCPOptions

instance NFData DHCPOptions

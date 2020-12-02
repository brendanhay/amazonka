{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DHCPConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DHCPConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a DHCP configuration option.
--
--
--
-- /See:/ 'dhcpConfiguration' smart constructor.
data DHCPConfiguration = DHCPConfiguration'
  { _dcValues ::
      !(Maybe [AttributeValue]),
    _dcKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DHCPConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcValues' - One or more values for the DHCP option.
--
-- * 'dcKey' - The name of a DHCP option.
dhcpConfiguration ::
  DHCPConfiguration
dhcpConfiguration =
  DHCPConfiguration' {_dcValues = Nothing, _dcKey = Nothing}

-- | One or more values for the DHCP option.
dcValues :: Lens' DHCPConfiguration [AttributeValue]
dcValues = lens _dcValues (\s a -> s {_dcValues = a}) . _Default . _Coerce

-- | The name of a DHCP option.
dcKey :: Lens' DHCPConfiguration (Maybe Text)
dcKey = lens _dcKey (\s a -> s {_dcKey = a})

instance FromXML DHCPConfiguration where
  parseXML x =
    DHCPConfiguration'
      <$> (x .@? "valueSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "key")

instance Hashable DHCPConfiguration

instance NFData DHCPConfiguration

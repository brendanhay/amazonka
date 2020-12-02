{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6CidrAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6CidrAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 CIDR block association.
--
--
--
-- /See:/ 'ipv6CidrAssociation' smart constructor.
data IPv6CidrAssociation = IPv6CidrAssociation'
  { _icaAssociatedResource ::
      !(Maybe Text),
    _icaIPv6Cidr :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPv6CidrAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icaAssociatedResource' - The resource that's associated with the IPv6 CIDR block.
--
-- * 'icaIPv6Cidr' - The IPv6 CIDR block.
ipv6CidrAssociation ::
  IPv6CidrAssociation
ipv6CidrAssociation =
  IPv6CidrAssociation'
    { _icaAssociatedResource = Nothing,
      _icaIPv6Cidr = Nothing
    }

-- | The resource that's associated with the IPv6 CIDR block.
icaAssociatedResource :: Lens' IPv6CidrAssociation (Maybe Text)
icaAssociatedResource = lens _icaAssociatedResource (\s a -> s {_icaAssociatedResource = a})

-- | The IPv6 CIDR block.
icaIPv6Cidr :: Lens' IPv6CidrAssociation (Maybe Text)
icaIPv6Cidr = lens _icaIPv6Cidr (\s a -> s {_icaIPv6Cidr = a})

instance FromXML IPv6CidrAssociation where
  parseXML x =
    IPv6CidrAssociation'
      <$> (x .@? "associatedResource") <*> (x .@? "ipv6Cidr")

instance Hashable IPv6CidrAssociation

instance NFData IPv6CidrAssociation

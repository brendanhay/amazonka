{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6CidrBlock where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 CIDR block.
--
--
--
-- /See:/ 'ipv6CidrBlock' smart constructor.
newtype IPv6CidrBlock = IPv6CidrBlock'
  { _icbIPv6CidrBlock ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPv6CidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icbIPv6CidrBlock' - The IPv6 CIDR block.
ipv6CidrBlock ::
  IPv6CidrBlock
ipv6CidrBlock = IPv6CidrBlock' {_icbIPv6CidrBlock = Nothing}

-- | The IPv6 CIDR block.
icbIPv6CidrBlock :: Lens' IPv6CidrBlock (Maybe Text)
icbIPv6CidrBlock = lens _icbIPv6CidrBlock (\s a -> s {_icbIPv6CidrBlock = a})

instance FromXML IPv6CidrBlock where
  parseXML x = IPv6CidrBlock' <$> (x .@? "ipv6CidrBlock")

instance Hashable IPv6CidrBlock

instance NFData IPv6CidrBlock

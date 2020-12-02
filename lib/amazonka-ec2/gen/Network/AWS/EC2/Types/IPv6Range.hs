{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6Range where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | [EC2-VPC only] Describes an IPv6 range.
--
--
--
-- /See:/ 'ipv6Range' smart constructor.
data IPv6Range = IPv6Range'
  { _irCidrIPv6 :: !(Maybe Text),
    _irDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPv6Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irCidrIPv6' - The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
--
-- * 'irDescription' - A description for the security group rule that references this IPv6 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
ipv6Range ::
  IPv6Range
ipv6Range =
  IPv6Range' {_irCidrIPv6 = Nothing, _irDescription = Nothing}

-- | The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
irCidrIPv6 :: Lens' IPv6Range (Maybe Text)
irCidrIPv6 = lens _irCidrIPv6 (\s a -> s {_irCidrIPv6 = a})

-- | A description for the security group rule that references this IPv6 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
irDescription :: Lens' IPv6Range (Maybe Text)
irDescription = lens _irDescription (\s a -> s {_irDescription = a})

instance FromXML IPv6Range where
  parseXML x =
    IPv6Range' <$> (x .@? "cidrIpv6") <*> (x .@? "description")

instance Hashable IPv6Range

instance NFData IPv6Range

instance ToQuery IPv6Range where
  toQuery IPv6Range' {..} =
    mconcat
      ["CidrIpv6" =: _irCidrIPv6, "Description" =: _irDescription]

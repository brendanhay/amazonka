{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPRange where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv4 range.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _iprDescription :: !(Maybe Text),
    _iprCidrIP :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprDescription' - A description for the security group rule that references this IPv4 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- * 'iprCidrIP' - The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
ipRange ::
  -- | 'iprCidrIP'
  Text ->
  IPRange
ipRange pCidrIP_ =
  IPRange' {_iprDescription = Nothing, _iprCidrIP = pCidrIP_}

-- | A description for the security group rule that references this IPv4 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
iprDescription :: Lens' IPRange (Maybe Text)
iprDescription = lens _iprDescription (\s a -> s {_iprDescription = a})

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
iprCidrIP :: Lens' IPRange Text
iprCidrIP = lens _iprCidrIP (\s a -> s {_iprCidrIP = a})

instance FromXML IPRange where
  parseXML x = IPRange' <$> (x .@? "description") <*> (x .@ "cidrIp")

instance Hashable IPRange

instance NFData IPRange

instance ToQuery IPRange where
  toQuery IPRange' {..} =
    mconcat
      ["Description" =: _iprDescription, "CidrIp" =: _iprCidrIP]

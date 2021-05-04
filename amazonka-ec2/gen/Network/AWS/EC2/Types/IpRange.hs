{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IpRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IpRange where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv4 range.
--
-- /See:/ 'newIpRange' smart constructor.
data IpRange = IpRange'
  { -- | A description for the security group rule that references this IPv4
    -- address range.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
    description :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR range. You can either specify a CIDR range or a source
    -- security group, not both. To specify a single IPv4 address, use the \/32
    -- prefix length.
    cidrIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IpRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'ipRange_description' - A description for the security group rule that references this IPv4
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
--
-- 'cidrIp', 'ipRange_cidrIp' - The IPv4 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv4 address, use the \/32
-- prefix length.
newIpRange ::
  -- | 'cidrIp'
  Prelude.Text ->
  IpRange
newIpRange pCidrIp_ =
  IpRange'
    { description = Prelude.Nothing,
      cidrIp = pCidrIp_
    }

-- | A description for the security group rule that references this IPv4
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
ipRange_description :: Lens.Lens' IpRange (Prelude.Maybe Prelude.Text)
ipRange_description = Lens.lens (\IpRange' {description} -> description) (\s@IpRange' {} a -> s {description = a} :: IpRange)

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv4 address, use the \/32
-- prefix length.
ipRange_cidrIp :: Lens.Lens' IpRange Prelude.Text
ipRange_cidrIp = Lens.lens (\IpRange' {cidrIp} -> cidrIp) (\s@IpRange' {} a -> s {cidrIp = a} :: IpRange)

instance Prelude.FromXML IpRange where
  parseXML x =
    IpRange'
      Prelude.<$> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@ "cidrIp")

instance Prelude.Hashable IpRange

instance Prelude.NFData IpRange

instance Prelude.ToQuery IpRange where
  toQuery IpRange' {..} =
    Prelude.mconcat
      [ "Description" Prelude.=: description,
        "CidrIp" Prelude.=: cidrIp
      ]

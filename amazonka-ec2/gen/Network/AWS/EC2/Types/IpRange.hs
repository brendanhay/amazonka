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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv4 range.
--
-- /See:/ 'newIpRange' smart constructor.
data IpRange = IpRange'
  { -- | A description for the security group rule that references this IPv4
    -- address range.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
    description :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR range. You can either specify a CIDR range or a source
    -- security group, not both. To specify a single IPv4 address, use the \/32
    -- prefix length.
    cidrIp :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  IpRange
newIpRange pCidrIp_ =
  IpRange'
    { description = Core.Nothing,
      cidrIp = pCidrIp_
    }

-- | A description for the security group rule that references this IPv4
-- address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=&;{}!$*
ipRange_description :: Lens.Lens' IpRange (Core.Maybe Core.Text)
ipRange_description = Lens.lens (\IpRange' {description} -> description) (\s@IpRange' {} a -> s {description = a} :: IpRange)

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source
-- security group, not both. To specify a single IPv4 address, use the \/32
-- prefix length.
ipRange_cidrIp :: Lens.Lens' IpRange Core.Text
ipRange_cidrIp = Lens.lens (\IpRange' {cidrIp} -> cidrIp) (\s@IpRange' {} a -> s {cidrIp = a} :: IpRange)

instance Core.FromXML IpRange where
  parseXML x =
    IpRange'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@ "cidrIp")

instance Core.Hashable IpRange

instance Core.NFData IpRange

instance Core.ToQuery IpRange where
  toQuery IpRange' {..} =
    Core.mconcat
      [ "Description" Core.=: description,
        "CidrIp" Core.=: cidrIp
      ]

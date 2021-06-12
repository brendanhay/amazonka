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
-- Module      : Network.AWS.EC2.Types.Ipv6CidrAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Ipv6CidrAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv6 CIDR block association.
--
-- /See:/ 'newIpv6CidrAssociation' smart constructor.
data Ipv6CidrAssociation = Ipv6CidrAssociation'
  { -- | The IPv6 CIDR block.
    ipv6Cidr :: Core.Maybe Core.Text,
    -- | The resource that\'s associated with the IPv6 CIDR block.
    associatedResource :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Ipv6CidrAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Cidr', 'ipv6CidrAssociation_ipv6Cidr' - The IPv6 CIDR block.
--
-- 'associatedResource', 'ipv6CidrAssociation_associatedResource' - The resource that\'s associated with the IPv6 CIDR block.
newIpv6CidrAssociation ::
  Ipv6CidrAssociation
newIpv6CidrAssociation =
  Ipv6CidrAssociation'
    { ipv6Cidr = Core.Nothing,
      associatedResource = Core.Nothing
    }

-- | The IPv6 CIDR block.
ipv6CidrAssociation_ipv6Cidr :: Lens.Lens' Ipv6CidrAssociation (Core.Maybe Core.Text)
ipv6CidrAssociation_ipv6Cidr = Lens.lens (\Ipv6CidrAssociation' {ipv6Cidr} -> ipv6Cidr) (\s@Ipv6CidrAssociation' {} a -> s {ipv6Cidr = a} :: Ipv6CidrAssociation)

-- | The resource that\'s associated with the IPv6 CIDR block.
ipv6CidrAssociation_associatedResource :: Lens.Lens' Ipv6CidrAssociation (Core.Maybe Core.Text)
ipv6CidrAssociation_associatedResource = Lens.lens (\Ipv6CidrAssociation' {associatedResource} -> associatedResource) (\s@Ipv6CidrAssociation' {} a -> s {associatedResource = a} :: Ipv6CidrAssociation)

instance Core.FromXML Ipv6CidrAssociation where
  parseXML x =
    Ipv6CidrAssociation'
      Core.<$> (x Core..@? "ipv6Cidr")
      Core.<*> (x Core..@? "associatedResource")

instance Core.Hashable Ipv6CidrAssociation

instance Core.NFData Ipv6CidrAssociation

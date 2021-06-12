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
-- Module      : Network.AWS.EC2.Types.StaleIpPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StaleIpPermission where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserIdGroupPair
import qualified Network.AWS.Lens as Lens

-- | Describes a stale rule in a security group.
--
-- /See:/ 'newStaleIpPermission' smart constructor.
data StaleIpPermission = StaleIpPermission'
  { -- | The start of the port range for the TCP and UDP protocols, or an ICMP
    -- type number. A value of @-1@ indicates all ICMP types.
    fromPort :: Core.Maybe Core.Int,
    -- | The prefix list IDs. Not applicable for stale security group rules.
    prefixListIds :: Core.Maybe [Core.Text],
    -- | The IP ranges. Not applicable for stale security group rules.
    ipRanges :: Core.Maybe [Core.Text],
    -- | The security group pairs. Returns the ID of the referenced security
    -- group and VPC, and the ID and status of the VPC peering connection.
    userIdGroupPairs :: Core.Maybe [UserIdGroupPair],
    -- | The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
    ipProtocol :: Core.Maybe Core.Text,
    -- | The end of the port range for the TCP and UDP protocols, or an ICMP type
    -- number. A value of @-1@ indicates all ICMP types.
    toPort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StaleIpPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'staleIpPermission_fromPort' - The start of the port range for the TCP and UDP protocols, or an ICMP
-- type number. A value of @-1@ indicates all ICMP types.
--
-- 'prefixListIds', 'staleIpPermission_prefixListIds' - The prefix list IDs. Not applicable for stale security group rules.
--
-- 'ipRanges', 'staleIpPermission_ipRanges' - The IP ranges. Not applicable for stale security group rules.
--
-- 'userIdGroupPairs', 'staleIpPermission_userIdGroupPairs' - The security group pairs. Returns the ID of the referenced security
-- group and VPC, and the ID and status of the VPC peering connection.
--
-- 'ipProtocol', 'staleIpPermission_ipProtocol' - The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
--
-- 'toPort', 'staleIpPermission_toPort' - The end of the port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of @-1@ indicates all ICMP types.
newStaleIpPermission ::
  StaleIpPermission
newStaleIpPermission =
  StaleIpPermission'
    { fromPort = Core.Nothing,
      prefixListIds = Core.Nothing,
      ipRanges = Core.Nothing,
      userIdGroupPairs = Core.Nothing,
      ipProtocol = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an ICMP
-- type number. A value of @-1@ indicates all ICMP types.
staleIpPermission_fromPort :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Int)
staleIpPermission_fromPort = Lens.lens (\StaleIpPermission' {fromPort} -> fromPort) (\s@StaleIpPermission' {} a -> s {fromPort = a} :: StaleIpPermission)

-- | The prefix list IDs. Not applicable for stale security group rules.
staleIpPermission_prefixListIds :: Lens.Lens' StaleIpPermission (Core.Maybe [Core.Text])
staleIpPermission_prefixListIds = Lens.lens (\StaleIpPermission' {prefixListIds} -> prefixListIds) (\s@StaleIpPermission' {} a -> s {prefixListIds = a} :: StaleIpPermission) Core.. Lens.mapping Lens._Coerce

-- | The IP ranges. Not applicable for stale security group rules.
staleIpPermission_ipRanges :: Lens.Lens' StaleIpPermission (Core.Maybe [Core.Text])
staleIpPermission_ipRanges = Lens.lens (\StaleIpPermission' {ipRanges} -> ipRanges) (\s@StaleIpPermission' {} a -> s {ipRanges = a} :: StaleIpPermission) Core.. Lens.mapping Lens._Coerce

-- | The security group pairs. Returns the ID of the referenced security
-- group and VPC, and the ID and status of the VPC peering connection.
staleIpPermission_userIdGroupPairs :: Lens.Lens' StaleIpPermission (Core.Maybe [UserIdGroupPair])
staleIpPermission_userIdGroupPairs = Lens.lens (\StaleIpPermission' {userIdGroupPairs} -> userIdGroupPairs) (\s@StaleIpPermission' {} a -> s {userIdGroupPairs = a} :: StaleIpPermission) Core.. Lens.mapping Lens._Coerce

-- | The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
staleIpPermission_ipProtocol :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Text)
staleIpPermission_ipProtocol = Lens.lens (\StaleIpPermission' {ipProtocol} -> ipProtocol) (\s@StaleIpPermission' {} a -> s {ipProtocol = a} :: StaleIpPermission)

-- | The end of the port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of @-1@ indicates all ICMP types.
staleIpPermission_toPort :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Int)
staleIpPermission_toPort = Lens.lens (\StaleIpPermission' {toPort} -> toPort) (\s@StaleIpPermission' {} a -> s {toPort = a} :: StaleIpPermission)

instance Core.FromXML StaleIpPermission where
  parseXML x =
    StaleIpPermission'
      Core.<$> (x Core..@? "fromPort")
      Core.<*> ( x Core..@? "prefixListIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "ipRanges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "groups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "ipProtocol")
      Core.<*> (x Core..@? "toPort")

instance Core.Hashable StaleIpPermission

instance Core.NFData StaleIpPermission

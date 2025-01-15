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
-- Module      : Amazonka.EC2.Types.StaleIpPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.StaleIpPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UserIdGroupPair
import qualified Amazonka.Prelude as Prelude

-- | Describes a stale rule in a security group.
--
-- /See:/ 'newStaleIpPermission' smart constructor.
data StaleIpPermission = StaleIpPermission'
  { -- | The start of the port range for the TCP and UDP protocols, or an ICMP
    -- type number. A value of @-1@ indicates all ICMP types.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The IP ranges. Not applicable for stale security group rules.
    ipRanges :: Prelude.Maybe [Prelude.Text],
    -- | The prefix list IDs. Not applicable for stale security group rules.
    prefixListIds :: Prelude.Maybe [Prelude.Text],
    -- | The end of the port range for the TCP and UDP protocols, or an ICMP type
    -- number. A value of @-1@ indicates all ICMP types.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The security group pairs. Returns the ID of the referenced security
    -- group and VPC, and the ID and status of the VPC peering connection.
    userIdGroupPairs :: Prelude.Maybe [UserIdGroupPair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'ipProtocol', 'staleIpPermission_ipProtocol' - The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
--
-- 'ipRanges', 'staleIpPermission_ipRanges' - The IP ranges. Not applicable for stale security group rules.
--
-- 'prefixListIds', 'staleIpPermission_prefixListIds' - The prefix list IDs. Not applicable for stale security group rules.
--
-- 'toPort', 'staleIpPermission_toPort' - The end of the port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of @-1@ indicates all ICMP types.
--
-- 'userIdGroupPairs', 'staleIpPermission_userIdGroupPairs' - The security group pairs. Returns the ID of the referenced security
-- group and VPC, and the ID and status of the VPC peering connection.
newStaleIpPermission ::
  StaleIpPermission
newStaleIpPermission =
  StaleIpPermission'
    { fromPort = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      prefixListIds = Prelude.Nothing,
      toPort = Prelude.Nothing,
      userIdGroupPairs = Prelude.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an ICMP
-- type number. A value of @-1@ indicates all ICMP types.
staleIpPermission_fromPort :: Lens.Lens' StaleIpPermission (Prelude.Maybe Prelude.Int)
staleIpPermission_fromPort = Lens.lens (\StaleIpPermission' {fromPort} -> fromPort) (\s@StaleIpPermission' {} a -> s {fromPort = a} :: StaleIpPermission)

-- | The IP protocol name (for @tcp@, @udp@, and @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)>.
staleIpPermission_ipProtocol :: Lens.Lens' StaleIpPermission (Prelude.Maybe Prelude.Text)
staleIpPermission_ipProtocol = Lens.lens (\StaleIpPermission' {ipProtocol} -> ipProtocol) (\s@StaleIpPermission' {} a -> s {ipProtocol = a} :: StaleIpPermission)

-- | The IP ranges. Not applicable for stale security group rules.
staleIpPermission_ipRanges :: Lens.Lens' StaleIpPermission (Prelude.Maybe [Prelude.Text])
staleIpPermission_ipRanges = Lens.lens (\StaleIpPermission' {ipRanges} -> ipRanges) (\s@StaleIpPermission' {} a -> s {ipRanges = a} :: StaleIpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The prefix list IDs. Not applicable for stale security group rules.
staleIpPermission_prefixListIds :: Lens.Lens' StaleIpPermission (Prelude.Maybe [Prelude.Text])
staleIpPermission_prefixListIds = Lens.lens (\StaleIpPermission' {prefixListIds} -> prefixListIds) (\s@StaleIpPermission' {} a -> s {prefixListIds = a} :: StaleIpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The end of the port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of @-1@ indicates all ICMP types.
staleIpPermission_toPort :: Lens.Lens' StaleIpPermission (Prelude.Maybe Prelude.Int)
staleIpPermission_toPort = Lens.lens (\StaleIpPermission' {toPort} -> toPort) (\s@StaleIpPermission' {} a -> s {toPort = a} :: StaleIpPermission)

-- | The security group pairs. Returns the ID of the referenced security
-- group and VPC, and the ID and status of the VPC peering connection.
staleIpPermission_userIdGroupPairs :: Lens.Lens' StaleIpPermission (Prelude.Maybe [UserIdGroupPair])
staleIpPermission_userIdGroupPairs = Lens.lens (\StaleIpPermission' {userIdGroupPairs} -> userIdGroupPairs) (\s@StaleIpPermission' {} a -> s {userIdGroupPairs = a} :: StaleIpPermission) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML StaleIpPermission where
  parseXML x =
    StaleIpPermission'
      Prelude.<$> (x Data..@? "fromPort")
      Prelude.<*> (x Data..@? "ipProtocol")
      Prelude.<*> ( x Data..@? "ipRanges" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "prefixListIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "toPort")
      Prelude.<*> ( x Data..@? "groups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable StaleIpPermission where
  hashWithSalt _salt StaleIpPermission' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` ipProtocol
      `Prelude.hashWithSalt` ipRanges
      `Prelude.hashWithSalt` prefixListIds
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` userIdGroupPairs

instance Prelude.NFData StaleIpPermission where
  rnf StaleIpPermission' {..} =
    Prelude.rnf fromPort `Prelude.seq`
      Prelude.rnf ipProtocol `Prelude.seq`
        Prelude.rnf ipRanges `Prelude.seq`
          Prelude.rnf prefixListIds `Prelude.seq`
            Prelude.rnf toPort `Prelude.seq`
              Prelude.rnf userIdGroupPairs

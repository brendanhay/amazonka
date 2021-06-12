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
-- Module      : Network.AWS.EC2.Types.DnsEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DnsEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a DNS entry.
--
-- /See:/ 'newDnsEntry' smart constructor.
data DnsEntry = DnsEntry'
  { -- | The ID of the private hosted zone.
    hostedZoneId :: Core.Maybe Core.Text,
    -- | The DNS name.
    dnsName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DnsEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'dnsEntry_hostedZoneId' - The ID of the private hosted zone.
--
-- 'dnsName', 'dnsEntry_dnsName' - The DNS name.
newDnsEntry ::
  DnsEntry
newDnsEntry =
  DnsEntry'
    { hostedZoneId = Core.Nothing,
      dnsName = Core.Nothing
    }

-- | The ID of the private hosted zone.
dnsEntry_hostedZoneId :: Lens.Lens' DnsEntry (Core.Maybe Core.Text)
dnsEntry_hostedZoneId = Lens.lens (\DnsEntry' {hostedZoneId} -> hostedZoneId) (\s@DnsEntry' {} a -> s {hostedZoneId = a} :: DnsEntry)

-- | The DNS name.
dnsEntry_dnsName :: Lens.Lens' DnsEntry (Core.Maybe Core.Text)
dnsEntry_dnsName = Lens.lens (\DnsEntry' {dnsName} -> dnsName) (\s@DnsEntry' {} a -> s {dnsName = a} :: DnsEntry)

instance Core.FromXML DnsEntry where
  parseXML x =
    DnsEntry'
      Core.<$> (x Core..@? "hostedZoneId")
      Core.<*> (x Core..@? "dnsName")

instance Core.Hashable DnsEntry

instance Core.NFData DnsEntry

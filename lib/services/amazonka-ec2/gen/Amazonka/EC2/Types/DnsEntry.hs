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
-- Module      : Amazonka.EC2.Types.DnsEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DnsEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a DNS entry.
--
-- /See:/ 'newDnsEntry' smart constructor.
data DnsEntry = DnsEntry'
  { -- | The DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the private hosted zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsName', 'dnsEntry_dnsName' - The DNS name.
--
-- 'hostedZoneId', 'dnsEntry_hostedZoneId' - The ID of the private hosted zone.
newDnsEntry ::
  DnsEntry
newDnsEntry =
  DnsEntry'
    { dnsName = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing
    }

-- | The DNS name.
dnsEntry_dnsName :: Lens.Lens' DnsEntry (Prelude.Maybe Prelude.Text)
dnsEntry_dnsName = Lens.lens (\DnsEntry' {dnsName} -> dnsName) (\s@DnsEntry' {} a -> s {dnsName = a} :: DnsEntry)

-- | The ID of the private hosted zone.
dnsEntry_hostedZoneId :: Lens.Lens' DnsEntry (Prelude.Maybe Prelude.Text)
dnsEntry_hostedZoneId = Lens.lens (\DnsEntry' {hostedZoneId} -> hostedZoneId) (\s@DnsEntry' {} a -> s {hostedZoneId = a} :: DnsEntry)

instance Data.FromXML DnsEntry where
  parseXML x =
    DnsEntry'
      Prelude.<$> (x Data..@? "dnsName")
      Prelude.<*> (x Data..@? "hostedZoneId")

instance Prelude.Hashable DnsEntry where
  hashWithSalt _salt DnsEntry' {..} =
    _salt
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData DnsEntry where
  rnf DnsEntry' {..} =
    Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf hostedZoneId

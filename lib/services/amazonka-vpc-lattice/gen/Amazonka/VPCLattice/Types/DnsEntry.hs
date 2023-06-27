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
-- Module      : Amazonka.VPCLattice.Types.DnsEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.DnsEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the DNS information of a service.
--
-- /See:/ 'newDnsEntry' smart constructor.
data DnsEntry = DnsEntry'
  { -- | The domain name of the service.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the hosted zone.
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
-- 'domainName', 'dnsEntry_domainName' - The domain name of the service.
--
-- 'hostedZoneId', 'dnsEntry_hostedZoneId' - The ID of the hosted zone.
newDnsEntry ::
  DnsEntry
newDnsEntry =
  DnsEntry'
    { domainName = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing
    }

-- | The domain name of the service.
dnsEntry_domainName :: Lens.Lens' DnsEntry (Prelude.Maybe Prelude.Text)
dnsEntry_domainName = Lens.lens (\DnsEntry' {domainName} -> domainName) (\s@DnsEntry' {} a -> s {domainName = a} :: DnsEntry)

-- | The ID of the hosted zone.
dnsEntry_hostedZoneId :: Lens.Lens' DnsEntry (Prelude.Maybe Prelude.Text)
dnsEntry_hostedZoneId = Lens.lens (\DnsEntry' {hostedZoneId} -> hostedZoneId) (\s@DnsEntry' {} a -> s {hostedZoneId = a} :: DnsEntry)

instance Data.FromJSON DnsEntry where
  parseJSON =
    Data.withObject
      "DnsEntry"
      ( \x ->
          DnsEntry'
            Prelude.<$> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "hostedZoneId")
      )

instance Prelude.Hashable DnsEntry where
  hashWithSalt _salt DnsEntry' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData DnsEntry where
  rnf DnsEntry' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf hostedZoneId

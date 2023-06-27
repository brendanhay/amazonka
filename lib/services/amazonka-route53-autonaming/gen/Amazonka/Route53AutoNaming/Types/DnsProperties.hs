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
-- Module      : Amazonka.Route53AutoNaming.Types.DnsProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.DnsProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.SOA

-- | A complex type that contains the ID for the Route 53 hosted zone that
-- Cloud Map creates when you create a namespace.
--
-- /See:/ 'newDnsProperties' smart constructor.
data DnsProperties = DnsProperties'
  { -- | The ID for the Route 53 hosted zone that Cloud Map creates when you
    -- create a namespace.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Start of Authority (SOA) record for the hosted zone.
    soa :: Prelude.Maybe SOA
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'dnsProperties_hostedZoneId' - The ID for the Route 53 hosted zone that Cloud Map creates when you
-- create a namespace.
--
-- 'soa', 'dnsProperties_soa' - Start of Authority (SOA) record for the hosted zone.
newDnsProperties ::
  DnsProperties
newDnsProperties =
  DnsProperties'
    { hostedZoneId = Prelude.Nothing,
      soa = Prelude.Nothing
    }

-- | The ID for the Route 53 hosted zone that Cloud Map creates when you
-- create a namespace.
dnsProperties_hostedZoneId :: Lens.Lens' DnsProperties (Prelude.Maybe Prelude.Text)
dnsProperties_hostedZoneId = Lens.lens (\DnsProperties' {hostedZoneId} -> hostedZoneId) (\s@DnsProperties' {} a -> s {hostedZoneId = a} :: DnsProperties)

-- | Start of Authority (SOA) record for the hosted zone.
dnsProperties_soa :: Lens.Lens' DnsProperties (Prelude.Maybe SOA)
dnsProperties_soa = Lens.lens (\DnsProperties' {soa} -> soa) (\s@DnsProperties' {} a -> s {soa = a} :: DnsProperties)

instance Data.FromJSON DnsProperties where
  parseJSON =
    Data.withObject
      "DnsProperties"
      ( \x ->
          DnsProperties'
            Prelude.<$> (x Data..:? "HostedZoneId")
            Prelude.<*> (x Data..:? "SOA")
      )

instance Prelude.Hashable DnsProperties where
  hashWithSalt _salt DnsProperties' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` soa

instance Prelude.NFData DnsProperties where
  rnf DnsProperties' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf soa

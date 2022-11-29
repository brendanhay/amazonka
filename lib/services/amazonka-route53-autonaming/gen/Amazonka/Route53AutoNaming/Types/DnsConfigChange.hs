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
-- Module      : Amazonka.Route53AutoNaming.Types.DnsConfigChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.DnsConfigChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.DnsRecord

-- | A complex type that contains information about changes to the Route 53
-- DNS records that Cloud Map creates when you register an instance.
--
-- /See:/ 'newDnsConfigChange' smart constructor.
data DnsConfigChange = DnsConfigChange'
  { -- | An array that contains one @DnsRecord@ object for each Route 53 record
    -- that you want Cloud Map to create when you register an instance.
    dnsRecords :: [DnsRecord]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsConfigChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsRecords', 'dnsConfigChange_dnsRecords' - An array that contains one @DnsRecord@ object for each Route 53 record
-- that you want Cloud Map to create when you register an instance.
newDnsConfigChange ::
  DnsConfigChange
newDnsConfigChange =
  DnsConfigChange' {dnsRecords = Prelude.mempty}

-- | An array that contains one @DnsRecord@ object for each Route 53 record
-- that you want Cloud Map to create when you register an instance.
dnsConfigChange_dnsRecords :: Lens.Lens' DnsConfigChange [DnsRecord]
dnsConfigChange_dnsRecords = Lens.lens (\DnsConfigChange' {dnsRecords} -> dnsRecords) (\s@DnsConfigChange' {} a -> s {dnsRecords = a} :: DnsConfigChange) Prelude.. Lens.coerced

instance Prelude.Hashable DnsConfigChange where
  hashWithSalt _salt DnsConfigChange' {..} =
    _salt `Prelude.hashWithSalt` dnsRecords

instance Prelude.NFData DnsConfigChange where
  rnf DnsConfigChange' {..} = Prelude.rnf dnsRecords

instance Core.ToJSON DnsConfigChange where
  toJSON DnsConfigChange' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DnsRecords" Core..= dnsRecords)]
      )

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
-- Module      : Amazonka.EC2.Types.DnsOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DnsOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DnsRecordIpType
import qualified Amazonka.Prelude as Prelude

-- | Describes the DNS options for an endpoint.
--
-- /See:/ 'newDnsOptions' smart constructor.
data DnsOptions = DnsOptions'
  { -- | The DNS records created for the endpoint.
    dnsRecordIpType :: Prelude.Maybe DnsRecordIpType,
    -- | Indicates whether to enable private DNS only for inbound endpoints.
    privateDnsOnlyForInboundResolverEndpoint :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsRecordIpType', 'dnsOptions_dnsRecordIpType' - The DNS records created for the endpoint.
--
-- 'privateDnsOnlyForInboundResolverEndpoint', 'dnsOptions_privateDnsOnlyForInboundResolverEndpoint' - Indicates whether to enable private DNS only for inbound endpoints.
newDnsOptions ::
  DnsOptions
newDnsOptions =
  DnsOptions'
    { dnsRecordIpType = Prelude.Nothing,
      privateDnsOnlyForInboundResolverEndpoint =
        Prelude.Nothing
    }

-- | The DNS records created for the endpoint.
dnsOptions_dnsRecordIpType :: Lens.Lens' DnsOptions (Prelude.Maybe DnsRecordIpType)
dnsOptions_dnsRecordIpType = Lens.lens (\DnsOptions' {dnsRecordIpType} -> dnsRecordIpType) (\s@DnsOptions' {} a -> s {dnsRecordIpType = a} :: DnsOptions)

-- | Indicates whether to enable private DNS only for inbound endpoints.
dnsOptions_privateDnsOnlyForInboundResolverEndpoint :: Lens.Lens' DnsOptions (Prelude.Maybe Prelude.Bool)
dnsOptions_privateDnsOnlyForInboundResolverEndpoint = Lens.lens (\DnsOptions' {privateDnsOnlyForInboundResolverEndpoint} -> privateDnsOnlyForInboundResolverEndpoint) (\s@DnsOptions' {} a -> s {privateDnsOnlyForInboundResolverEndpoint = a} :: DnsOptions)

instance Data.FromXML DnsOptions where
  parseXML x =
    DnsOptions'
      Prelude.<$> (x Data..@? "dnsRecordIpType")
      Prelude.<*> ( x
                      Data..@? "privateDnsOnlyForInboundResolverEndpoint"
                  )

instance Prelude.Hashable DnsOptions where
  hashWithSalt _salt DnsOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dnsRecordIpType
      `Prelude.hashWithSalt` privateDnsOnlyForInboundResolverEndpoint

instance Prelude.NFData DnsOptions where
  rnf DnsOptions' {..} =
    Prelude.rnf dnsRecordIpType
      `Prelude.seq` Prelude.rnf privateDnsOnlyForInboundResolverEndpoint

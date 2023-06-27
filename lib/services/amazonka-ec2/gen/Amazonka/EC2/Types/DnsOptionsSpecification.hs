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
-- Module      : Amazonka.EC2.Types.DnsOptionsSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DnsOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DnsRecordIpType
import qualified Amazonka.Prelude as Prelude

-- | Describes the DNS options for an endpoint.
--
-- /See:/ 'newDnsOptionsSpecification' smart constructor.
data DnsOptionsSpecification = DnsOptionsSpecification'
  { -- | The DNS records created for the endpoint.
    dnsRecordIpType :: Prelude.Maybe DnsRecordIpType,
    -- | Indicates whether to enable private DNS only for inbound endpoints. This
    -- option is available only for services that support both gateway and
    -- interface endpoints. It routes traffic that originates from the VPC to
    -- the gateway endpoint and traffic that originates from on-premises to the
    -- interface endpoint.
    privateDnsOnlyForInboundResolverEndpoint :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsRecordIpType', 'dnsOptionsSpecification_dnsRecordIpType' - The DNS records created for the endpoint.
--
-- 'privateDnsOnlyForInboundResolverEndpoint', 'dnsOptionsSpecification_privateDnsOnlyForInboundResolverEndpoint' - Indicates whether to enable private DNS only for inbound endpoints. This
-- option is available only for services that support both gateway and
-- interface endpoints. It routes traffic that originates from the VPC to
-- the gateway endpoint and traffic that originates from on-premises to the
-- interface endpoint.
newDnsOptionsSpecification ::
  DnsOptionsSpecification
newDnsOptionsSpecification =
  DnsOptionsSpecification'
    { dnsRecordIpType =
        Prelude.Nothing,
      privateDnsOnlyForInboundResolverEndpoint =
        Prelude.Nothing
    }

-- | The DNS records created for the endpoint.
dnsOptionsSpecification_dnsRecordIpType :: Lens.Lens' DnsOptionsSpecification (Prelude.Maybe DnsRecordIpType)
dnsOptionsSpecification_dnsRecordIpType = Lens.lens (\DnsOptionsSpecification' {dnsRecordIpType} -> dnsRecordIpType) (\s@DnsOptionsSpecification' {} a -> s {dnsRecordIpType = a} :: DnsOptionsSpecification)

-- | Indicates whether to enable private DNS only for inbound endpoints. This
-- option is available only for services that support both gateway and
-- interface endpoints. It routes traffic that originates from the VPC to
-- the gateway endpoint and traffic that originates from on-premises to the
-- interface endpoint.
dnsOptionsSpecification_privateDnsOnlyForInboundResolverEndpoint :: Lens.Lens' DnsOptionsSpecification (Prelude.Maybe Prelude.Bool)
dnsOptionsSpecification_privateDnsOnlyForInboundResolverEndpoint = Lens.lens (\DnsOptionsSpecification' {privateDnsOnlyForInboundResolverEndpoint} -> privateDnsOnlyForInboundResolverEndpoint) (\s@DnsOptionsSpecification' {} a -> s {privateDnsOnlyForInboundResolverEndpoint = a} :: DnsOptionsSpecification)

instance Prelude.Hashable DnsOptionsSpecification where
  hashWithSalt _salt DnsOptionsSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` dnsRecordIpType
      `Prelude.hashWithSalt` privateDnsOnlyForInboundResolverEndpoint

instance Prelude.NFData DnsOptionsSpecification where
  rnf DnsOptionsSpecification' {..} =
    Prelude.rnf dnsRecordIpType
      `Prelude.seq` Prelude.rnf privateDnsOnlyForInboundResolverEndpoint

instance Data.ToQuery DnsOptionsSpecification where
  toQuery DnsOptionsSpecification' {..} =
    Prelude.mconcat
      [ "DnsRecordIpType" Data.=: dnsRecordIpType,
        "PrivateDnsOnlyForInboundResolverEndpoint"
          Data.=: privateDnsOnlyForInboundResolverEndpoint
      ]

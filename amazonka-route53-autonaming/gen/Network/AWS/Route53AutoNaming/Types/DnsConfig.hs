{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DnsConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.DnsRecord
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy

-- | A complex type that contains information about the Amazon Route 53 DNS
-- records that you want AWS Cloud Map to create when you register an
-- instance.
--
-- /See:/ 'newDnsConfig' smart constructor.
data DnsConfig = DnsConfig'
  { -- | The ID of the namespace to use for DNS configuration.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | The routing policy that you want to apply to all Route 53 DNS records
    -- that AWS Cloud Map creates when you register an instance and specify
    -- this service.
    --
    -- If you want to use this service to register instances that create alias
    -- records, specify @WEIGHTED@ for the routing policy.
    --
    -- You can specify the following values:
    --
    -- __MULTIVALUE__
    --
    -- If you define a health check for the service and the health check is
    -- healthy, Route 53 returns the applicable value for up to eight
    -- instances.
    --
    -- For example, suppose the service includes configurations for one @A@
    -- record and a health check, and you use the service to register 10
    -- instances. Route 53 responds to DNS queries with IP addresses for up to
    -- eight healthy instances. If fewer than eight instances are healthy,
    -- Route 53 responds to every DNS query with the IP addresses for all of
    -- the healthy instances.
    --
    -- If you don\'t define a health check for the service, Route 53 assumes
    -- that all instances are healthy and returns the values for up to eight
    -- instances.
    --
    -- For more information about the multivalue routing policy, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing>
    -- in the /Route 53 Developer Guide/.
    --
    -- __WEIGHTED__
    --
    -- Route 53 returns the applicable value from one randomly selected
    -- instance from among the instances that you registered using the same
    -- service. Currently, all records have the same weight, so you can\'t
    -- route more or less traffic to any instances.
    --
    -- For example, suppose the service includes configurations for one @A@
    -- record and a health check, and you use the service to register 10
    -- instances. Route 53 responds to DNS queries with the IP address for one
    -- randomly selected instance from among the healthy instances. If no
    -- instances are healthy, Route 53 responds to DNS queries as if all of the
    -- instances were healthy.
    --
    -- If you don\'t define a health check for the service, Route 53 assumes
    -- that all instances are healthy and returns the applicable value for one
    -- randomly selected instance.
    --
    -- For more information about the weighted routing policy, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing>
    -- in the /Route 53 Developer Guide/.
    routingPolicy :: Prelude.Maybe RoutingPolicy,
    -- | An array that contains one @DnsRecord@ object for each Route 53 DNS
    -- record that you want AWS Cloud Map to create when you register an
    -- instance.
    dnsRecords :: [DnsRecord]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DnsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceId', 'dnsConfig_namespaceId' - The ID of the namespace to use for DNS configuration.
--
-- 'routingPolicy', 'dnsConfig_routingPolicy' - The routing policy that you want to apply to all Route 53 DNS records
-- that AWS Cloud Map creates when you register an instance and specify
-- this service.
--
-- If you want to use this service to register instances that create alias
-- records, specify @WEIGHTED@ for the routing policy.
--
-- You can specify the following values:
--
-- __MULTIVALUE__
--
-- If you define a health check for the service and the health check is
-- healthy, Route 53 returns the applicable value for up to eight
-- instances.
--
-- For example, suppose the service includes configurations for one @A@
-- record and a health check, and you use the service to register 10
-- instances. Route 53 responds to DNS queries with IP addresses for up to
-- eight healthy instances. If fewer than eight instances are healthy,
-- Route 53 responds to every DNS query with the IP addresses for all of
-- the healthy instances.
--
-- If you don\'t define a health check for the service, Route 53 assumes
-- that all instances are healthy and returns the values for up to eight
-- instances.
--
-- For more information about the multivalue routing policy, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing>
-- in the /Route 53 Developer Guide/.
--
-- __WEIGHTED__
--
-- Route 53 returns the applicable value from one randomly selected
-- instance from among the instances that you registered using the same
-- service. Currently, all records have the same weight, so you can\'t
-- route more or less traffic to any instances.
--
-- For example, suppose the service includes configurations for one @A@
-- record and a health check, and you use the service to register 10
-- instances. Route 53 responds to DNS queries with the IP address for one
-- randomly selected instance from among the healthy instances. If no
-- instances are healthy, Route 53 responds to DNS queries as if all of the
-- instances were healthy.
--
-- If you don\'t define a health check for the service, Route 53 assumes
-- that all instances are healthy and returns the applicable value for one
-- randomly selected instance.
--
-- For more information about the weighted routing policy, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing>
-- in the /Route 53 Developer Guide/.
--
-- 'dnsRecords', 'dnsConfig_dnsRecords' - An array that contains one @DnsRecord@ object for each Route 53 DNS
-- record that you want AWS Cloud Map to create when you register an
-- instance.
newDnsConfig ::
  DnsConfig
newDnsConfig =
  DnsConfig'
    { namespaceId = Prelude.Nothing,
      routingPolicy = Prelude.Nothing,
      dnsRecords = Prelude.mempty
    }

-- | The ID of the namespace to use for DNS configuration.
dnsConfig_namespaceId :: Lens.Lens' DnsConfig (Prelude.Maybe Prelude.Text)
dnsConfig_namespaceId = Lens.lens (\DnsConfig' {namespaceId} -> namespaceId) (\s@DnsConfig' {} a -> s {namespaceId = a} :: DnsConfig)

-- | The routing policy that you want to apply to all Route 53 DNS records
-- that AWS Cloud Map creates when you register an instance and specify
-- this service.
--
-- If you want to use this service to register instances that create alias
-- records, specify @WEIGHTED@ for the routing policy.
--
-- You can specify the following values:
--
-- __MULTIVALUE__
--
-- If you define a health check for the service and the health check is
-- healthy, Route 53 returns the applicable value for up to eight
-- instances.
--
-- For example, suppose the service includes configurations for one @A@
-- record and a health check, and you use the service to register 10
-- instances. Route 53 responds to DNS queries with IP addresses for up to
-- eight healthy instances. If fewer than eight instances are healthy,
-- Route 53 responds to every DNS query with the IP addresses for all of
-- the healthy instances.
--
-- If you don\'t define a health check for the service, Route 53 assumes
-- that all instances are healthy and returns the values for up to eight
-- instances.
--
-- For more information about the multivalue routing policy, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing>
-- in the /Route 53 Developer Guide/.
--
-- __WEIGHTED__
--
-- Route 53 returns the applicable value from one randomly selected
-- instance from among the instances that you registered using the same
-- service. Currently, all records have the same weight, so you can\'t
-- route more or less traffic to any instances.
--
-- For example, suppose the service includes configurations for one @A@
-- record and a health check, and you use the service to register 10
-- instances. Route 53 responds to DNS queries with the IP address for one
-- randomly selected instance from among the healthy instances. If no
-- instances are healthy, Route 53 responds to DNS queries as if all of the
-- instances were healthy.
--
-- If you don\'t define a health check for the service, Route 53 assumes
-- that all instances are healthy and returns the applicable value for one
-- randomly selected instance.
--
-- For more information about the weighted routing policy, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing>
-- in the /Route 53 Developer Guide/.
dnsConfig_routingPolicy :: Lens.Lens' DnsConfig (Prelude.Maybe RoutingPolicy)
dnsConfig_routingPolicy = Lens.lens (\DnsConfig' {routingPolicy} -> routingPolicy) (\s@DnsConfig' {} a -> s {routingPolicy = a} :: DnsConfig)

-- | An array that contains one @DnsRecord@ object for each Route 53 DNS
-- record that you want AWS Cloud Map to create when you register an
-- instance.
dnsConfig_dnsRecords :: Lens.Lens' DnsConfig [DnsRecord]
dnsConfig_dnsRecords = Lens.lens (\DnsConfig' {dnsRecords} -> dnsRecords) (\s@DnsConfig' {} a -> s {dnsRecords = a} :: DnsConfig) Prelude.. Prelude._Coerce

instance Prelude.FromJSON DnsConfig where
  parseJSON =
    Prelude.withObject
      "DnsConfig"
      ( \x ->
          DnsConfig'
            Prelude.<$> (x Prelude..:? "NamespaceId")
            Prelude.<*> (x Prelude..:? "RoutingPolicy")
            Prelude.<*> ( x Prelude..:? "DnsRecords"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DnsConfig

instance Prelude.NFData DnsConfig

instance Prelude.ToJSON DnsConfig where
  toJSON DnsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NamespaceId" Prelude..=) Prelude.<$> namespaceId,
            ("RoutingPolicy" Prelude..=)
              Prelude.<$> routingPolicy,
            Prelude.Just ("DnsRecords" Prelude..= dnsRecords)
          ]
      )

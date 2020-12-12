{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSConfig
  ( DNSConfig (..),

    -- * Smart constructor
    mkDNSConfig,

    -- * Lenses
    dcRoutingPolicy,
    dcNamespaceId,
    dcDNSRecords,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSRecord
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy

-- | A complex type that contains information about the Amazon Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /See:/ 'mkDNSConfig' smart constructor.
data DNSConfig = DNSConfig'
  { routingPolicy ::
      Lude.Maybe RoutingPolicy,
    namespaceId :: Lude.Maybe Lude.Text,
    dnsRecords :: [DNSRecord]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSConfig' with the minimum fields required to make a request.
--
-- * 'dnsRecords' - An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
-- * 'namespaceId' - The ID of the namespace to use for DNS configuration.
-- * 'routingPolicy' - The routing policy that you want to apply to all Route 53 DNS records that AWS Cloud Map creates when you register an instance and specify this service.
--
-- You can specify the following values:
-- __MULTIVALUE__
-- If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances.
-- For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances.
-- If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances.
-- For more information about the multivalue routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ .
-- __WEIGHTED__
-- Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances.
-- For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy.
-- If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance.
-- For more information about the weighted routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
mkDNSConfig ::
  DNSConfig
mkDNSConfig =
  DNSConfig'
    { routingPolicy = Lude.Nothing,
      namespaceId = Lude.Nothing,
      dnsRecords = Lude.mempty
    }

-- | The routing policy that you want to apply to all Route 53 DNS records that AWS Cloud Map creates when you register an instance and specify this service.
--
-- You can specify the following values:
-- __MULTIVALUE__
-- If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances.
-- For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances.
-- If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances.
-- For more information about the multivalue routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ .
-- __WEIGHTED__
-- Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances.
-- For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy.
-- If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance.
-- For more information about the weighted routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'routingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcRoutingPolicy :: Lens.Lens' DNSConfig (Lude.Maybe RoutingPolicy)
dcRoutingPolicy = Lens.lens (routingPolicy :: DNSConfig -> Lude.Maybe RoutingPolicy) (\s a -> s {routingPolicy = a} :: DNSConfig)
{-# DEPRECATED dcRoutingPolicy "Use generic-lens or generic-optics with 'routingPolicy' instead." #-}

-- | The ID of the namespace to use for DNS configuration.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNamespaceId :: Lens.Lens' DNSConfig (Lude.Maybe Lude.Text)
dcNamespaceId = Lens.lens (namespaceId :: DNSConfig -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: DNSConfig)
{-# DEPRECATED dcNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDNSRecords :: Lens.Lens' DNSConfig [DNSRecord]
dcDNSRecords = Lens.lens (dnsRecords :: DNSConfig -> [DNSRecord]) (\s a -> s {dnsRecords = a} :: DNSConfig)
{-# DEPRECATED dcDNSRecords "Use generic-lens or generic-optics with 'dnsRecords' instead." #-}

instance Lude.FromJSON DNSConfig where
  parseJSON =
    Lude.withObject
      "DNSConfig"
      ( \x ->
          DNSConfig'
            Lude.<$> (x Lude..:? "RoutingPolicy")
            Lude.<*> (x Lude..:? "NamespaceId")
            Lude.<*> (x Lude..:? "DnsRecords" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DNSConfig where
  toJSON DNSConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoutingPolicy" Lude..=) Lude.<$> routingPolicy,
            ("NamespaceId" Lude..=) Lude.<$> namespaceId,
            Lude.Just ("DnsRecords" Lude..= dnsRecords)
          ]
      )

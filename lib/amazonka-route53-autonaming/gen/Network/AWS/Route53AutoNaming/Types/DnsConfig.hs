{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.DnsConfig
  ( DnsConfig (..)
  -- * Smart constructor
  , mkDnsConfig
  -- * Lenses
  , dcDnsRecords
  , dcNamespaceId
  , dcRoutingPolicy
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.DnsRecord as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types
import qualified Network.AWS.Route53AutoNaming.Types.RoutingPolicy as Types

-- | A complex type that contains information about the Amazon Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /See:/ 'mkDnsConfig' smart constructor.
data DnsConfig = DnsConfig'
  { dnsRecords :: [Types.DnsRecord]
    -- ^ An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
  , namespaceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the namespace to use for DNS configuration.
  , routingPolicy :: Core.Maybe Types.RoutingPolicy
    -- ^ The routing policy that you want to apply to all Route 53 DNS records that AWS Cloud Map creates when you register an instance and specify this service.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DnsConfig' value with any optional fields omitted.
mkDnsConfig
    :: DnsConfig
mkDnsConfig
  = DnsConfig'{dnsRecords = Core.mempty, namespaceId = Core.Nothing,
               routingPolicy = Core.Nothing}

-- | An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDnsRecords :: Lens.Lens' DnsConfig [Types.DnsRecord]
dcDnsRecords = Lens.field @"dnsRecords"
{-# INLINEABLE dcDnsRecords #-}
{-# DEPRECATED dnsRecords "Use generic-lens or generic-optics with 'dnsRecords' instead"  #-}

-- | The ID of the namespace to use for DNS configuration.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNamespaceId :: Lens.Lens' DnsConfig (Core.Maybe Types.ResourceId)
dcNamespaceId = Lens.field @"namespaceId"
{-# INLINEABLE dcNamespaceId #-}
{-# DEPRECATED namespaceId "Use generic-lens or generic-optics with 'namespaceId' instead"  #-}

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
dcRoutingPolicy :: Lens.Lens' DnsConfig (Core.Maybe Types.RoutingPolicy)
dcRoutingPolicy = Lens.field @"routingPolicy"
{-# INLINEABLE dcRoutingPolicy #-}
{-# DEPRECATED routingPolicy "Use generic-lens or generic-optics with 'routingPolicy' instead"  #-}

instance Core.FromJSON DnsConfig where
        toJSON DnsConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DnsRecords" Core..= dnsRecords),
                  ("NamespaceId" Core..=) Core.<$> namespaceId,
                  ("RoutingPolicy" Core..=) Core.<$> routingPolicy])

instance Core.FromJSON DnsConfig where
        parseJSON
          = Core.withObject "DnsConfig" Core.$
              \ x ->
                DnsConfig' Core.<$>
                  (x Core..:? "DnsRecords" Core..!= Core.mempty) Core.<*>
                    x Core..:? "NamespaceId"
                    Core.<*> x Core..:? "RoutingPolicy"

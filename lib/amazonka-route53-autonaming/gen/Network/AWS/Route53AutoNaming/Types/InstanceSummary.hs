{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.InstanceSummary
  ( InstanceSummary (..)
  -- * Smart constructor
  , mkInstanceSummary
  -- * Lenses
  , isAttributes
  , isId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.AttrKey as Types
import qualified Network.AWS.Route53AutoNaming.Types.AttrValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about the instances that you registered by using a specified service.
--
-- /See:/ 'mkInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { attributes :: Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue)
    -- ^ A string map that contains the following information:
--
--
--     * The attributes that are associate with the instance. 
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
--
--     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer. 
--
--
--     * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
--
--
--     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .
--
--
--     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .
--
--
--     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
--
--
--     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
--
  , id :: Core.Maybe Types.ResourceId
    -- ^ The ID for an instance that you created by using a specified service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceSummary' value with any optional fields omitted.
mkInstanceSummary
    :: InstanceSummary
mkInstanceSummary
  = InstanceSummary'{attributes = Core.Nothing, id = Core.Nothing}

-- | A string map that contains the following information:
--
--
--     * The attributes that are associate with the instance. 
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
--
--     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer. 
--
--
--     * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
--
--
--     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .
--
--
--     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .
--
--
--     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
--
--
--     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isAttributes :: Lens.Lens' InstanceSummary (Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue))
isAttributes = Lens.field @"attributes"
{-# INLINEABLE isAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The ID for an instance that you created by using a specified service.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InstanceSummary (Core.Maybe Types.ResourceId)
isId = Lens.field @"id"
{-# INLINEABLE isId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON InstanceSummary where
        parseJSON
          = Core.withObject "InstanceSummary" Core.$
              \ x ->
                InstanceSummary' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Id"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LoadBalancerAttributes
  ( LoadBalancerAttributes (..),

    -- * Smart constructor
    mkLoadBalancerAttributes,

    -- * Lenses
    lbaAccessLog,
    lbaAdditionalAttributes,
    lbaConnectionDraining,
    lbaConnectionSettings,
    lbaCrossZoneLoadBalancing,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AccessLog as Types
import qualified Network.AWS.ELB.Types.AdditionalAttribute as Types
import qualified Network.AWS.ELB.Types.ConnectionDraining as Types
import qualified Network.AWS.ELB.Types.ConnectionSettings as Types
import qualified Network.AWS.ELB.Types.CrossZoneLoadBalancing as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The attributes for a load balancer.
--
-- /See:/ 'mkLoadBalancerAttributes' smart constructor.
data LoadBalancerAttributes = LoadBalancerAttributes'
  { -- | If enabled, the load balancer captures detailed information of all requests and delivers the information to the Amazon S3 bucket that you specify.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs> in the /Classic Load Balancers Guide/ .
    accessLog :: Core.Maybe Types.AccessLog,
    -- | Any additional attributes.
    additionalAttributes :: Core.Maybe [Types.AdditionalAttribute],
    -- | If enabled, the load balancer allows existing requests to complete before the load balancer shifts traffic away from a deregistered or unhealthy instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining> in the /Classic Load Balancers Guide/ .
    connectionDraining :: Core.Maybe Types.ConnectionDraining,
    -- | If enabled, the load balancer allows the connections to remain idle (no data is sent over the connection) for the specified duration.
    --
    -- By default, Elastic Load Balancing maintains a 60-second idle connection timeout for both front-end and back-end connections of your load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout> in the /Classic Load Balancers Guide/ .
    connectionSettings :: Core.Maybe Types.ConnectionSettings,
    -- | If enabled, the load balancer routes the request traffic evenly across all instances regardless of the Availability Zones.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing> in the /Classic Load Balancers Guide/ .
    crossZoneLoadBalancing :: Core.Maybe Types.CrossZoneLoadBalancing
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerAttributes' value with any optional fields omitted.
mkLoadBalancerAttributes ::
  LoadBalancerAttributes
mkLoadBalancerAttributes =
  LoadBalancerAttributes'
    { accessLog = Core.Nothing,
      additionalAttributes = Core.Nothing,
      connectionDraining = Core.Nothing,
      connectionSettings = Core.Nothing,
      crossZoneLoadBalancing = Core.Nothing
    }

-- | If enabled, the load balancer captures detailed information of all requests and delivers the information to the Amazon S3 bucket that you specify.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'accessLog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAccessLog :: Lens.Lens' LoadBalancerAttributes (Core.Maybe Types.AccessLog)
lbaAccessLog = Lens.field @"accessLog"
{-# DEPRECATED lbaAccessLog "Use generic-lens or generic-optics with 'accessLog' instead." #-}

-- | Any additional attributes.
--
-- /Note:/ Consider using 'additionalAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAdditionalAttributes :: Lens.Lens' LoadBalancerAttributes (Core.Maybe [Types.AdditionalAttribute])
lbaAdditionalAttributes = Lens.field @"additionalAttributes"
{-# DEPRECATED lbaAdditionalAttributes "Use generic-lens or generic-optics with 'additionalAttributes' instead." #-}

-- | If enabled, the load balancer allows existing requests to complete before the load balancer shifts traffic away from a deregistered or unhealthy instance.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'connectionDraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaConnectionDraining :: Lens.Lens' LoadBalancerAttributes (Core.Maybe Types.ConnectionDraining)
lbaConnectionDraining = Lens.field @"connectionDraining"
{-# DEPRECATED lbaConnectionDraining "Use generic-lens or generic-optics with 'connectionDraining' instead." #-}

-- | If enabled, the load balancer allows the connections to remain idle (no data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection timeout for both front-end and back-end connections of your load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'connectionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaConnectionSettings :: Lens.Lens' LoadBalancerAttributes (Core.Maybe Types.ConnectionSettings)
lbaConnectionSettings = Lens.field @"connectionSettings"
{-# DEPRECATED lbaConnectionSettings "Use generic-lens or generic-optics with 'connectionSettings' instead." #-}

-- | If enabled, the load balancer routes the request traffic evenly across all instances regardless of the Availability Zones.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'crossZoneLoadBalancing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaCrossZoneLoadBalancing :: Lens.Lens' LoadBalancerAttributes (Core.Maybe Types.CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing = Lens.field @"crossZoneLoadBalancing"
{-# DEPRECATED lbaCrossZoneLoadBalancing "Use generic-lens or generic-optics with 'crossZoneLoadBalancing' instead." #-}

instance Core.FromXML LoadBalancerAttributes where
  parseXML x =
    LoadBalancerAttributes'
      Core.<$> (x Core..@? "AccessLog")
      Core.<*> ( x Core..@? "AdditionalAttributes"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "ConnectionDraining")
      Core.<*> (x Core..@? "ConnectionSettings")
      Core.<*> (x Core..@? "CrossZoneLoadBalancing")

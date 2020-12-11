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
    lbaCrossZoneLoadBalancing,
    lbaAccessLog,
    lbaAdditionalAttributes,
    lbaConnectionSettings,
    lbaConnectionDraining,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.AccessLog
import Network.AWS.ELB.Types.AdditionalAttribute
import Network.AWS.ELB.Types.ConnectionDraining
import Network.AWS.ELB.Types.ConnectionSettings
import Network.AWS.ELB.Types.CrossZoneLoadBalancing
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The attributes for a load balancer.
--
-- /See:/ 'mkLoadBalancerAttributes' smart constructor.
data LoadBalancerAttributes = LoadBalancerAttributes'
  { crossZoneLoadBalancing ::
      Lude.Maybe CrossZoneLoadBalancing,
    accessLog :: Lude.Maybe AccessLog,
    additionalAttributes ::
      Lude.Maybe [AdditionalAttribute],
    connectionSettings ::
      Lude.Maybe ConnectionSettings,
    connectionDraining ::
      Lude.Maybe ConnectionDraining
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerAttributes' with the minimum fields required to make a request.
--
-- * 'accessLog' - If enabled, the load balancer captures detailed information of all requests and delivers the information to the Amazon S3 bucket that you specify.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs> in the /Classic Load Balancers Guide/ .
-- * 'additionalAttributes' - Any additional attributes.
-- * 'connectionDraining' - If enabled, the load balancer allows existing requests to complete before the load balancer shifts traffic away from a deregistered or unhealthy instance.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining> in the /Classic Load Balancers Guide/ .
-- * 'connectionSettings' - If enabled, the load balancer allows the connections to remain idle (no data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection timeout for both front-end and back-end connections of your load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout> in the /Classic Load Balancers Guide/ .
-- * 'crossZoneLoadBalancing' - If enabled, the load balancer routes the request traffic evenly across all instances regardless of the Availability Zones.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing> in the /Classic Load Balancers Guide/ .
mkLoadBalancerAttributes ::
  LoadBalancerAttributes
mkLoadBalancerAttributes =
  LoadBalancerAttributes'
    { crossZoneLoadBalancing = Lude.Nothing,
      accessLog = Lude.Nothing,
      additionalAttributes = Lude.Nothing,
      connectionSettings = Lude.Nothing,
      connectionDraining = Lude.Nothing
    }

-- | If enabled, the load balancer routes the request traffic evenly across all instances regardless of the Availability Zones.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'crossZoneLoadBalancing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaCrossZoneLoadBalancing :: Lens.Lens' LoadBalancerAttributes (Lude.Maybe CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing = Lens.lens (crossZoneLoadBalancing :: LoadBalancerAttributes -> Lude.Maybe CrossZoneLoadBalancing) (\s a -> s {crossZoneLoadBalancing = a} :: LoadBalancerAttributes)
{-# DEPRECATED lbaCrossZoneLoadBalancing "Use generic-lens or generic-optics with 'crossZoneLoadBalancing' instead." #-}

-- | If enabled, the load balancer captures detailed information of all requests and delivers the information to the Amazon S3 bucket that you specify.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'accessLog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAccessLog :: Lens.Lens' LoadBalancerAttributes (Lude.Maybe AccessLog)
lbaAccessLog = Lens.lens (accessLog :: LoadBalancerAttributes -> Lude.Maybe AccessLog) (\s a -> s {accessLog = a} :: LoadBalancerAttributes)
{-# DEPRECATED lbaAccessLog "Use generic-lens or generic-optics with 'accessLog' instead." #-}

-- | Any additional attributes.
--
-- /Note:/ Consider using 'additionalAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAdditionalAttributes :: Lens.Lens' LoadBalancerAttributes (Lude.Maybe [AdditionalAttribute])
lbaAdditionalAttributes = Lens.lens (additionalAttributes :: LoadBalancerAttributes -> Lude.Maybe [AdditionalAttribute]) (\s a -> s {additionalAttributes = a} :: LoadBalancerAttributes)
{-# DEPRECATED lbaAdditionalAttributes "Use generic-lens or generic-optics with 'additionalAttributes' instead." #-}

-- | If enabled, the load balancer allows the connections to remain idle (no data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection timeout for both front-end and back-end connections of your load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'connectionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaConnectionSettings :: Lens.Lens' LoadBalancerAttributes (Lude.Maybe ConnectionSettings)
lbaConnectionSettings = Lens.lens (connectionSettings :: LoadBalancerAttributes -> Lude.Maybe ConnectionSettings) (\s a -> s {connectionSettings = a} :: LoadBalancerAttributes)
{-# DEPRECATED lbaConnectionSettings "Use generic-lens or generic-optics with 'connectionSettings' instead." #-}

-- | If enabled, the load balancer allows existing requests to complete before the load balancer shifts traffic away from a deregistered or unhealthy instance.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'connectionDraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaConnectionDraining :: Lens.Lens' LoadBalancerAttributes (Lude.Maybe ConnectionDraining)
lbaConnectionDraining = Lens.lens (connectionDraining :: LoadBalancerAttributes -> Lude.Maybe ConnectionDraining) (\s a -> s {connectionDraining = a} :: LoadBalancerAttributes)
{-# DEPRECATED lbaConnectionDraining "Use generic-lens or generic-optics with 'connectionDraining' instead." #-}

instance Lude.FromXML LoadBalancerAttributes where
  parseXML x =
    LoadBalancerAttributes'
      Lude.<$> (x Lude..@? "CrossZoneLoadBalancing")
      Lude.<*> (x Lude..@? "AccessLog")
      Lude.<*> ( x Lude..@? "AdditionalAttributes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ConnectionSettings")
      Lude.<*> (x Lude..@? "ConnectionDraining")

instance Lude.ToQuery LoadBalancerAttributes where
  toQuery LoadBalancerAttributes' {..} =
    Lude.mconcat
      [ "CrossZoneLoadBalancing" Lude.=: crossZoneLoadBalancing,
        "AccessLog" Lude.=: accessLog,
        "AdditionalAttributes"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> additionalAttributes),
        "ConnectionSettings" Lude.=: connectionSettings,
        "ConnectionDraining" Lude.=: connectionDraining
      ]

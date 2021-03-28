{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Listener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Listener
  ( Listener (..)
  -- * Smart constructor
  , mkListener
  -- * Lenses
  , lAlpnPolicy
  , lCertificates
  , lDefaultActions
  , lListenerArn
  , lLoadBalancerArn
  , lPort
  , lProtocol
  , lSslPolicy
  ) where

import qualified Network.AWS.ELBv2.Types.Action as Types
import qualified Network.AWS.ELBv2.Types.AlpnPolicyValue as Types
import qualified Network.AWS.ELBv2.Types.Certificate as Types
import qualified Network.AWS.ELBv2.Types.ListenerArn as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerArn as Types
import qualified Network.AWS.ELBv2.Types.ProtocolEnum as Types
import qualified Network.AWS.ELBv2.Types.SslPolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a listener.
--
-- /See:/ 'mkListener' smart constructor.
data Listener = Listener'
  { alpnPolicy :: Core.Maybe [Types.AlpnPolicyValue]
    -- ^ [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
  , certificates :: Core.Maybe [Types.Certificate]
    -- ^ [HTTPS or TLS listener] The default certificate for the listener.
  , defaultActions :: Core.Maybe [Types.Action]
    -- ^ The default actions for the listener.
  , listenerArn :: Core.Maybe Types.ListenerArn
    -- ^ The Amazon Resource Name (ARN) of the listener.
  , loadBalancerArn :: Core.Maybe Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , port :: Core.Maybe Core.Natural
    -- ^ The port on which the load balancer is listening.
  , protocol :: Core.Maybe Types.ProtocolEnum
    -- ^ The protocol for connections from clients to the load balancer.
  , sslPolicy :: Core.Maybe Types.SslPolicyName
    -- ^ [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Listener' value with any optional fields omitted.
mkListener
    :: Listener
mkListener
  = Listener'{alpnPolicy = Core.Nothing, certificates = Core.Nothing,
              defaultActions = Core.Nothing, listenerArn = Core.Nothing,
              loadBalancerArn = Core.Nothing, port = Core.Nothing,
              protocol = Core.Nothing, sslPolicy = Core.Nothing}

-- | [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
--
-- /Note:/ Consider using 'alpnPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAlpnPolicy :: Lens.Lens' Listener (Core.Maybe [Types.AlpnPolicyValue])
lAlpnPolicy = Lens.field @"alpnPolicy"
{-# INLINEABLE lAlpnPolicy #-}
{-# DEPRECATED alpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead"  #-}

-- | [HTTPS or TLS listener] The default certificate for the listener.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCertificates :: Lens.Lens' Listener (Core.Maybe [Types.Certificate])
lCertificates = Lens.field @"certificates"
{-# INLINEABLE lCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | The default actions for the listener.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultActions :: Lens.Lens' Listener (Core.Maybe [Types.Action])
lDefaultActions = Lens.field @"defaultActions"
{-# INLINEABLE lDefaultActions #-}
{-# DEPRECATED defaultActions "Use generic-lens or generic-optics with 'defaultActions' instead"  #-}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lListenerArn :: Lens.Lens' Listener (Core.Maybe Types.ListenerArn)
lListenerArn = Lens.field @"listenerArn"
{-# INLINEABLE lListenerArn #-}
{-# DEPRECATED listenerArn "Use generic-lens or generic-optics with 'listenerArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLoadBalancerArn :: Lens.Lens' Listener (Core.Maybe Types.LoadBalancerArn)
lLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE lLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The port on which the load balancer is listening.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPort :: Lens.Lens' Listener (Core.Maybe Core.Natural)
lPort = Lens.field @"port"
{-# INLINEABLE lPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The protocol for connections from clients to the load balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lProtocol :: Lens.Lens' Listener (Core.Maybe Types.ProtocolEnum)
lProtocol = Lens.field @"protocol"
{-# INLINEABLE lProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSslPolicy :: Lens.Lens' Listener (Core.Maybe Types.SslPolicyName)
lSslPolicy = Lens.field @"sslPolicy"
{-# INLINEABLE lSslPolicy #-}
{-# DEPRECATED sslPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead"  #-}

instance Core.FromXML Listener where
        parseXML x
          = Listener' Core.<$>
              (x Core..@? "AlpnPolicy" Core..<@> Core.parseXMLList "member")
                Core.<*>
                x Core..@? "Certificates" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "DefaultActions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "ListenerArn"
                Core.<*> x Core..@? "LoadBalancerArn"
                Core.<*> x Core..@? "Port"
                Core.<*> x Core..@? "Protocol"
                Core.<*> x Core..@? "SslPolicy"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Listener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Listener
  ( Listener (..),

    -- * Smart constructor
    mkListener,

    -- * Lenses
    lSSLPolicy,
    lListenerARN,
    lProtocol,
    lDefaultActions,
    lCertificates,
    lLoadBalancerARN,
    lAlpnPolicy,
    lPort,
  )
where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.Certificate
import Network.AWS.ELBv2.Types.ProtocolEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a listener.
--
-- /See:/ 'mkListener' smart constructor.
data Listener = Listener'
  { -- | [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
    sslPolicy :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerARN :: Lude.Maybe Lude.Text,
    -- | The protocol for connections from clients to the load balancer.
    protocol :: Lude.Maybe ProtocolEnum,
    -- | The default actions for the listener.
    defaultActions :: Lude.Maybe [Action],
    -- | [HTTPS or TLS listener] The default certificate for the listener.
    certificates :: Lude.Maybe [Certificate],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Maybe Lude.Text,
    -- | [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
    alpnPolicy :: Lude.Maybe [Lude.Text],
    -- | The port on which the load balancer is listening.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- * 'sslPolicy' - [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
-- * 'protocol' - The protocol for connections from clients to the load balancer.
-- * 'defaultActions' - The default actions for the listener.
-- * 'certificates' - [HTTPS or TLS listener] The default certificate for the listener.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'alpnPolicy' - [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
-- * 'port' - The port on which the load balancer is listening.
mkListener ::
  Listener
mkListener =
  Listener'
    { sslPolicy = Lude.Nothing,
      listenerARN = Lude.Nothing,
      protocol = Lude.Nothing,
      defaultActions = Lude.Nothing,
      certificates = Lude.Nothing,
      loadBalancerARN = Lude.Nothing,
      alpnPolicy = Lude.Nothing,
      port = Lude.Nothing
    }

-- | [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSSLPolicy :: Lens.Lens' Listener (Lude.Maybe Lude.Text)
lSSLPolicy = Lens.lens (sslPolicy :: Listener -> Lude.Maybe Lude.Text) (\s a -> s {sslPolicy = a} :: Listener)
{-# DEPRECATED lSSLPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead." #-}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lListenerARN :: Lens.Lens' Listener (Lude.Maybe Lude.Text)
lListenerARN = Lens.lens (listenerARN :: Listener -> Lude.Maybe Lude.Text) (\s a -> s {listenerARN = a} :: Listener)
{-# DEPRECATED lListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The protocol for connections from clients to the load balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lProtocol :: Lens.Lens' Listener (Lude.Maybe ProtocolEnum)
lProtocol = Lens.lens (protocol :: Listener -> Lude.Maybe ProtocolEnum) (\s a -> s {protocol = a} :: Listener)
{-# DEPRECATED lProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The default actions for the listener.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultActions :: Lens.Lens' Listener (Lude.Maybe [Action])
lDefaultActions = Lens.lens (defaultActions :: Listener -> Lude.Maybe [Action]) (\s a -> s {defaultActions = a} :: Listener)
{-# DEPRECATED lDefaultActions "Use generic-lens or generic-optics with 'defaultActions' instead." #-}

-- | [HTTPS or TLS listener] The default certificate for the listener.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCertificates :: Lens.Lens' Listener (Lude.Maybe [Certificate])
lCertificates = Lens.lens (certificates :: Listener -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: Listener)
{-# DEPRECATED lCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLoadBalancerARN :: Lens.Lens' Listener (Lude.Maybe Lude.Text)
lLoadBalancerARN = Lens.lens (loadBalancerARN :: Listener -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerARN = a} :: Listener)
{-# DEPRECATED lLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
--
-- /Note:/ Consider using 'alpnPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAlpnPolicy :: Lens.Lens' Listener (Lude.Maybe [Lude.Text])
lAlpnPolicy = Lens.lens (alpnPolicy :: Listener -> Lude.Maybe [Lude.Text]) (\s a -> s {alpnPolicy = a} :: Listener)
{-# DEPRECATED lAlpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead." #-}

-- | The port on which the load balancer is listening.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPort :: Lens.Lens' Listener (Lude.Maybe Lude.Natural)
lPort = Lens.lens (port :: Listener -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: Listener)
{-# DEPRECATED lPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML Listener where
  parseXML x =
    Listener'
      Lude.<$> (x Lude..@? "SslPolicy")
      Lude.<*> (x Lude..@? "ListenerArn")
      Lude.<*> (x Lude..@? "Protocol")
      Lude.<*> ( x Lude..@? "DefaultActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Certificates" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "LoadBalancerArn")
      Lude.<*> ( x Lude..@? "AlpnPolicy" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Port")

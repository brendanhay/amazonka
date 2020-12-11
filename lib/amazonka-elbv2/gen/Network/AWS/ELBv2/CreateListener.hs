{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateListener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listener for the specified Application Load Balancer, Network Load Balancer. or Gateway Load Balancer.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html Listeners for your Application Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html Listeners for your Network Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-listeners.html Listeners for your Gateway Load Balancers>
--
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple listeners with the same settings, each call succeeds.
module Network.AWS.ELBv2.CreateListener
  ( -- * Creating a request
    CreateListener (..),
    mkCreateListener,

    -- ** Request lenses
    clSSLPolicy,
    clProtocol,
    clCertificates,
    clAlpnPolicy,
    clTags,
    clPort,
    clLoadBalancerARN,
    clDefaultActions,

    -- * Destructuring the response
    CreateListenerResponse (..),
    mkCreateListenerResponse,

    -- ** Response lenses
    clrsListeners,
    clrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateListener' smart constructor.
data CreateListener = CreateListener'
  { sslPolicy ::
      Lude.Maybe Lude.Text,
    protocol :: Lude.Maybe ProtocolEnum,
    certificates :: Lude.Maybe [Certificate],
    alpnPolicy :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    port :: Lude.Maybe Lude.Natural,
    loadBalancerARN :: Lude.Text,
    defaultActions :: [Action]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateListener' with the minimum fields required to make a request.
--
-- * 'alpnPolicy' - [TLS listeners] The name of the Application-Layer Protocol Negotiation (ALPN) policy. You can specify one policy name. The following are the possible values:
--
--
--     * @HTTP1Only@
--
--
--     * @HTTP2Only@
--
--
--     * @HTTP2Optional@
--
--
--     * @HTTP2Preferred@
--
--
--     * @None@
--
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies> in the /Network Load Balancers Guide/ .
-- * 'certificates' - [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
-- * 'defaultActions' - The actions for the default rule.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'port' - The port on which the load balancer is listening. You cannot specify a port for a Gateway Load Balancer.
-- * 'protocol' - The protocol for connections from clients to the load balancer. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
-- * 'sslPolicy' - [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
-- * 'tags' - The tags to assign to the listener.
mkCreateListener ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  CreateListener
mkCreateListener pLoadBalancerARN_ =
  CreateListener'
    { sslPolicy = Lude.Nothing,
      protocol = Lude.Nothing,
      certificates = Lude.Nothing,
      alpnPolicy = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing,
      loadBalancerARN = pLoadBalancerARN_,
      defaultActions = Lude.mempty
    }

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clSSLPolicy :: Lens.Lens' CreateListener (Lude.Maybe Lude.Text)
clSSLPolicy = Lens.lens (sslPolicy :: CreateListener -> Lude.Maybe Lude.Text) (\s a -> s {sslPolicy = a} :: CreateListener)
{-# DEPRECATED clSSLPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead." #-}

-- | The protocol for connections from clients to the load balancer. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clProtocol :: Lens.Lens' CreateListener (Lude.Maybe ProtocolEnum)
clProtocol = Lens.lens (protocol :: CreateListener -> Lude.Maybe ProtocolEnum) (\s a -> s {protocol = a} :: CreateListener)
{-# DEPRECATED clProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCertificates :: Lens.Lens' CreateListener (Lude.Maybe [Certificate])
clCertificates = Lens.lens (certificates :: CreateListener -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: CreateListener)
{-# DEPRECATED clCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | [TLS listeners] The name of the Application-Layer Protocol Negotiation (ALPN) policy. You can specify one policy name. The following are the possible values:
--
--
--     * @HTTP1Only@
--
--
--     * @HTTP2Only@
--
--
--     * @HTTP2Optional@
--
--
--     * @HTTP2Preferred@
--
--
--     * @None@
--
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies> in the /Network Load Balancers Guide/ .
--
-- /Note:/ Consider using 'alpnPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAlpnPolicy :: Lens.Lens' CreateListener (Lude.Maybe [Lude.Text])
clAlpnPolicy = Lens.lens (alpnPolicy :: CreateListener -> Lude.Maybe [Lude.Text]) (\s a -> s {alpnPolicy = a} :: CreateListener)
{-# DEPRECATED clAlpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead." #-}

-- | The tags to assign to the listener.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clTags :: Lens.Lens' CreateListener (Lude.Maybe (Lude.NonEmpty Tag))
clTags = Lens.lens (tags :: CreateListener -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateListener)
{-# DEPRECATED clTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port on which the load balancer is listening. You cannot specify a port for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clPort :: Lens.Lens' CreateListener (Lude.Maybe Lude.Natural)
clPort = Lens.lens (port :: CreateListener -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: CreateListener)
{-# DEPRECATED clPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLoadBalancerARN :: Lens.Lens' CreateListener Lude.Text
clLoadBalancerARN = Lens.lens (loadBalancerARN :: CreateListener -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: CreateListener)
{-# DEPRECATED clLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The actions for the default rule.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clDefaultActions :: Lens.Lens' CreateListener [Action]
clDefaultActions = Lens.lens (defaultActions :: CreateListener -> [Action]) (\s a -> s {defaultActions = a} :: CreateListener)
{-# DEPRECATED clDefaultActions "Use generic-lens or generic-optics with 'defaultActions' instead." #-}

instance Lude.AWSRequest CreateListener where
  type Rs CreateListener = CreateListenerResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "CreateListenerResult"
      ( \s h x ->
          CreateListenerResponse'
            Lude.<$> ( x Lude..@? "Listeners" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateListener where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateListener where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateListener where
  toQuery CreateListener' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateListener" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "SslPolicy" Lude.=: sslPolicy,
        "Protocol" Lude.=: protocol,
        "Certificates"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> certificates),
        "AlpnPolicy"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alpnPolicy),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "Port" Lude.=: port,
        "LoadBalancerArn" Lude.=: loadBalancerARN,
        "DefaultActions" Lude.=: Lude.toQueryList "member" defaultActions
      ]

-- | /See:/ 'mkCreateListenerResponse' smart constructor.
data CreateListenerResponse = CreateListenerResponse'
  { listeners ::
      Lude.Maybe [Listener],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateListenerResponse' with the minimum fields required to make a request.
--
-- * 'listeners' - Information about the listener.
-- * 'responseStatus' - The response status code.
mkCreateListenerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateListenerResponse
mkCreateListenerResponse pResponseStatus_ =
  CreateListenerResponse'
    { listeners = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the listener.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsListeners :: Lens.Lens' CreateListenerResponse (Lude.Maybe [Listener])
clrsListeners = Lens.lens (listeners :: CreateListenerResponse -> Lude.Maybe [Listener]) (\s a -> s {listeners = a} :: CreateListenerResponse)
{-# DEPRECATED clrsListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsResponseStatus :: Lens.Lens' CreateListenerResponse Lude.Int
clrsResponseStatus = Lens.lens (responseStatus :: CreateListenerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateListenerResponse)
{-# DEPRECATED clrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

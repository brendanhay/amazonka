{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyListener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified properties of the specified listener. Any properties that you do not specify remain unchanged.
--
-- Changing the protocol from HTTPS to HTTP, or from TLS to TCP, removes the security policy and default certificate properties. If you change the protocol from HTTP to HTTPS, or from TCP to TLS, you must add the security policy and default certificate properties.
-- To add an item to a list, remove an item from a list, or update an item in a list, you must provide the entire list. For example, to add an action, specify a list with the current actions plus the new action.
module Network.AWS.ELBv2.ModifyListener
  ( -- * Creating a request
    ModifyListener (..),
    mkModifyListener,

    -- ** Request lenses
    mlSSLPolicy,
    mlListenerARN,
    mlProtocol,
    mlDefaultActions,
    mlCertificates,
    mlAlpnPolicy,
    mlPort,

    -- * Destructuring the response
    ModifyListenerResponse (..),
    mkModifyListenerResponse,

    -- ** Response lenses
    mlrsListeners,
    mlrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyListener' smart constructor.
data ModifyListener = ModifyListener'
  { -- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
    sslPolicy :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerARN :: Lude.Text,
    -- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
    protocol :: Lude.Maybe ProtocolEnum,
    -- | The actions for the default rule.
    defaultActions :: Lude.Maybe [Action],
    -- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
    certificates :: Lude.Maybe [Certificate],
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
    alpnPolicy :: Lude.Maybe [Lude.Text],
    -- | The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyListener' with the minimum fields required to make a request.
--
-- * 'sslPolicy' - [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
-- * 'protocol' - The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
-- * 'defaultActions' - The actions for the default rule.
-- * 'certificates' - [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
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
-- * 'port' - The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
mkModifyListener ::
  -- | 'listenerARN'
  Lude.Text ->
  ModifyListener
mkModifyListener pListenerARN_ =
  ModifyListener'
    { sslPolicy = Lude.Nothing,
      listenerARN = pListenerARN_,
      protocol = Lude.Nothing,
      defaultActions = Lude.Nothing,
      certificates = Lude.Nothing,
      alpnPolicy = Lude.Nothing,
      port = Lude.Nothing
    }

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlSSLPolicy :: Lens.Lens' ModifyListener (Lude.Maybe Lude.Text)
mlSSLPolicy = Lens.lens (sslPolicy :: ModifyListener -> Lude.Maybe Lude.Text) (\s a -> s {sslPolicy = a} :: ModifyListener)
{-# DEPRECATED mlSSLPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead." #-}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlListenerARN :: Lens.Lens' ModifyListener Lude.Text
mlListenerARN = Lens.lens (listenerARN :: ModifyListener -> Lude.Text) (\s a -> s {listenerARN = a} :: ModifyListener)
{-# DEPRECATED mlListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlProtocol :: Lens.Lens' ModifyListener (Lude.Maybe ProtocolEnum)
mlProtocol = Lens.lens (protocol :: ModifyListener -> Lude.Maybe ProtocolEnum) (\s a -> s {protocol = a} :: ModifyListener)
{-# DEPRECATED mlProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The actions for the default rule.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlDefaultActions :: Lens.Lens' ModifyListener (Lude.Maybe [Action])
mlDefaultActions = Lens.lens (defaultActions :: ModifyListener -> Lude.Maybe [Action]) (\s a -> s {defaultActions = a} :: ModifyListener)
{-# DEPRECATED mlDefaultActions "Use generic-lens or generic-optics with 'defaultActions' instead." #-}

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlCertificates :: Lens.Lens' ModifyListener (Lude.Maybe [Certificate])
mlCertificates = Lens.lens (certificates :: ModifyListener -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: ModifyListener)
{-# DEPRECATED mlCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

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
mlAlpnPolicy :: Lens.Lens' ModifyListener (Lude.Maybe [Lude.Text])
mlAlpnPolicy = Lens.lens (alpnPolicy :: ModifyListener -> Lude.Maybe [Lude.Text]) (\s a -> s {alpnPolicy = a} :: ModifyListener)
{-# DEPRECATED mlAlpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead." #-}

-- | The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlPort :: Lens.Lens' ModifyListener (Lude.Maybe Lude.Natural)
mlPort = Lens.lens (port :: ModifyListener -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: ModifyListener)
{-# DEPRECATED mlPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.AWSRequest ModifyListener where
  type Rs ModifyListener = ModifyListenerResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "ModifyListenerResult"
      ( \s h x ->
          ModifyListenerResponse'
            Lude.<$> ( x Lude..@? "Listeners" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyListener where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyListener where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyListener where
  toQuery ModifyListener' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyListener" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "SslPolicy" Lude.=: sslPolicy,
        "ListenerArn" Lude.=: listenerARN,
        "Protocol" Lude.=: protocol,
        "DefaultActions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> defaultActions),
        "Certificates"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> certificates),
        "AlpnPolicy"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alpnPolicy),
        "Port" Lude.=: port
      ]

-- | /See:/ 'mkModifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'
  { -- | Information about the modified listener.
    listeners :: Lude.Maybe [Listener],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyListenerResponse' with the minimum fields required to make a request.
--
-- * 'listeners' - Information about the modified listener.
-- * 'responseStatus' - The response status code.
mkModifyListenerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyListenerResponse
mkModifyListenerResponse pResponseStatus_ =
  ModifyListenerResponse'
    { listeners = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the modified listener.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrsListeners :: Lens.Lens' ModifyListenerResponse (Lude.Maybe [Listener])
mlrsListeners = Lens.lens (listeners :: ModifyListenerResponse -> Lude.Maybe [Listener]) (\s a -> s {listeners = a} :: ModifyListenerResponse)
{-# DEPRECATED mlrsListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrsResponseStatus :: Lens.Lens' ModifyListenerResponse Lude.Int
mlrsResponseStatus = Lens.lens (responseStatus :: ModifyListenerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyListenerResponse)
{-# DEPRECATED mlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    mlListenerArn,
    mlAlpnPolicy,
    mlCertificates,
    mlDefaultActions,
    mlPort,
    mlProtocol,
    mlSslPolicy,

    -- * Destructuring the response
    ModifyListenerResponse (..),
    mkModifyListenerResponse,

    -- ** Response lenses
    mlrrsListeners,
    mlrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyListener' smart constructor.
data ModifyListener = ModifyListener'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Types.ListenerArn,
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
    alpnPolicy :: Core.Maybe [Types.AlpnPolicyValue],
    -- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
    certificates :: Core.Maybe [Types.Certificate],
    -- | The actions for the default rule.
    defaultActions :: Core.Maybe [Types.Action],
    -- | The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
    port :: Core.Maybe Core.Natural,
    -- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
    protocol :: Core.Maybe Types.ProtocolEnum,
    -- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
    sslPolicy :: Core.Maybe Types.SslPolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyListener' value with any optional fields omitted.
mkModifyListener ::
  -- | 'listenerArn'
  Types.ListenerArn ->
  ModifyListener
mkModifyListener listenerArn =
  ModifyListener'
    { listenerArn,
      alpnPolicy = Core.Nothing,
      certificates = Core.Nothing,
      defaultActions = Core.Nothing,
      port = Core.Nothing,
      protocol = Core.Nothing,
      sslPolicy = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlListenerArn :: Lens.Lens' ModifyListener Types.ListenerArn
mlListenerArn = Lens.field @"listenerArn"
{-# DEPRECATED mlListenerArn "Use generic-lens or generic-optics with 'listenerArn' instead." #-}

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
mlAlpnPolicy :: Lens.Lens' ModifyListener (Core.Maybe [Types.AlpnPolicyValue])
mlAlpnPolicy = Lens.field @"alpnPolicy"
{-# DEPRECATED mlAlpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead." #-}

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlCertificates :: Lens.Lens' ModifyListener (Core.Maybe [Types.Certificate])
mlCertificates = Lens.field @"certificates"
{-# DEPRECATED mlCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The actions for the default rule.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlDefaultActions :: Lens.Lens' ModifyListener (Core.Maybe [Types.Action])
mlDefaultActions = Lens.field @"defaultActions"
{-# DEPRECATED mlDefaultActions "Use generic-lens or generic-optics with 'defaultActions' instead." #-}

-- | The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlPort :: Lens.Lens' ModifyListener (Core.Maybe Core.Natural)
mlPort = Lens.field @"port"
{-# DEPRECATED mlPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlProtocol :: Lens.Lens' ModifyListener (Core.Maybe Types.ProtocolEnum)
mlProtocol = Lens.field @"protocol"
{-# DEPRECATED mlProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlSslPolicy :: Lens.Lens' ModifyListener (Core.Maybe Types.SslPolicyName)
mlSslPolicy = Lens.field @"sslPolicy"
{-# DEPRECATED mlSslPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead." #-}

instance Core.AWSRequest ModifyListener where
  type Rs ModifyListener = ModifyListenerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyListener")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "ListenerArn" listenerArn)
                Core.<> ( Core.toQueryValue
                            "AlpnPolicy"
                            (Core.toQueryList "member" Core.<$> alpnPolicy)
                        )
                Core.<> ( Core.toQueryValue
                            "Certificates"
                            (Core.toQueryList "member" Core.<$> certificates)
                        )
                Core.<> ( Core.toQueryValue
                            "DefaultActions"
                            (Core.toQueryList "member" Core.<$> defaultActions)
                        )
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> (Core.toQueryValue "Protocol" Core.<$> protocol)
                Core.<> (Core.toQueryValue "SslPolicy" Core.<$> sslPolicy)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyListenerResult"
      ( \s h x ->
          ModifyListenerResponse'
            Core.<$> (x Core..@? "Listeners" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'
  { -- | Information about the modified listener.
    listeners :: Core.Maybe [Types.Listener],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyListenerResponse' value with any optional fields omitted.
mkModifyListenerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyListenerResponse
mkModifyListenerResponse responseStatus =
  ModifyListenerResponse' {listeners = Core.Nothing, responseStatus}

-- | Information about the modified listener.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrrsListeners :: Lens.Lens' ModifyListenerResponse (Core.Maybe [Types.Listener])
mlrrsListeners = Lens.field @"listeners"
{-# DEPRECATED mlrrsListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrrsResponseStatus :: Lens.Lens' ModifyListenerResponse Core.Int
mlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyListener (..)
    , mkModifyListener
    -- ** Request lenses
    , mlListenerArn
    , mlAlpnPolicy
    , mlCertificates
    , mlDefaultActions
    , mlPort
    , mlProtocol
    , mlSslPolicy

    -- * Destructuring the response
    , ModifyListenerResponse (..)
    , mkModifyListenerResponse
    -- ** Response lenses
    , mlrrsListeners
    , mlrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyListener' smart constructor.
data ModifyListener = ModifyListener'
  { listenerArn :: Types.ListenerArn
    -- ^ The Amazon Resource Name (ARN) of the listener.
  , alpnPolicy :: Core.Maybe [Types.AlpnPolicyValue]
    -- ^ [TLS listeners] The name of the Application-Layer Protocol Negotiation (ALPN) policy. You can specify one policy name. The following are the possible values:
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
  , certificates :: Core.Maybe [Types.Certificate]
    -- ^ [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
  , defaultActions :: Core.Maybe [Types.Action]
    -- ^ The actions for the default rule.
  , port :: Core.Maybe Core.Natural
    -- ^ The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
  , protocol :: Core.Maybe Types.ProtocolEnum
    -- ^ The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
  , sslPolicy :: Core.Maybe Types.SslPolicyName
    -- ^ [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyListener' value with any optional fields omitted.
mkModifyListener
    :: Types.ListenerArn -- ^ 'listenerArn'
    -> ModifyListener
mkModifyListener listenerArn
  = ModifyListener'{listenerArn, alpnPolicy = Core.Nothing,
                    certificates = Core.Nothing, defaultActions = Core.Nothing,
                    port = Core.Nothing, protocol = Core.Nothing,
                    sslPolicy = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlListenerArn :: Lens.Lens' ModifyListener Types.ListenerArn
mlListenerArn = Lens.field @"listenerArn"
{-# INLINEABLE mlListenerArn #-}
{-# DEPRECATED listenerArn "Use generic-lens or generic-optics with 'listenerArn' instead"  #-}

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
{-# INLINEABLE mlAlpnPolicy #-}
{-# DEPRECATED alpnPolicy "Use generic-lens or generic-optics with 'alpnPolicy' instead"  #-}

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlCertificates :: Lens.Lens' ModifyListener (Core.Maybe [Types.Certificate])
mlCertificates = Lens.field @"certificates"
{-# INLINEABLE mlCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | The actions for the default rule.
--
-- /Note:/ Consider using 'defaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlDefaultActions :: Lens.Lens' ModifyListener (Core.Maybe [Types.Action])
mlDefaultActions = Lens.field @"defaultActions"
{-# INLINEABLE mlDefaultActions #-}
{-# DEPRECATED defaultActions "Use generic-lens or generic-optics with 'defaultActions' instead"  #-}

-- | The port for connections from clients to the load balancer. You cannot specify a port for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlPort :: Lens.Lens' ModifyListener (Core.Maybe Core.Natural)
mlPort = Lens.field @"port"
{-# INLINEABLE mlPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols. You can’t change the protocol to UDP or TCP_UDP if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlProtocol :: Lens.Lens' ModifyListener (Core.Maybe Types.ProtocolEnum)
mlProtocol = Lens.field @"protocol"
{-# INLINEABLE mlProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- /Note:/ Consider using 'sslPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlSslPolicy :: Lens.Lens' ModifyListener (Core.Maybe Types.SslPolicyName)
mlSslPolicy = Lens.field @"sslPolicy"
{-# INLINEABLE mlSslPolicy #-}
{-# DEPRECATED sslPolicy "Use generic-lens or generic-optics with 'sslPolicy' instead"  #-}

instance Core.ToQuery ModifyListener where
        toQuery ModifyListener{..}
          = Core.toQueryPair "Action" ("ModifyListener" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ListenerArn" listenerArn
              Core.<>
              Core.toQueryPair "AlpnPolicy"
                (Core.maybe Core.mempty (Core.toQueryList "member") alpnPolicy)
              Core.<>
              Core.toQueryPair "Certificates"
                (Core.maybe Core.mempty (Core.toQueryList "member") certificates)
              Core.<>
              Core.toQueryPair "DefaultActions"
                (Core.maybe Core.mempty (Core.toQueryList "member") defaultActions)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Protocol") protocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SslPolicy") sslPolicy

instance Core.ToHeaders ModifyListener where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyListener where
        type Rs ModifyListener = ModifyListenerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyListenerResult"
              (\ s h x ->
                 ModifyListenerResponse' Core.<$>
                   (x Core..@? "Listeners" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'
  { listeners :: Core.Maybe [Types.Listener]
    -- ^ Information about the modified listener.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyListenerResponse' value with any optional fields omitted.
mkModifyListenerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyListenerResponse
mkModifyListenerResponse responseStatus
  = ModifyListenerResponse'{listeners = Core.Nothing, responseStatus}

-- | Information about the modified listener.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrrsListeners :: Lens.Lens' ModifyListenerResponse (Core.Maybe [Types.Listener])
mlrrsListeners = Lens.field @"listeners"
{-# INLINEABLE mlrrsListeners #-}
{-# DEPRECATED listeners "Use generic-lens or generic-optics with 'listeners' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlrrsResponseStatus :: Lens.Lens' ModifyListenerResponse Core.Int
mlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

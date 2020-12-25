{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom authorization behavior by invoking a specified custom authorizer. Use this to test and debug the custom authorization behavior of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestInvokeAuthorizer
  ( -- * Creating a request
    TestInvokeAuthorizer (..),
    mkTestInvokeAuthorizer,

    -- ** Request lenses
    tiaAuthorizerName,
    tiaHttpContext,
    tiaMqttContext,
    tiaTlsContext,
    tiaToken,
    tiaTokenSignature,

    -- * Destructuring the response
    TestInvokeAuthorizerResponse (..),
    mkTestInvokeAuthorizerResponse,

    -- ** Response lenses
    tiarrsDisconnectAfterInSeconds,
    tiarrsIsAuthenticated,
    tiarrsPolicyDocuments,
    tiarrsPrincipalId,
    tiarrsRefreshAfterInSeconds,
    tiarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | The custom authorizer name.
    authorizerName :: Types.AuthorizerName,
    -- | Specifies a test HTTP authorization request.
    httpContext :: Core.Maybe Types.HttpContext,
    -- | Specifies a test MQTT authorization request.
    mqttContext :: Core.Maybe Types.MqttContext,
    -- | Specifies a test TLS authorization request.
    tlsContext :: Core.Maybe Types.TlsContext,
    -- | The token returned by your custom authentication service.
    token :: Core.Maybe Types.Token,
    -- | The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
    tokenSignature :: Core.Maybe Types.TokenSignature
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeAuthorizer' value with any optional fields omitted.
mkTestInvokeAuthorizer ::
  -- | 'authorizerName'
  Types.AuthorizerName ->
  TestInvokeAuthorizer
mkTestInvokeAuthorizer authorizerName =
  TestInvokeAuthorizer'
    { authorizerName,
      httpContext = Core.Nothing,
      mqttContext = Core.Nothing,
      tlsContext = Core.Nothing,
      token = Core.Nothing,
      tokenSignature = Core.Nothing
    }

-- | The custom authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaAuthorizerName :: Lens.Lens' TestInvokeAuthorizer Types.AuthorizerName
tiaAuthorizerName = Lens.field @"authorizerName"
{-# DEPRECATED tiaAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | Specifies a test HTTP authorization request.
--
-- /Note:/ Consider using 'httpContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaHttpContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Types.HttpContext)
tiaHttpContext = Lens.field @"httpContext"
{-# DEPRECATED tiaHttpContext "Use generic-lens or generic-optics with 'httpContext' instead." #-}

-- | Specifies a test MQTT authorization request.
--
-- /Note:/ Consider using 'mqttContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaMqttContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Types.MqttContext)
tiaMqttContext = Lens.field @"mqttContext"
{-# DEPRECATED tiaMqttContext "Use generic-lens or generic-optics with 'mqttContext' instead." #-}

-- | Specifies a test TLS authorization request.
--
-- /Note:/ Consider using 'tlsContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaTlsContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Types.TlsContext)
tiaTlsContext = Lens.field @"tlsContext"
{-# DEPRECATED tiaTlsContext "Use generic-lens or generic-optics with 'tlsContext' instead." #-}

-- | The token returned by your custom authentication service.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaToken :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Types.Token)
tiaToken = Lens.field @"token"
{-# DEPRECATED tiaToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
--
-- /Note:/ Consider using 'tokenSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiaTokenSignature :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Types.TokenSignature)
tiaTokenSignature = Lens.field @"tokenSignature"
{-# DEPRECATED tiaTokenSignature "Use generic-lens or generic-optics with 'tokenSignature' instead." #-}

instance Core.FromJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer {..} =
    Core.object
      ( Core.catMaybes
          [ ("httpContext" Core..=) Core.<$> httpContext,
            ("mqttContext" Core..=) Core.<$> mqttContext,
            ("tlsContext" Core..=) Core.<$> tlsContext,
            ("token" Core..=) Core.<$> token,
            ("tokenSignature" Core..=) Core.<$> tokenSignature
          ]
      )

instance Core.AWSRequest TestInvokeAuthorizer where
  type Rs TestInvokeAuthorizer = TestInvokeAuthorizerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/authorizer/" Core.<> (Core.toText authorizerName)
                Core.<> ("/test")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Core.<$> (x Core..:? "disconnectAfterInSeconds")
            Core.<*> (x Core..:? "isAuthenticated")
            Core.<*> (x Core..:? "policyDocuments")
            Core.<*> (x Core..:? "principalId")
            Core.<*> (x Core..:? "refreshAfterInSeconds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The number of seconds after which the connection is terminated.
    disconnectAfterInSeconds :: Core.Maybe Core.Int,
    -- | True if the token is authenticated, otherwise false.
    isAuthenticated :: Core.Maybe Core.Bool,
    -- | IAM policy documents.
    policyDocuments :: Core.Maybe [Types.PolicyDocument],
    -- | The principal ID.
    principalId :: Core.Maybe Types.PrincipalId,
    -- | The number of seconds after which the temporary credentials are refreshed.
    refreshAfterInSeconds :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestInvokeAuthorizerResponse' value with any optional fields omitted.
mkTestInvokeAuthorizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestInvokeAuthorizerResponse
mkTestInvokeAuthorizerResponse responseStatus =
  TestInvokeAuthorizerResponse'
    { disconnectAfterInSeconds =
        Core.Nothing,
      isAuthenticated = Core.Nothing,
      policyDocuments = Core.Nothing,
      principalId = Core.Nothing,
      refreshAfterInSeconds = Core.Nothing,
      responseStatus
    }

-- | The number of seconds after which the connection is terminated.
--
-- /Note:/ Consider using 'disconnectAfterInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsDisconnectAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
tiarrsDisconnectAfterInSeconds = Lens.field @"disconnectAfterInSeconds"
{-# DEPRECATED tiarrsDisconnectAfterInSeconds "Use generic-lens or generic-optics with 'disconnectAfterInSeconds' instead." #-}

-- | True if the token is authenticated, otherwise false.
--
-- /Note:/ Consider using 'isAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsIsAuthenticated :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Bool)
tiarrsIsAuthenticated = Lens.field @"isAuthenticated"
{-# DEPRECATED tiarrsIsAuthenticated "Use generic-lens or generic-optics with 'isAuthenticated' instead." #-}

-- | IAM policy documents.
--
-- /Note:/ Consider using 'policyDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsPolicyDocuments :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe [Types.PolicyDocument])
tiarrsPolicyDocuments = Lens.field @"policyDocuments"
{-# DEPRECATED tiarrsPolicyDocuments "Use generic-lens or generic-optics with 'policyDocuments' instead." #-}

-- | The principal ID.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsPrincipalId :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Types.PrincipalId)
tiarrsPrincipalId = Lens.field @"principalId"
{-# DEPRECATED tiarrsPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The number of seconds after which the temporary credentials are refreshed.
--
-- /Note:/ Consider using 'refreshAfterInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsRefreshAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
tiarrsRefreshAfterInSeconds = Lens.field @"refreshAfterInSeconds"
{-# DEPRECATED tiarrsRefreshAfterInSeconds "Use generic-lens or generic-optics with 'refreshAfterInSeconds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiarrsResponseStatus :: Lens.Lens' TestInvokeAuthorizerResponse Core.Int
tiarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tiarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an authorizer.
module Network.AWS.IoT.UpdateAuthorizer
  ( -- * Creating a request
    UpdateAuthorizer (..),
    mkUpdateAuthorizer,

    -- ** Request lenses
    uaAuthorizerName,
    uaAuthorizerFunctionArn,
    uaStatus,
    uaTokenKeyName,
    uaTokenSigningPublicKeys,

    -- * Destructuring the response
    UpdateAuthorizerResponse (..),
    mkUpdateAuthorizerResponse,

    -- ** Response lenses
    uarrsAuthorizerArn,
    uarrsAuthorizerName,
    uarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { -- | The authorizer name.
    authorizerName :: Types.AuthorizerName,
    -- | The ARN of the authorizer's Lambda function.
    authorizerFunctionArn :: Core.Maybe Types.AuthorizerFunctionArn,
    -- | The status of the update authorizer request.
    status :: Core.Maybe Types.AuthorizerStatus,
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Core.Maybe Types.TokenKeyName,
    -- | The public keys used to verify the token signature.
    tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAuthorizer' value with any optional fields omitted.
mkUpdateAuthorizer ::
  -- | 'authorizerName'
  Types.AuthorizerName ->
  UpdateAuthorizer
mkUpdateAuthorizer authorizerName =
  UpdateAuthorizer'
    { authorizerName,
      authorizerFunctionArn = Core.Nothing,
      status = Core.Nothing,
      tokenKeyName = Core.Nothing,
      tokenSigningPublicKeys = Core.Nothing
    }

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAuthorizerName :: Lens.Lens' UpdateAuthorizer Types.AuthorizerName
uaAuthorizerName = Lens.field @"authorizerName"
{-# DEPRECATED uaAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The ARN of the authorizer's Lambda function.
--
-- /Note:/ Consider using 'authorizerFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAuthorizerFunctionArn :: Lens.Lens' UpdateAuthorizer (Core.Maybe Types.AuthorizerFunctionArn)
uaAuthorizerFunctionArn = Lens.field @"authorizerFunctionArn"
{-# DEPRECATED uaAuthorizerFunctionArn "Use generic-lens or generic-optics with 'authorizerFunctionArn' instead." #-}

-- | The status of the update authorizer request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaStatus :: Lens.Lens' UpdateAuthorizer (Core.Maybe Types.AuthorizerStatus)
uaStatus = Lens.field @"status"
{-# DEPRECATED uaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTokenKeyName :: Lens.Lens' UpdateAuthorizer (Core.Maybe Types.TokenKeyName)
uaTokenKeyName = Lens.field @"tokenKeyName"
{-# DEPRECATED uaTokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead." #-}

-- | The public keys used to verify the token signature.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTokenSigningPublicKeys :: Lens.Lens' UpdateAuthorizer (Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue))
uaTokenSigningPublicKeys = Lens.field @"tokenSigningPublicKeys"
{-# DEPRECATED uaTokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead." #-}

instance Core.FromJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer {..} =
    Core.object
      ( Core.catMaybes
          [ ("authorizerFunctionArn" Core..=) Core.<$> authorizerFunctionArn,
            ("status" Core..=) Core.<$> status,
            ("tokenKeyName" Core..=) Core.<$> tokenKeyName,
            ("tokenSigningPublicKeys" Core..=)
              Core.<$> tokenSigningPublicKeys
          ]
      )

instance Core.AWSRequest UpdateAuthorizer where
  type Rs UpdateAuthorizer = UpdateAuthorizerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/authorizer/" Core.<> (Core.toText authorizerName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAuthorizerResponse'
            Core.<$> (x Core..:? "authorizerArn")
            Core.<*> (x Core..:? "authorizerName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Types.AuthorizerArn,
    -- | The authorizer name.
    authorizerName :: Core.Maybe Types.AuthorizerName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAuthorizerResponse' value with any optional fields omitted.
mkUpdateAuthorizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAuthorizerResponse
mkUpdateAuthorizerResponse responseStatus =
  UpdateAuthorizerResponse'
    { authorizerArn = Core.Nothing,
      authorizerName = Core.Nothing,
      responseStatus
    }

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAuthorizerArn :: Lens.Lens' UpdateAuthorizerResponse (Core.Maybe Types.AuthorizerArn)
uarrsAuthorizerArn = Lens.field @"authorizerArn"
{-# DEPRECATED uarrsAuthorizerArn "Use generic-lens or generic-optics with 'authorizerArn' instead." #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAuthorizerName :: Lens.Lens' UpdateAuthorizerResponse (Core.Maybe Types.AuthorizerName)
uarrsAuthorizerName = Lens.field @"authorizerName"
{-# DEPRECATED uarrsAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateAuthorizerResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

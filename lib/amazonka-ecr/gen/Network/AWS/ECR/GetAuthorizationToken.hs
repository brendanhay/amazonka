{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetAuthorizationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an authorization token. An authorization token represents your IAM authentication credentials and can be used to access any Amazon ECR registry that your IAM principal has access to. The authorization token is valid for 12 hours.
--
-- The @authorizationToken@ returned is a base64 encoded string that can be decoded and used in a @docker login@ command to authenticate to a registry. The AWS CLI offers an @get-login-password@ command that simplifies the login process. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Registries.html#registry_auth Registry Authentication> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.GetAuthorizationToken
  ( -- * Creating a request
    GetAuthorizationToken (..),
    mkGetAuthorizationToken,

    -- ** Request lenses
    gatRegistryIds,

    -- * Destructuring the response
    GetAuthorizationTokenResponse (..),
    mkGetAuthorizationTokenResponse,

    -- ** Response lenses
    gatrrsAuthorizationData,
    gatrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAuthorizationToken' smart constructor.
newtype GetAuthorizationToken = GetAuthorizationToken'
  { -- | A list of AWS account IDs that are associated with the registries for which to get AuthorizationData objects. If you do not specify a registry, the default registry is assumed.
    registryIds :: Core.Maybe (Core.NonEmpty Types.RegistryId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAuthorizationToken' value with any optional fields omitted.
mkGetAuthorizationToken ::
  GetAuthorizationToken
mkGetAuthorizationToken =
  GetAuthorizationToken' {registryIds = Core.Nothing}

-- | A list of AWS account IDs that are associated with the registries for which to get AuthorizationData objects. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatRegistryIds :: Lens.Lens' GetAuthorizationToken (Core.Maybe (Core.NonEmpty Types.RegistryId))
gatRegistryIds = Lens.field @"registryIds"
{-# DEPRECATED gatRegistryIds "Use generic-lens or generic-optics with 'registryIds' instead." #-}

instance Core.FromJSON GetAuthorizationToken where
  toJSON GetAuthorizationToken {..} =
    Core.object
      (Core.catMaybes [("registryIds" Core..=) Core.<$> registryIds])

instance Core.AWSRequest GetAuthorizationToken where
  type Rs GetAuthorizationToken = GetAuthorizationTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.GetAuthorizationToken"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizationTokenResponse'
            Core.<$> (x Core..:? "authorizationData")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAuthorizationTokenResponse' smart constructor.
data GetAuthorizationTokenResponse = GetAuthorizationTokenResponse'
  { -- | A list of authorization token data objects that correspond to the @registryIds@ values in the request.
    authorizationData :: Core.Maybe [Types.AuthorizationData],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAuthorizationTokenResponse' value with any optional fields omitted.
mkGetAuthorizationTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAuthorizationTokenResponse
mkGetAuthorizationTokenResponse responseStatus =
  GetAuthorizationTokenResponse'
    { authorizationData = Core.Nothing,
      responseStatus
    }

-- | A list of authorization token data objects that correspond to the @registryIds@ values in the request.
--
-- /Note:/ Consider using 'authorizationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrrsAuthorizationData :: Lens.Lens' GetAuthorizationTokenResponse (Core.Maybe [Types.AuthorizationData])
gatrrsAuthorizationData = Lens.field @"authorizationData"
{-# DEPRECATED gatrrsAuthorizationData "Use generic-lens or generic-optics with 'authorizationData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrrsResponseStatus :: Lens.Lens' GetAuthorizationTokenResponse Core.Int
gatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

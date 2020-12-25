{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetFederationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
module Network.AWS.Connect.GetFederationToken
  ( -- * Creating a request
    GetFederationToken (..),
    mkGetFederationToken,

    -- ** Request lenses
    gftInstanceId,

    -- * Destructuring the response
    GetFederationTokenResponse (..),
    mkGetFederationTokenResponse,

    -- ** Response lenses
    gftrrsCredentials,
    gftrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFederationToken' smart constructor.
newtype GetFederationToken = GetFederationToken'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFederationToken' value with any optional fields omitted.
mkGetFederationToken ::
  -- | 'instanceId'
  Types.InstanceId ->
  GetFederationToken
mkGetFederationToken instanceId = GetFederationToken' {instanceId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftInstanceId :: Lens.Lens' GetFederationToken Types.InstanceId
gftInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gftInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.AWSRequest GetFederationToken where
  type Rs GetFederationToken = GetFederationTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/user/federate/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFederationTokenResponse'
            Core.<$> (x Core..:? "Credentials") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The credentials to use for federation.
    credentials :: Core.Maybe Types.Credentials,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetFederationTokenResponse' value with any optional fields omitted.
mkGetFederationTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFederationTokenResponse
mkGetFederationTokenResponse responseStatus =
  GetFederationTokenResponse'
    { credentials = Core.Nothing,
      responseStatus
    }

-- | The credentials to use for federation.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsCredentials :: Lens.Lens' GetFederationTokenResponse (Core.Maybe Types.Credentials)
gftrrsCredentials = Lens.field @"credentials"
{-# DEPRECATED gftrrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsResponseStatus :: Lens.Lens' GetFederationTokenResponse Core.Int
gftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

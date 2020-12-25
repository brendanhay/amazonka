{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.RetryBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a build.
module Network.AWS.CodeBuild.RetryBuild
  ( -- * Creating a request
    RetryBuild (..),
    mkRetryBuild,

    -- ** Request lenses
    rbId,
    rbIdempotencyToken,

    -- * Destructuring the response
    RetryBuildResponse (..),
    mkRetryBuildResponse,

    -- ** Response lenses
    rbrrsBuild,
    rbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRetryBuild' smart constructor.
data RetryBuild = RetryBuild'
  { -- | Specifies the identifier of the build to restart.
    id :: Core.Maybe Types.Id,
    -- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryBuild' value with any optional fields omitted.
mkRetryBuild ::
  RetryBuild
mkRetryBuild =
  RetryBuild' {id = Core.Nothing, idempotencyToken = Core.Nothing}

-- | Specifies the identifier of the build to restart.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbId :: Lens.Lens' RetryBuild (Core.Maybe Types.Id)
rbId = Lens.field @"id"
{-# DEPRECATED rbId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbIdempotencyToken :: Lens.Lens' RetryBuild (Core.Maybe Types.String)
rbIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED rbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Core.FromJSON RetryBuild where
  toJSON RetryBuild {..} =
    Core.object
      ( Core.catMaybes
          [ ("id" Core..=) Core.<$> id,
            ("idempotencyToken" Core..=) Core.<$> idempotencyToken
          ]
      )

instance Core.AWSRequest RetryBuild where
  type Rs RetryBuild = RetryBuildResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.RetryBuild")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryBuildResponse'
            Core.<$> (x Core..:? "build") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRetryBuildResponse' smart constructor.
data RetryBuildResponse = RetryBuildResponse'
  { build :: Core.Maybe Types.Build,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RetryBuildResponse' value with any optional fields omitted.
mkRetryBuildResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RetryBuildResponse
mkRetryBuildResponse responseStatus =
  RetryBuildResponse' {build = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsBuild :: Lens.Lens' RetryBuildResponse (Core.Maybe Types.Build)
rbrrsBuild = Lens.field @"build"
{-# DEPRECATED rbrrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsResponseStatus :: Lens.Lens' RetryBuildResponse Core.Int
rbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.InvalidateProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the cache for a project.
module Network.AWS.CodeBuild.InvalidateProjectCache
  ( -- * Creating a request
    InvalidateProjectCache (..),
    mkInvalidateProjectCache,

    -- ** Request lenses
    ipcProjectName,

    -- * Destructuring the response
    InvalidateProjectCacheResponse (..),
    mkInvalidateProjectCacheResponse,

    -- ** Response lenses
    ipcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInvalidateProjectCache' smart constructor.
newtype InvalidateProjectCache = InvalidateProjectCache'
  { -- | The name of the AWS CodeBuild build project that the cache is reset for.
    projectName :: Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InvalidateProjectCache' value with any optional fields omitted.
mkInvalidateProjectCache ::
  -- | 'projectName'
  Types.NonEmptyString ->
  InvalidateProjectCache
mkInvalidateProjectCache projectName =
  InvalidateProjectCache' {projectName}

-- | The name of the AWS CodeBuild build project that the cache is reset for.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcProjectName :: Lens.Lens' InvalidateProjectCache Types.NonEmptyString
ipcProjectName = Lens.field @"projectName"
{-# DEPRECATED ipcProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Core.FromJSON InvalidateProjectCache where
  toJSON InvalidateProjectCache {..} =
    Core.object
      (Core.catMaybes [Core.Just ("projectName" Core..= projectName)])

instance Core.AWSRequest InvalidateProjectCache where
  type Rs InvalidateProjectCache = InvalidateProjectCacheResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.InvalidateProjectCache")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          InvalidateProjectCacheResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkInvalidateProjectCacheResponse' smart constructor.
newtype InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InvalidateProjectCacheResponse' value with any optional fields omitted.
mkInvalidateProjectCacheResponse ::
  -- | 'responseStatus'
  Core.Int ->
  InvalidateProjectCacheResponse
mkInvalidateProjectCacheResponse responseStatus =
  InvalidateProjectCacheResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcrrsResponseStatus :: Lens.Lens' InvalidateProjectCacheResponse Core.Int
ipcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ipcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more builds.
module Network.AWS.CodeBuild.BatchGetBuilds
  ( -- * Creating a request
    BatchGetBuilds (..),
    mkBatchGetBuilds,

    -- ** Request lenses
    bgbIds,

    -- * Destructuring the response
    BatchGetBuildsResponse (..),
    mkBatchGetBuildsResponse,

    -- ** Response lenses
    bgbrrsBuilds,
    bgbrrsBuildsNotFound,
    bgbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetBuilds' smart constructor.
newtype BatchGetBuilds = BatchGetBuilds'
  { -- | The IDs of the builds.
    ids :: Core.NonEmpty Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetBuilds' value with any optional fields omitted.
mkBatchGetBuilds ::
  -- | 'ids'
  Core.NonEmpty Types.NonEmptyString ->
  BatchGetBuilds
mkBatchGetBuilds ids = BatchGetBuilds' {ids}

-- | The IDs of the builds.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbIds :: Lens.Lens' BatchGetBuilds (Core.NonEmpty Types.NonEmptyString)
bgbIds = Lens.field @"ids"
{-# DEPRECATED bgbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Core.FromJSON BatchGetBuilds where
  toJSON BatchGetBuilds {..} =
    Core.object (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.AWSRequest BatchGetBuilds where
  type Rs BatchGetBuilds = BatchGetBuildsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchGetBuilds")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildsResponse'
            Core.<$> (x Core..:? "builds")
            Core.<*> (x Core..:? "buildsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { -- | Information about the requested builds.
    builds :: Core.Maybe [Types.Build],
    -- | The IDs of builds for which information could not be found.
    buildsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetBuildsResponse' value with any optional fields omitted.
mkBatchGetBuildsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetBuildsResponse
mkBatchGetBuildsResponse responseStatus =
  BatchGetBuildsResponse'
    { builds = Core.Nothing,
      buildsNotFound = Core.Nothing,
      responseStatus
    }

-- | Information about the requested builds.
--
-- /Note:/ Consider using 'builds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsBuilds :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe [Types.Build])
bgbrrsBuilds = Lens.field @"builds"
{-# DEPRECATED bgbrrsBuilds "Use generic-lens or generic-optics with 'builds' instead." #-}

-- | The IDs of builds for which information could not be found.
--
-- /Note:/ Consider using 'buildsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsBuildsNotFound :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgbrrsBuildsNotFound = Lens.field @"buildsNotFound"
{-# DEPRECATED bgbrrsBuildsNotFound "Use generic-lens or generic-optics with 'buildsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsResponseStatus :: Lens.Lens' BatchGetBuildsResponse Core.Int
bgbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

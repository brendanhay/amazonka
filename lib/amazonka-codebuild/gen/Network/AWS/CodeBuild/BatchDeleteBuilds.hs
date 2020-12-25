{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchDeleteBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more builds.
module Network.AWS.CodeBuild.BatchDeleteBuilds
  ( -- * Creating a request
    BatchDeleteBuilds (..),
    mkBatchDeleteBuilds,

    -- ** Request lenses
    bdbIds,

    -- * Destructuring the response
    BatchDeleteBuildsResponse (..),
    mkBatchDeleteBuildsResponse,

    -- ** Response lenses
    bdbrrsBuildsDeleted,
    bdbrrsBuildsNotDeleted,
    bdbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteBuilds' smart constructor.
newtype BatchDeleteBuilds = BatchDeleteBuilds'
  { -- | The IDs of the builds to delete.
    ids :: Core.NonEmpty Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteBuilds' value with any optional fields omitted.
mkBatchDeleteBuilds ::
  -- | 'ids'
  Core.NonEmpty Types.NonEmptyString ->
  BatchDeleteBuilds
mkBatchDeleteBuilds ids = BatchDeleteBuilds' {ids}

-- | The IDs of the builds to delete.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbIds :: Lens.Lens' BatchDeleteBuilds (Core.NonEmpty Types.NonEmptyString)
bdbIds = Lens.field @"ids"
{-# DEPRECATED bdbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Core.FromJSON BatchDeleteBuilds where
  toJSON BatchDeleteBuilds {..} =
    Core.object (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.AWSRequest BatchDeleteBuilds where
  type Rs BatchDeleteBuilds = BatchDeleteBuildsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchDeleteBuilds")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteBuildsResponse'
            Core.<$> (x Core..:? "buildsDeleted")
            Core.<*> (x Core..:? "buildsNotDeleted")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDeleteBuildsResponse' smart constructor.
data BatchDeleteBuildsResponse = BatchDeleteBuildsResponse'
  { -- | The IDs of the builds that were successfully deleted.
    buildsDeleted :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | Information about any builds that could not be successfully deleted.
    buildsNotDeleted :: Core.Maybe [Types.BuildNotDeleted],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteBuildsResponse' value with any optional fields omitted.
mkBatchDeleteBuildsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDeleteBuildsResponse
mkBatchDeleteBuildsResponse responseStatus =
  BatchDeleteBuildsResponse'
    { buildsDeleted = Core.Nothing,
      buildsNotDeleted = Core.Nothing,
      responseStatus
    }

-- | The IDs of the builds that were successfully deleted.
--
-- /Note:/ Consider using 'buildsDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrrsBuildsDeleted :: Lens.Lens' BatchDeleteBuildsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bdbrrsBuildsDeleted = Lens.field @"buildsDeleted"
{-# DEPRECATED bdbrrsBuildsDeleted "Use generic-lens or generic-optics with 'buildsDeleted' instead." #-}

-- | Information about any builds that could not be successfully deleted.
--
-- /Note:/ Consider using 'buildsNotDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrrsBuildsNotDeleted :: Lens.Lens' BatchDeleteBuildsResponse (Core.Maybe [Types.BuildNotDeleted])
bdbrrsBuildsNotDeleted = Lens.field @"buildsNotDeleted"
{-# DEPRECATED bdbrrsBuildsNotDeleted "Use generic-lens or generic-optics with 'buildsNotDeleted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrrsResponseStatus :: Lens.Lens' BatchDeleteBuildsResponse Core.Int
bdbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

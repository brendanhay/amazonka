{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a manual snapshot of an instance or disk as another manual snapshot, or copies an automatic snapshot of an instance or disk as a manual snapshot. This operation can also be used to copy a manual or automatic snapshot of an instance or a disk from one AWS Region to another in Amazon Lightsail.
--
-- When copying a /manual snapshot/ , be sure to define the @source region@ , @source snapshot name@ , and @target snapshot name@ parameters.
-- When copying an /automatic snapshot/ , be sure to define the @source region@ , @source resource name@ , @target snapshot name@ , and either the @restore date@ or the @use latest restorable auto snapshot@ parameters.
module Network.AWS.Lightsail.CopySnapshot
  ( -- * Creating a request
    CopySnapshot (..),
    mkCopySnapshot,

    -- ** Request lenses
    csTargetSnapshotName,
    csSourceRegion,
    csRestoreDate,
    csSourceResourceName,
    csSourceSnapshotName,
    csUseLatestRestorableAutoSnapshot,

    -- * Destructuring the response
    CopySnapshotResponse (..),
    mkCopySnapshotResponse,

    -- ** Response lenses
    csrrsOperations,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The name of the new manual snapshot to be created as a copy.
    targetSnapshotName :: Types.ResourceName,
    -- | The AWS Region where the source manual or automatic snapshot is located.
    sourceRegion :: Types.RegionName,
    -- | The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
    --
    -- Constraints:
    --
    --     * Must be specified in @YYYY-MM-DD@ format.
    --
    --
    --     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
    --
    --
    --     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
    restoreDate :: Core.Maybe Types.String,
    -- | The name of the source instance or disk from which the source automatic snapshot was created.
    --
    -- Constraint:
    --
    --     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
    sourceResourceName :: Core.Maybe Types.String,
    -- | The name of the source manual snapshot to copy.
    --
    -- Constraint:
    --
    --     * Define this parameter only when copying a manual snapshot as another manual snapshot.
    sourceSnapshotName :: Core.Maybe Types.ResourceName,
    -- | A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk.
    --
    -- Constraints:
    --
    --     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
    --
    --
    --     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
    useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopySnapshot' value with any optional fields omitted.
mkCopySnapshot ::
  -- | 'targetSnapshotName'
  Types.ResourceName ->
  -- | 'sourceRegion'
  Types.RegionName ->
  CopySnapshot
mkCopySnapshot targetSnapshotName sourceRegion =
  CopySnapshot'
    { targetSnapshotName,
      sourceRegion,
      restoreDate = Core.Nothing,
      sourceResourceName = Core.Nothing,
      sourceSnapshotName = Core.Nothing,
      useLatestRestorableAutoSnapshot = Core.Nothing
    }

-- | The name of the new manual snapshot to be created as a copy.
--
-- /Note:/ Consider using 'targetSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetSnapshotName :: Lens.Lens' CopySnapshot Types.ResourceName
csTargetSnapshotName = Lens.field @"targetSnapshotName"
{-# DEPRECATED csTargetSnapshotName "Use generic-lens or generic-optics with 'targetSnapshotName' instead." #-}

-- | The AWS Region where the source manual or automatic snapshot is located.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceRegion :: Lens.Lens' CopySnapshot Types.RegionName
csSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED csSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRestoreDate :: Lens.Lens' CopySnapshot (Core.Maybe Types.String)
csRestoreDate = Lens.field @"restoreDate"
{-# DEPRECATED csRestoreDate "Use generic-lens or generic-optics with 'restoreDate' instead." #-}

-- | The name of the source instance or disk from which the source automatic snapshot was created.
--
-- Constraint:
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceResourceName :: Lens.Lens' CopySnapshot (Core.Maybe Types.String)
csSourceResourceName = Lens.field @"sourceResourceName"
{-# DEPRECATED csSourceResourceName "Use generic-lens or generic-optics with 'sourceResourceName' instead." #-}

-- | The name of the source manual snapshot to copy.
--
-- Constraint:
--
--     * Define this parameter only when copying a manual snapshot as another manual snapshot.
--
--
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSnapshotName :: Lens.Lens' CopySnapshot (Core.Maybe Types.ResourceName)
csSourceSnapshotName = Lens.field @"sourceSnapshotName"
{-# DEPRECATED csSourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead." #-}

-- | A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseLatestRestorableAutoSnapshot :: Lens.Lens' CopySnapshot (Core.Maybe Core.Bool)
csUseLatestRestorableAutoSnapshot = Lens.field @"useLatestRestorableAutoSnapshot"
{-# DEPRECATED csUseLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead." #-}

instance Core.FromJSON CopySnapshot where
  toJSON CopySnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targetSnapshotName" Core..= targetSnapshotName),
            Core.Just ("sourceRegion" Core..= sourceRegion),
            ("restoreDate" Core..=) Core.<$> restoreDate,
            ("sourceResourceName" Core..=) Core.<$> sourceResourceName,
            ("sourceSnapshotName" Core..=) Core.<$> sourceSnapshotName,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Core.<$> useLatestRestorableAutoSnapshot
          ]
      )

instance Core.AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.CopySnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopySnapshotResponse' value with any optional fields omitted.
mkCopySnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopySnapshotResponse
mkCopySnapshotResponse responseStatus =
  CopySnapshotResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsOperations :: Lens.Lens' CopySnapshotResponse (Core.Maybe [Types.Operation])
csrrsOperations = Lens.field @"operations"
{-# DEPRECATED csrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CopySnapshotResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

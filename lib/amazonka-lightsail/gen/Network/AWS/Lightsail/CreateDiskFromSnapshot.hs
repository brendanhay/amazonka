{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk from a manual or automatic snapshot of a disk. The resulting disk can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
-- The @create disk from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @disk snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDiskFromSnapshot
  ( -- * Creating a request
    CreateDiskFromSnapshot (..),
    mkCreateDiskFromSnapshot,

    -- ** Request lenses
    cdfsDiskName,
    cdfsAvailabilityZone,
    cdfsSizeInGb,
    cdfsAddOns,
    cdfsDiskSnapshotName,
    cdfsRestoreDate,
    cdfsSourceDiskName,
    cdfsTags,
    cdfsUseLatestRestorableAutoSnapshot,

    -- * Destructuring the response
    CreateDiskFromSnapshotResponse (..),
    mkCreateDiskFromSnapshotResponse,

    -- ** Response lenses
    cdfsrrsOperations,
    cdfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { -- | The unique Lightsail disk name (e.g., @my-disk@ ).
    diskName :: Types.DiskName,
    -- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
    --
    -- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
    availabilityZone :: Types.NonEmptyString,
    -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Core.Int,
    -- | An array of objects that represent the add-ons to enable for the new disk.
    addOns :: Core.Maybe [Types.AddOnRequest],
    -- | The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
    --
    -- Constraint:
    --
    --     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
    diskSnapshotName :: Core.Maybe Types.DiskSnapshotName,
    -- | The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
    --
    -- Constraints:
    --
    --     * Must be specified in @YYYY-MM-DD@ format.
    --
    --
    --     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
    --
    --
    --     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
    restoreDate :: Core.Maybe Types.RestoreDate,
    -- | The name of the source disk from which the source automatic snapshot was created.
    --
    -- Constraints:
    --
    --     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.
    --
    --
    --     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
    sourceDiskName :: Core.Maybe Types.SourceDiskName,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Core.Maybe [Types.Tag],
    -- | A Boolean value to indicate whether to use the latest available automatic snapshot.
    --
    -- Constraints:
    --
    --     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
    --
    --
    --     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
    useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDiskFromSnapshot' value with any optional fields omitted.
mkCreateDiskFromSnapshot ::
  -- | 'diskName'
  Types.DiskName ->
  -- | 'availabilityZone'
  Types.NonEmptyString ->
  -- | 'sizeInGb'
  Core.Int ->
  CreateDiskFromSnapshot
mkCreateDiskFromSnapshot diskName availabilityZone sizeInGb =
  CreateDiskFromSnapshot'
    { diskName,
      availabilityZone,
      sizeInGb,
      addOns = Core.Nothing,
      diskSnapshotName = Core.Nothing,
      restoreDate = Core.Nothing,
      sourceDiskName = Core.Nothing,
      tags = Core.Nothing,
      useLatestRestorableAutoSnapshot = Core.Nothing
    }

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsDiskName :: Lens.Lens' CreateDiskFromSnapshot Types.DiskName
cdfsDiskName = Lens.field @"diskName"
{-# DEPRECATED cdfsDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAvailabilityZone :: Lens.Lens' CreateDiskFromSnapshot Types.NonEmptyString
cdfsAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED cdfsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsSizeInGb :: Lens.Lens' CreateDiskFromSnapshot Core.Int
cdfsSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED cdfsSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | An array of objects that represent the add-ons to enable for the new disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAddOns :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [Types.AddOnRequest])
cdfsAddOns = Lens.field @"addOns"
{-# DEPRECATED cdfsAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
--
--
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsDiskSnapshotName :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Types.DiskSnapshotName)
cdfsDiskSnapshotName = Lens.field @"diskSnapshotName"
{-# DEPRECATED cdfsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

-- | The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsRestoreDate :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Types.RestoreDate)
cdfsRestoreDate = Lens.field @"restoreDate"
{-# DEPRECATED cdfsRestoreDate "Use generic-lens or generic-optics with 'restoreDate' instead." #-}

-- | The name of the source disk from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsSourceDiskName :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Types.SourceDiskName)
cdfsSourceDiskName = Lens.field @"sourceDiskName"
{-# DEPRECATED cdfsSourceDiskName "Use generic-lens or generic-optics with 'sourceDiskName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsTags :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [Types.Tag])
cdfsTags = Lens.field @"tags"
{-# DEPRECATED cdfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsUseLatestRestorableAutoSnapshot :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Bool)
cdfsUseLatestRestorableAutoSnapshot = Lens.field @"useLatestRestorableAutoSnapshot"
{-# DEPRECATED cdfsUseLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead." #-}

instance Core.FromJSON CreateDiskFromSnapshot where
  toJSON CreateDiskFromSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("diskName" Core..= diskName),
            Core.Just ("availabilityZone" Core..= availabilityZone),
            Core.Just ("sizeInGb" Core..= sizeInGb),
            ("addOns" Core..=) Core.<$> addOns,
            ("diskSnapshotName" Core..=) Core.<$> diskSnapshotName,
            ("restoreDate" Core..=) Core.<$> restoreDate,
            ("sourceDiskName" Core..=) Core.<$> sourceDiskName,
            ("tags" Core..=) Core.<$> tags,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Core.<$> useLatestRestorableAutoSnapshot
          ]
      )

instance Core.AWSRequest CreateDiskFromSnapshot where
  type Rs CreateDiskFromSnapshot = CreateDiskFromSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.CreateDiskFromSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskFromSnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDiskFromSnapshotResponse' value with any optional fields omitted.
mkCreateDiskFromSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDiskFromSnapshotResponse
mkCreateDiskFromSnapshotResponse responseStatus =
  CreateDiskFromSnapshotResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrrsOperations :: Lens.Lens' CreateDiskFromSnapshotResponse (Core.Maybe [Types.Operation])
cdfsrrsOperations = Lens.field @"operations"
{-# DEPRECATED cdfsrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrrsResponseStatus :: Lens.Lens' CreateDiskFromSnapshotResponse Core.Int
cdfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDiskFromSnapshot (..)
    , mkCreateDiskFromSnapshot
    -- ** Request lenses
    , cdfsDiskName
    , cdfsAvailabilityZone
    , cdfsSizeInGb
    , cdfsAddOns
    , cdfsDiskSnapshotName
    , cdfsRestoreDate
    , cdfsSourceDiskName
    , cdfsTags
    , cdfsUseLatestRestorableAutoSnapshot

    -- * Destructuring the response
    , CreateDiskFromSnapshotResponse (..)
    , mkCreateDiskFromSnapshotResponse
    -- ** Response lenses
    , cdfsrrsOperations
    , cdfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { diskName :: Types.DiskName
    -- ^ The unique Lightsail disk name (e.g., @my-disk@ ).
  , availabilityZone :: Types.NonEmptyString
    -- ^ The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
  , sizeInGb :: Core.Int
    -- ^ The size of the disk in GB (e.g., @32@ ).
  , addOns :: Core.Maybe [Types.AddOnRequest]
    -- ^ An array of objects that represent the add-ons to enable for the new disk.
  , diskSnapshotName :: Core.Maybe Types.DiskSnapshotName
    -- ^ The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
--
--
  , restoreDate :: Core.Maybe Core.Text
    -- ^ The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
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
  , sourceDiskName :: Core.Maybe Core.Text
    -- ^ The name of the source disk from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  , useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool
    -- ^ A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDiskFromSnapshot' value with any optional fields omitted.
mkCreateDiskFromSnapshot
    :: Types.DiskName -- ^ 'diskName'
    -> Types.NonEmptyString -- ^ 'availabilityZone'
    -> Core.Int -- ^ 'sizeInGb'
    -> CreateDiskFromSnapshot
mkCreateDiskFromSnapshot diskName availabilityZone sizeInGb
  = CreateDiskFromSnapshot'{diskName, availabilityZone, sizeInGb,
                            addOns = Core.Nothing, diskSnapshotName = Core.Nothing,
                            restoreDate = Core.Nothing, sourceDiskName = Core.Nothing,
                            tags = Core.Nothing,
                            useLatestRestorableAutoSnapshot = Core.Nothing}

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsDiskName :: Lens.Lens' CreateDiskFromSnapshot Types.DiskName
cdfsDiskName = Lens.field @"diskName"
{-# INLINEABLE cdfsDiskName #-}
{-# DEPRECATED diskName "Use generic-lens or generic-optics with 'diskName' instead"  #-}

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAvailabilityZone :: Lens.Lens' CreateDiskFromSnapshot Types.NonEmptyString
cdfsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cdfsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsSizeInGb :: Lens.Lens' CreateDiskFromSnapshot Core.Int
cdfsSizeInGb = Lens.field @"sizeInGb"
{-# INLINEABLE cdfsSizeInGb #-}
{-# DEPRECATED sizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead"  #-}

-- | An array of objects that represent the add-ons to enable for the new disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAddOns :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [Types.AddOnRequest])
cdfsAddOns = Lens.field @"addOns"
{-# INLINEABLE cdfsAddOns #-}
{-# DEPRECATED addOns "Use generic-lens or generic-optics with 'addOns' instead"  #-}

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
{-# INLINEABLE cdfsDiskSnapshotName #-}
{-# DEPRECATED diskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead"  #-}

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
cdfsRestoreDate :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Text)
cdfsRestoreDate = Lens.field @"restoreDate"
{-# INLINEABLE cdfsRestoreDate #-}
{-# DEPRECATED restoreDate "Use generic-lens or generic-optics with 'restoreDate' instead"  #-}

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
cdfsSourceDiskName :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Text)
cdfsSourceDiskName = Lens.field @"sourceDiskName"
{-# INLINEABLE cdfsSourceDiskName #-}
{-# DEPRECATED sourceDiskName "Use generic-lens or generic-optics with 'sourceDiskName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsTags :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [Types.Tag])
cdfsTags = Lens.field @"tags"
{-# INLINEABLE cdfsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

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
{-# INLINEABLE cdfsUseLatestRestorableAutoSnapshot #-}
{-# DEPRECATED useLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead"  #-}

instance Core.ToQuery CreateDiskFromSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDiskFromSnapshot where
        toHeaders CreateDiskFromSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateDiskFromSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDiskFromSnapshot where
        toJSON CreateDiskFromSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("diskName" Core..= diskName),
                  Core.Just ("availabilityZone" Core..= availabilityZone),
                  Core.Just ("sizeInGb" Core..= sizeInGb),
                  ("addOns" Core..=) Core.<$> addOns,
                  ("diskSnapshotName" Core..=) Core.<$> diskSnapshotName,
                  ("restoreDate" Core..=) Core.<$> restoreDate,
                  ("sourceDiskName" Core..=) Core.<$> sourceDiskName,
                  ("tags" Core..=) Core.<$> tags,
                  ("useLatestRestorableAutoSnapshot" Core..=) Core.<$>
                    useLatestRestorableAutoSnapshot])

instance Core.AWSRequest CreateDiskFromSnapshot where
        type Rs CreateDiskFromSnapshot = CreateDiskFromSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDiskFromSnapshotResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDiskFromSnapshotResponse' value with any optional fields omitted.
mkCreateDiskFromSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDiskFromSnapshotResponse
mkCreateDiskFromSnapshotResponse responseStatus
  = CreateDiskFromSnapshotResponse'{operations = Core.Nothing,
                                    responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrrsOperations :: Lens.Lens' CreateDiskFromSnapshotResponse (Core.Maybe [Types.Operation])
cdfsrrsOperations = Lens.field @"operations"
{-# INLINEABLE cdfsrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrrsResponseStatus :: Lens.Lens' CreateDiskFromSnapshotResponse Core.Int
cdfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.
--
-- You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.
-- You can also use this operation to create a snapshot of an instance's system volume. You might want to do this, for example, to recover data from the system volume of a botched instance or to create a backup of the system volume like you would for a block storage disk. To create a snapshot of a system volume, just define the @instance name@ parameter when issuing the snapshot command, and a snapshot of the defined instance's system volume will be created. After the snapshot is available, you can create a block storage disk from the snapshot and attach it to a running instance to access the data on the disk.
-- The @create disk snapshot@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDiskSnapshot
    (
    -- * Creating a request
      CreateDiskSnapshot (..)
    , mkCreateDiskSnapshot
    -- ** Request lenses
    , cdsDiskSnapshotName
    , cdsDiskName
    , cdsInstanceName
    , cdsTags

    -- * Destructuring the response
    , CreateDiskSnapshotResponse (..)
    , mkCreateDiskSnapshotResponse
    -- ** Response lenses
    , cdsrrsOperations
    , cdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDiskSnapshot' smart constructor.
data CreateDiskSnapshot = CreateDiskSnapshot'
  { diskSnapshotName :: Types.ResourceName
    -- ^ The name of the destination disk snapshot (e.g., @my-disk-snapshot@ ) based on the source disk.
  , diskName :: Core.Maybe Types.ResourceName
    -- ^ The unique name of the source disk (e.g., @Disk-Virginia-1@ ).
  , instanceName :: Core.Maybe Types.ResourceName
    -- ^ The unique name of the source instance (e.g., @Amazon_Linux-512MB-Virginia-1@ ). When this is defined, a snapshot of the instance's system volume is created.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDiskSnapshot' value with any optional fields omitted.
mkCreateDiskSnapshot
    :: Types.ResourceName -- ^ 'diskSnapshotName'
    -> CreateDiskSnapshot
mkCreateDiskSnapshot diskSnapshotName
  = CreateDiskSnapshot'{diskSnapshotName, diskName = Core.Nothing,
                        instanceName = Core.Nothing, tags = Core.Nothing}

-- | The name of the destination disk snapshot (e.g., @my-disk-snapshot@ ) based on the source disk.
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDiskSnapshotName :: Lens.Lens' CreateDiskSnapshot Types.ResourceName
cdsDiskSnapshotName = Lens.field @"diskSnapshotName"
{-# INLINEABLE cdsDiskSnapshotName #-}
{-# DEPRECATED diskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead"  #-}

-- | The unique name of the source disk (e.g., @Disk-Virginia-1@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDiskName :: Lens.Lens' CreateDiskSnapshot (Core.Maybe Types.ResourceName)
cdsDiskName = Lens.field @"diskName"
{-# INLINEABLE cdsDiskName #-}
{-# DEPRECATED diskName "Use generic-lens or generic-optics with 'diskName' instead"  #-}

-- | The unique name of the source instance (e.g., @Amazon_Linux-512MB-Virginia-1@ ). When this is defined, a snapshot of the instance's system volume is created.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsInstanceName :: Lens.Lens' CreateDiskSnapshot (Core.Maybe Types.ResourceName)
cdsInstanceName = Lens.field @"instanceName"
{-# INLINEABLE cdsInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTags :: Lens.Lens' CreateDiskSnapshot (Core.Maybe [Types.Tag])
cdsTags = Lens.field @"tags"
{-# INLINEABLE cdsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDiskSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDiskSnapshot where
        toHeaders CreateDiskSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateDiskSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDiskSnapshot where
        toJSON CreateDiskSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("diskSnapshotName" Core..= diskSnapshotName),
                  ("diskName" Core..=) Core.<$> diskName,
                  ("instanceName" Core..=) Core.<$> instanceName,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDiskSnapshot where
        type Rs CreateDiskSnapshot = CreateDiskSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDiskSnapshotResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDiskSnapshotResponse' smart constructor.
data CreateDiskSnapshotResponse = CreateDiskSnapshotResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDiskSnapshotResponse' value with any optional fields omitted.
mkCreateDiskSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDiskSnapshotResponse
mkCreateDiskSnapshotResponse responseStatus
  = CreateDiskSnapshotResponse'{operations = Core.Nothing,
                                responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsOperations :: Lens.Lens' CreateDiskSnapshotResponse (Core.Maybe [Types.Operation])
cdsrrsOperations = Lens.field @"operations"
{-# INLINEABLE cdsrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsResponseStatus :: Lens.Lens' CreateDiskSnapshotResponse Core.Int
cdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

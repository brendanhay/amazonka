{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS cloud.
module Network.AWS.DirectoryService.CreateSnapshot
    (
    -- * Creating a request
      CreateSnapshot (..)
    , mkCreateSnapshot
    -- ** Request lenses
    , csDirectoryId
    , csName

    -- * Destructuring the response
    , CreateSnapshotResponse (..)
    , mkCreateSnapshotResponse
    -- ** Response lenses
    , csrrsSnapshotId
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateSnapshot' operation.
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory of which to take a snapshot.
  , name :: Core.Maybe Types.Name
    -- ^ The descriptive name to apply to the snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshot' value with any optional fields omitted.
mkCreateSnapshot
    :: Types.DirectoryId -- ^ 'directoryId'
    -> CreateSnapshot
mkCreateSnapshot directoryId
  = CreateSnapshot'{directoryId, name = Core.Nothing}

-- | The identifier of the directory of which to take a snapshot.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDirectoryId :: Lens.Lens' CreateSnapshot Types.DirectoryId
csDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE csDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The descriptive name to apply to the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSnapshot (Core.Maybe Types.Name)
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery CreateSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSnapshot where
        toHeaders CreateSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSnapshot where
        toJSON CreateSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = CreateSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSnapshotResponse' Core.<$>
                   (x Core..:? "SnapshotId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'CreateSnapshot' operation.
--
-- /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The identifier of the snapshot that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotResponse' value with any optional fields omitted.
mkCreateSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSnapshotResponse
mkCreateSnapshotResponse responseStatus
  = CreateSnapshotResponse'{snapshotId = Core.Nothing,
                            responseStatus}

-- | The identifier of the snapshot that was created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSnapshotId :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Types.SnapshotId)
csrrsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE csrrsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

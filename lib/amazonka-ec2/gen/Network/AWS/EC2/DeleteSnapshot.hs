{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot.
--
-- When you make periodic snapshots of a volume, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the volume.
-- You cannot delete a snapshot of the root device of an EBS volume used by a registered AMI. You must first de-register the AMI before you can delete the snapshot.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteSnapshot
    (
    -- * Creating a request
      DeleteSnapshot (..)
    , mkDeleteSnapshot
    -- ** Request lenses
    , dshSnapshotId
    , dshDryRun

    -- * Destructuring the response
    , DeleteSnapshotResponse (..)
    , mkDeleteSnapshotResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { snapshotId :: Types.SnapshotId
    -- ^ The ID of the EBS snapshot.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshot' value with any optional fields omitted.
mkDeleteSnapshot
    :: Types.SnapshotId -- ^ 'snapshotId'
    -> DeleteSnapshot
mkDeleteSnapshot snapshotId
  = DeleteSnapshot'{snapshotId, dryRun = Core.Nothing}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dshSnapshotId :: Lens.Lens' DeleteSnapshot Types.SnapshotId
dshSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dshSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dshDryRun :: Lens.Lens' DeleteSnapshot (Core.Maybe Core.Bool)
dshDryRun = Lens.field @"dryRun"
{-# INLINEABLE dshDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteSnapshot where
        toQuery DeleteSnapshot{..}
          = Core.toQueryPair "Action" ("DeleteSnapshot" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotId" snapshotId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSnapshot where
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteSnapshotResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotResponse' value with any optional fields omitted.
mkDeleteSnapshotResponse
    :: DeleteSnapshotResponse
mkDeleteSnapshotResponse = DeleteSnapshotResponse'

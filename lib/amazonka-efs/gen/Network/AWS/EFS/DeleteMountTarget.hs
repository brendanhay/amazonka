{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteMountTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified mount target.
--
-- This operation forcibly breaks any mounts of the file system by using the mount target that is being deleted, which might disrupt instances or applications using those mounts. To avoid applications getting cut off abruptly, you might consider unmounting any mounts of the mount target, if feasible. The operation also deletes the associated network interface. Uncommitted writes might be lost, but breaking a mount target using this operation does not corrupt the file system itself. The file system you created remains. You can mount an EC2 instance in your VPC by using another mount target.
-- This operation requires permissions for the following action on the file system:
--
--     * @elasticfilesystem:DeleteMountTarget@ 
--
--
-- The operation also requires permissions for the following Amazon EC2 action on the mount target's network interface:
--
--     * @ec2:DeleteNetworkInterface@ 
--
--
module Network.AWS.EFS.DeleteMountTarget
    (
    -- * Creating a request
      DeleteMountTarget (..)
    , mkDeleteMountTarget
    -- ** Request lenses
    , dMountTargetId

    -- * Destructuring the response
    , DeleteMountTargetResponse (..)
    , mkDeleteMountTargetResponse
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteMountTarget' smart constructor.
newtype DeleteMountTarget = DeleteMountTarget'
  { mountTargetId :: Types.MountTargetId
    -- ^ The ID of the mount target to delete (String).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMountTarget' value with any optional fields omitted.
mkDeleteMountTarget
    :: Types.MountTargetId -- ^ 'mountTargetId'
    -> DeleteMountTarget
mkDeleteMountTarget mountTargetId
  = DeleteMountTarget'{mountTargetId}

-- | The ID of the mount target to delete (String).
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMountTargetId :: Lens.Lens' DeleteMountTarget Types.MountTargetId
dMountTargetId = Lens.field @"mountTargetId"
{-# INLINEABLE dMountTargetId #-}
{-# DEPRECATED mountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead"  #-}

instance Core.ToQuery DeleteMountTarget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMountTarget where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteMountTarget where
        type Rs DeleteMountTarget = DeleteMountTargetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2015-02-01/mount-targets/" Core.<> Core.toText mountTargetId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteMountTargetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse = DeleteMountTargetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMountTargetResponse' value with any optional fields omitted.
mkDeleteMountTargetResponse
    :: DeleteMountTargetResponse
mkDeleteMountTargetResponse = DeleteMountTargetResponse'

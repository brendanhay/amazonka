{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DAX.DeleteSubnetGroup
    (
    -- * Creating a request
      DeleteSubnetGroup (..)
    , mkDeleteSubnetGroup
    -- ** Request lenses
    , dsgSubnetGroupName

    -- * Destructuring the response
    , DeleteSubnetGroupResponse (..)
    , mkDeleteSubnetGroupResponse
    -- ** Response lenses
    , dsgrrsDeletionMessage
    , dsgrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSubnetGroup' smart constructor.
newtype DeleteSubnetGroup = DeleteSubnetGroup'
  { subnetGroupName :: Core.Text
    -- ^ The name of the subnet group to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnetGroup' value with any optional fields omitted.
mkDeleteSubnetGroup
    :: Core.Text -- ^ 'subnetGroupName'
    -> DeleteSubnetGroup
mkDeleteSubnetGroup subnetGroupName
  = DeleteSubnetGroup'{subnetGroupName}

-- | The name of the subnet group to delete.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupName :: Lens.Lens' DeleteSubnetGroup Core.Text
dsgSubnetGroupName = Lens.field @"subnetGroupName"
{-# INLINEABLE dsgSubnetGroupName #-}
{-# DEPRECATED subnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead"  #-}

instance Core.ToQuery DeleteSubnetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSubnetGroup where
        toHeaders DeleteSubnetGroup{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.DeleteSubnetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSubnetGroup where
        toJSON DeleteSubnetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubnetGroupName" Core..= subnetGroupName)])

instance Core.AWSRequest DeleteSubnetGroup where
        type Rs DeleteSubnetGroup = DeleteSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSubnetGroupResponse' Core.<$>
                   (x Core..:? "DeletionMessage") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { deletionMessage :: Core.Maybe Core.Text
    -- ^ A user-specified message for this action (i.e., a reason for deleting the subnet group).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnetGroupResponse' value with any optional fields omitted.
mkDeleteSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSubnetGroupResponse
mkDeleteSubnetGroupResponse responseStatus
  = DeleteSubnetGroupResponse'{deletionMessage = Core.Nothing,
                               responseStatus}

-- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
--
-- /Note:/ Consider using 'deletionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsDeletionMessage :: Lens.Lens' DeleteSubnetGroupResponse (Core.Maybe Core.Text)
dsgrrsDeletionMessage = Lens.field @"deletionMessage"
{-# INLINEABLE dsgrrsDeletionMessage #-}
{-# DEPRECATED deletionMessage "Use generic-lens or generic-optics with 'deletionMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DeleteSubnetGroupResponse Core.Int
dsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

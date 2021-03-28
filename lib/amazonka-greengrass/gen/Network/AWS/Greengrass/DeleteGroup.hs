{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group.
module Network.AWS.Greengrass.DeleteGroup
    (
    -- * Creating a request
      DeleteGroup (..)
    , mkDeleteGroup
    -- ** Request lenses
    , dgGroupId

    -- * Destructuring the response
    , DeleteGroupResponse (..)
    , mkDeleteGroupResponse
    -- ** Response lenses
    , dgrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroup' value with any optional fields omitted.
mkDeleteGroup
    :: Core.Text -- ^ 'groupId'
    -> DeleteGroup
mkDeleteGroup groupId = DeleteGroup'{groupId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupId :: Lens.Lens' DeleteGroup Core.Text
dgGroupId = Lens.field @"groupId"
{-# INLINEABLE dgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery DeleteGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGroup where
        toHeaders DeleteGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/greengrass/groups/" Core.<> Core.toText groupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
newtype DeleteGroupResponse = DeleteGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupResponse' value with any optional fields omitted.
mkDeleteGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGroupResponse
mkDeleteGroupResponse responseStatus
  = DeleteGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsResponseStatus :: Lens.Lens' DeleteGroupResponse Core.Int
dgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

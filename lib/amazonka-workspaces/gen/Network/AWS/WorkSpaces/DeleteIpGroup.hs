{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteIpGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP access control group.
--
-- You cannot delete an IP access control group that is associated with a directory.
module Network.AWS.WorkSpaces.DeleteIpGroup
    (
    -- * Creating a request
      DeleteIpGroup (..)
    , mkDeleteIpGroup
    -- ** Request lenses
    , digGroupId

    -- * Destructuring the response
    , DeleteIpGroupResponse (..)
    , mkDeleteIpGroupResponse
    -- ** Response lenses
    , digrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDeleteIpGroup' smart constructor.
newtype DeleteIpGroup = DeleteIpGroup'
  { groupId :: Types.IpGroupId
    -- ^ The identifier of the IP access control group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIpGroup' value with any optional fields omitted.
mkDeleteIpGroup
    :: Types.IpGroupId -- ^ 'groupId'
    -> DeleteIpGroup
mkDeleteIpGroup groupId = DeleteIpGroup'{groupId}

-- | The identifier of the IP access control group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digGroupId :: Lens.Lens' DeleteIpGroup Types.IpGroupId
digGroupId = Lens.field @"groupId"
{-# INLINEABLE digGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery DeleteIpGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIpGroup where
        toHeaders DeleteIpGroup{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.DeleteIpGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteIpGroup where
        toJSON DeleteIpGroup{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GroupId" Core..= groupId)])

instance Core.AWSRequest DeleteIpGroup where
        type Rs DeleteIpGroup = DeleteIpGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteIpGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIpGroupResponse' smart constructor.
newtype DeleteIpGroupResponse = DeleteIpGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIpGroupResponse' value with any optional fields omitted.
mkDeleteIpGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteIpGroupResponse
mkDeleteIpGroupResponse responseStatus
  = DeleteIpGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrfrsResponseStatus :: Lens.Lens' DeleteIpGroupResponse Core.Int
digrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE digrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

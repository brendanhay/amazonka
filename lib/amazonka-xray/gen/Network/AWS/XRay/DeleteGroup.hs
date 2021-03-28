{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group resource.
module Network.AWS.XRay.DeleteGroup
    (
    -- * Creating a request
      DeleteGroup (..)
    , mkDeleteGroup
    -- ** Request lenses
    , dgGroupARN
    , dgGroupName

    -- * Destructuring the response
    , DeleteGroupResponse (..)
    , mkDeleteGroupResponse
    -- ** Response lenses
    , dgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { groupARN :: Core.Maybe Types.GroupARN
    -- ^ The ARN of the group that was generated on creation.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The case-sensitive name of the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroup' value with any optional fields omitted.
mkDeleteGroup
    :: DeleteGroup
mkDeleteGroup
  = DeleteGroup'{groupARN = Core.Nothing, groupName = Core.Nothing}

-- | The ARN of the group that was generated on creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupARN :: Lens.Lens' DeleteGroup (Core.Maybe Types.GroupARN)
dgGroupARN = Lens.field @"groupARN"
{-# INLINEABLE dgGroupARN #-}
{-# DEPRECATED groupARN "Use generic-lens or generic-optics with 'groupARN' instead"  #-}

-- | The case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup (Core.Maybe Types.GroupName)
dgGroupName = Lens.field @"groupName"
{-# INLINEABLE dgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery DeleteGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DeleteGroup where
        toJSON DeleteGroup{..}
          = Core.object
              (Core.catMaybes
                 [("GroupARN" Core..=) Core.<$> groupARN,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/DeleteGroup",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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

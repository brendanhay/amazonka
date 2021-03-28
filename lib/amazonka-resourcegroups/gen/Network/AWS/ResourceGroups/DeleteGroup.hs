{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource group. Deleting a resource group does not delete any resources that are members of the group; it only deletes the group structure.
module Network.AWS.ResourceGroups.DeleteGroup
    (
    -- * Creating a request
      DeleteGroup (..)
    , mkDeleteGroup
    -- ** Request lenses
    , dgGroup
    , dgGroupName

    -- * Destructuring the response
    , DeleteGroupResponse (..)
    , mkDeleteGroupResponse
    -- ** Response lenses
    , dgrrsGroup
    , dgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { group :: Core.Maybe Types.GroupString
    -- ^ The name or the ARN of the resource group to delete.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ Don't use this parameter. Use @Group@ instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroup' value with any optional fields omitted.
mkDeleteGroup
    :: DeleteGroup
mkDeleteGroup
  = DeleteGroup'{group = Core.Nothing, groupName = Core.Nothing}

-- | The name or the ARN of the resource group to delete.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroup :: Lens.Lens' DeleteGroup (Core.Maybe Types.GroupString)
dgGroup = Lens.field @"group"
{-# INLINEABLE dgGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | Don't use this parameter. Use @Group@ instead.
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
                 [("Group" Core..=) Core.<$> group,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/delete-group",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { group :: Core.Maybe Types.Group
    -- ^ A full description of the deleted resource group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupResponse' value with any optional fields omitted.
mkDeleteGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGroupResponse
mkDeleteGroupResponse responseStatus
  = DeleteGroupResponse'{group = Core.Nothing, responseStatus}

-- | A full description of the deleted resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsGroup :: Lens.Lens' DeleteGroupResponse (Core.Maybe Types.Group)
dgrrsGroup = Lens.field @"group"
{-# INLINEABLE dgrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsResponseStatus :: Lens.Lens' DeleteGroupResponse Core.Int
dgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

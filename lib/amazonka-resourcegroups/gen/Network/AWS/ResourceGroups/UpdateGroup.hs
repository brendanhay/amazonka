{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for an existing group. You cannot update the name of a resource group.
module Network.AWS.ResourceGroups.UpdateGroup
    (
    -- * Creating a request
      UpdateGroup (..)
    , mkUpdateGroup
    -- ** Request lenses
    , ugDescription
    , ugGroup
    , ugGroupName

    -- * Destructuring the response
    , UpdateGroupResponse (..)
    , mkUpdateGroupResponse
    -- ** Response lenses
    , ugrrsGroup
    , ugrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { description :: Core.Maybe Types.Description
    -- ^ The new description that you want to update the resource group with. Descriptions can contain letters, numbers, hyphens, underscores, periods, and spaces.
  , group :: Core.Maybe Types.GroupString
    -- ^ The name or the ARN of the resource group to modify.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ Don't use this parameter. Use @Group@ instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup
    :: UpdateGroup
mkUpdateGroup
  = UpdateGroup'{description = Core.Nothing, group = Core.Nothing,
                 groupName = Core.Nothing}

-- | The new description that you want to update the resource group with. Descriptions can contain letters, numbers, hyphens, underscores, periods, and spaces.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGroup (Core.Maybe Types.Description)
ugDescription = Lens.field @"description"
{-# INLINEABLE ugDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name or the ARN of the resource group to modify.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroup :: Lens.Lens' UpdateGroup (Core.Maybe Types.GroupString)
ugGroup = Lens.field @"group"
{-# INLINEABLE ugGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup (Core.Maybe Types.GroupName)
ugGroupName = Lens.field @"groupName"
{-# INLINEABLE ugGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery UpdateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateGroup where
        toJSON UpdateGroup{..}
          = Core.object
              (Core.catMaybes
                 [("Description" Core..=) Core.<$> description,
                  ("Group" Core..=) Core.<$> group,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/update-group",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { group :: Core.Maybe Types.Group
    -- ^ The update description of the resource group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupResponse' value with any optional fields omitted.
mkUpdateGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGroupResponse
mkUpdateGroupResponse responseStatus
  = UpdateGroupResponse'{group = Core.Nothing, responseStatus}

-- | The update description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsGroup :: Lens.Lens' UpdateGroupResponse (Core.Maybe Types.Group)
ugrrsGroup = Lens.field @"group"
{-# INLINEABLE ugrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGroupResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified resource group.
module Network.AWS.ResourceGroups.GetGroup
    (
    -- * Creating a request
      GetGroup (..)
    , mkGetGroup
    -- ** Request lenses
    , ggGroup
    , ggGroupName

    -- * Destructuring the response
    , GetGroupResponse (..)
    , mkGetGroupResponse
    -- ** Response lenses
    , ggrrsGroup
    , ggrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { group :: Core.Maybe Types.GroupString
    -- ^ The name or the ARN of the resource group to retrieve.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ Don't use this parameter. Use @Group@ instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup
    :: GetGroup
mkGetGroup
  = GetGroup'{group = Core.Nothing, groupName = Core.Nothing}

-- | The name or the ARN of the resource group to retrieve.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroup :: Lens.Lens' GetGroup (Core.Maybe Types.GroupString)
ggGroup = Lens.field @"group"
{-# INLINEABLE ggGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup (Core.Maybe Types.GroupName)
ggGroupName = Lens.field @"groupName"
{-# INLINEABLE ggGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery GetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetGroup where
        toJSON GetGroup{..}
          = Core.object
              (Core.catMaybes
                 [("Group" Core..=) Core.<$> group,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/get-group",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { group :: Core.Maybe Types.Group
    -- ^ A full description of the resource group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupResponse' value with any optional fields omitted.
mkGetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGroupResponse
mkGetGroupResponse responseStatus
  = GetGroupResponse'{group = Core.Nothing, responseStatus}

-- | A full description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroup :: Lens.Lens' GetGroupResponse (Core.Maybe Types.Group)
ggrrsGroup = Lens.field @"group"
{-# INLINEABLE ggrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

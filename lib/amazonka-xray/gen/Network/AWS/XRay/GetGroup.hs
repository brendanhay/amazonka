{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves group resource details.
module Network.AWS.XRay.GetGroup
    (
    -- * Creating a request
      GetGroup (..)
    , mkGetGroup
    -- ** Request lenses
    , ggGroupARN
    , ggGroupName

    -- * Destructuring the response
    , GetGroupResponse (..)
    , mkGetGroupResponse
    -- ** Response lenses
    , grsGroup
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { groupARN :: Core.Maybe Types.GroupARN
    -- ^ The ARN of the group that was generated on creation.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The case-sensitive name of the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup
    :: GetGroup
mkGetGroup
  = GetGroup'{groupARN = Core.Nothing, groupName = Core.Nothing}

-- | The ARN of the group that was generated on creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupARN :: Lens.Lens' GetGroup (Core.Maybe Types.GroupARN)
ggGroupARN = Lens.field @"groupARN"
{-# INLINEABLE ggGroupARN #-}
{-# DEPRECATED groupARN "Use generic-lens or generic-optics with 'groupARN' instead"  #-}

-- | The case-sensitive name of the group.
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
                 [("GroupARN" Core..=) Core.<$> groupARN,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/GetGroup",
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
    -- ^ The group that was requested. Contains the name of the group, the ARN of the group, the filter expression, and the insight configuration assigned to the group.
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

-- | The group that was requested. Contains the name of the group, the ARN of the group, the filter expression, and the insight configuration assigned to the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsGroup :: Lens.Lens' GetGroupResponse (Core.Maybe Types.Group)
grsGroup = Lens.field @"group"
{-# INLINEABLE grsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

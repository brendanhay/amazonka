{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group resource.
module Network.AWS.XRay.UpdateGroup
    (
    -- * Creating a request
      UpdateGroup (..)
    , mkUpdateGroup
    -- ** Request lenses
    , ugFilterExpression
    , ugGroupARN
    , ugGroupName
    , ugInsightsConfiguration

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
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { filterExpression :: Core.Maybe Types.FilterExpression
    -- ^ The updated filter expression defining criteria by which to group traces.
  , groupARN :: Core.Maybe Types.GroupARN
    -- ^ The ARN that was generated upon creation.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The case-sensitive name of the group.
  , insightsConfiguration :: Core.Maybe Types.InsightsConfiguration
    -- ^ The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the group. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup
    :: UpdateGroup
mkUpdateGroup
  = UpdateGroup'{filterExpression = Core.Nothing,
                 groupARN = Core.Nothing, groupName = Core.Nothing,
                 insightsConfiguration = Core.Nothing}

-- | The updated filter expression defining criteria by which to group traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugFilterExpression :: Lens.Lens' UpdateGroup (Core.Maybe Types.FilterExpression)
ugFilterExpression = Lens.field @"filterExpression"
{-# INLINEABLE ugFilterExpression #-}
{-# DEPRECATED filterExpression "Use generic-lens or generic-optics with 'filterExpression' instead"  #-}

-- | The ARN that was generated upon creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupARN :: Lens.Lens' UpdateGroup (Core.Maybe Types.GroupARN)
ugGroupARN = Lens.field @"groupARN"
{-# INLINEABLE ugGroupARN #-}
{-# DEPRECATED groupARN "Use generic-lens or generic-optics with 'groupARN' instead"  #-}

-- | The case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup (Core.Maybe Types.GroupName)
ugGroupName = Lens.field @"groupName"
{-# INLINEABLE ugGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the group. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugInsightsConfiguration :: Lens.Lens' UpdateGroup (Core.Maybe Types.InsightsConfiguration)
ugInsightsConfiguration = Lens.field @"insightsConfiguration"
{-# INLINEABLE ugInsightsConfiguration #-}
{-# DEPRECATED insightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead"  #-}

instance Core.ToQuery UpdateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateGroup where
        toJSON UpdateGroup{..}
          = Core.object
              (Core.catMaybes
                 [("FilterExpression" Core..=) Core.<$> filterExpression,
                  ("GroupARN" Core..=) Core.<$> groupARN,
                  ("GroupName" Core..=) Core.<$> groupName,
                  ("InsightsConfiguration" Core..=) Core.<$> insightsConfiguration])

instance Core.AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/UpdateGroup",
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
    -- ^ The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, the updated filter expression, and the updated insight configuration assigned to the group.
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

-- | The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, the updated filter expression, and the updated insight configuration assigned to the group.
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

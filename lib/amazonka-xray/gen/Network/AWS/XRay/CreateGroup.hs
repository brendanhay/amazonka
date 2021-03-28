{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group resource with a name and a filter expression. 
module Network.AWS.XRay.CreateGroup
    (
    -- * Creating a request
      CreateGroup (..)
    , mkCreateGroup
    -- ** Request lenses
    , cgGroupName
    , cgFilterExpression
    , cgInsightsConfiguration
    , cgTags

    -- * Destructuring the response
    , CreateGroupResponse (..)
    , mkCreateGroupResponse
    -- ** Response lenses
    , cgrrsGroup
    , cgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { groupName :: Types.GroupName
    -- ^ The case-sensitive name of the new group. Default is a reserved name and names must be unique.
  , filterExpression :: Core.Maybe Types.FilterExpression
    -- ^ The filter expression defining criteria by which to group traces.
  , insightsConfiguration :: Core.Maybe Types.InsightsConfiguration
    -- ^ The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroup' value with any optional fields omitted.
mkCreateGroup
    :: Types.GroupName -- ^ 'groupName'
    -> CreateGroup
mkCreateGroup groupName
  = CreateGroup'{groupName, filterExpression = Core.Nothing,
                 insightsConfiguration = Core.Nothing, tags = Core.Nothing}

-- | The case-sensitive name of the new group. Default is a reserved name and names must be unique.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGroupName :: Lens.Lens' CreateGroup Types.GroupName
cgGroupName = Lens.field @"groupName"
{-# INLINEABLE cgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The filter expression defining criteria by which to group traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgFilterExpression :: Lens.Lens' CreateGroup (Core.Maybe Types.FilterExpression)
cgFilterExpression = Lens.field @"filterExpression"
{-# INLINEABLE cgFilterExpression #-}
{-# DEPRECATED filterExpression "Use generic-lens or generic-optics with 'filterExpression' instead"  #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgInsightsConfiguration :: Lens.Lens' CreateGroup (Core.Maybe Types.InsightsConfiguration)
cgInsightsConfiguration = Lens.field @"insightsConfiguration"
{-# INLINEABLE cgInsightsConfiguration #-}
{-# DEPRECATED insightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead"  #-}

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CreateGroup (Core.Maybe [Types.Tag])
cgTags = Lens.field @"tags"
{-# INLINEABLE cgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateGroup where
        toJSON CreateGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupName" Core..= groupName),
                  ("FilterExpression" Core..=) Core.<$> filterExpression,
                  ("InsightsConfiguration" Core..=) Core.<$> insightsConfiguration,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/CreateGroup",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { group :: Core.Maybe Types.Group
    -- ^ The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupResponse' value with any optional fields omitted.
mkCreateGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGroupResponse
mkCreateGroupResponse responseStatus
  = CreateGroupResponse'{group = Core.Nothing, responseStatus}

-- | The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGroup :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.Group)
cgrrsGroup = Lens.field @"group"
{-# INLINEABLE cgrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResponseStatus :: Lens.Lens' CreateGroupResponse Core.Int
cgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

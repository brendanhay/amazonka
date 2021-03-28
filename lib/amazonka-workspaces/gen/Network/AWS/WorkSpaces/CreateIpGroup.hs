{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateIpGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IP access control group.
--
-- An IP access control group provides you with the ability to control the IP addresses from which users are allowed to access their WorkSpaces. To specify the CIDR address ranges, add rules to your IP access control group and then associate the group with your directory. You can add rules when you create the group or at any time using 'AuthorizeIpRules' .
-- There is a default IP access control group associated with your directory. If you don't associate an IP access control group with your directory, the default group is used. The default group includes a default rule that allows users to access their WorkSpaces from anywhere. You cannot modify the default IP access control group for your directory.
module Network.AWS.WorkSpaces.CreateIpGroup
    (
    -- * Creating a request
      CreateIpGroup (..)
    , mkCreateIpGroup
    -- ** Request lenses
    , cigGroupName
    , cigGroupDesc
    , cigTags
    , cigUserRules

    -- * Destructuring the response
    , CreateIpGroupResponse (..)
    , mkCreateIpGroupResponse
    -- ** Response lenses
    , cigrrsGroupId
    , cigrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkCreateIpGroup' smart constructor.
data CreateIpGroup = CreateIpGroup'
  { groupName :: Types.IpGroupName
    -- ^ The name of the group.
  , groupDesc :: Core.Maybe Types.IpGroupDesc
    -- ^ The description of the group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags. Each WorkSpaces resource can have a maximum of 50 tags.
  , userRules :: Core.Maybe [Types.IpRuleItem]
    -- ^ The rules to add to the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIpGroup' value with any optional fields omitted.
mkCreateIpGroup
    :: Types.IpGroupName -- ^ 'groupName'
    -> CreateIpGroup
mkCreateIpGroup groupName
  = CreateIpGroup'{groupName, groupDesc = Core.Nothing,
                   tags = Core.Nothing, userRules = Core.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigGroupName :: Lens.Lens' CreateIpGroup Types.IpGroupName
cigGroupName = Lens.field @"groupName"
{-# INLINEABLE cigGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The description of the group.
--
-- /Note:/ Consider using 'groupDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigGroupDesc :: Lens.Lens' CreateIpGroup (Core.Maybe Types.IpGroupDesc)
cigGroupDesc = Lens.field @"groupDesc"
{-# INLINEABLE cigGroupDesc #-}
{-# DEPRECATED groupDesc "Use generic-lens or generic-optics with 'groupDesc' instead"  #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigTags :: Lens.Lens' CreateIpGroup (Core.Maybe [Types.Tag])
cigTags = Lens.field @"tags"
{-# INLINEABLE cigTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The rules to add to the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigUserRules :: Lens.Lens' CreateIpGroup (Core.Maybe [Types.IpRuleItem])
cigUserRules = Lens.field @"userRules"
{-# INLINEABLE cigUserRules #-}
{-# DEPRECATED userRules "Use generic-lens or generic-optics with 'userRules' instead"  #-}

instance Core.ToQuery CreateIpGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateIpGroup where
        toHeaders CreateIpGroup{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.CreateIpGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateIpGroup where
        toJSON CreateIpGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupName" Core..= groupName),
                  ("GroupDesc" Core..=) Core.<$> groupDesc,
                  ("Tags" Core..=) Core.<$> tags,
                  ("UserRules" Core..=) Core.<$> userRules])

instance Core.AWSRequest CreateIpGroup where
        type Rs CreateIpGroup = CreateIpGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateIpGroupResponse' Core.<$>
                   (x Core..:? "GroupId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateIpGroupResponse' smart constructor.
data CreateIpGroupResponse = CreateIpGroupResponse'
  { groupId :: Core.Maybe Types.GroupId
    -- ^ The identifier of the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIpGroupResponse' value with any optional fields omitted.
mkCreateIpGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateIpGroupResponse
mkCreateIpGroupResponse responseStatus
  = CreateIpGroupResponse'{groupId = Core.Nothing, responseStatus}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsGroupId :: Lens.Lens' CreateIpGroupResponse (Core.Maybe Types.GroupId)
cigrrsGroupId = Lens.field @"groupId"
{-# INLINEABLE cigrrsGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsResponseStatus :: Lens.Lens' CreateIpGroupResponse Core.Int
cigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

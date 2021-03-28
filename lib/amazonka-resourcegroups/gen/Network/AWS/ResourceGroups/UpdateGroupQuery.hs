{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UpdateGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource query of a group.
module Network.AWS.ResourceGroups.UpdateGroupQuery
    (
    -- * Creating a request
      UpdateGroupQuery (..)
    , mkUpdateGroupQuery
    -- ** Request lenses
    , ugqResourceQuery
    , ugqGroup
    , ugqGroupName

    -- * Destructuring the response
    , UpdateGroupQueryResponse (..)
    , mkUpdateGroupQueryResponse
    -- ** Response lenses
    , ugqrrsGroupQuery
    , ugqrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroupQuery' smart constructor.
data UpdateGroupQuery = UpdateGroupQuery'
  { resourceQuery :: Types.ResourceQuery
    -- ^ The resource query to determine which AWS resources are members of this resource group.
  , group :: Core.Maybe Types.GroupString
    -- ^ The name or the ARN of the resource group to query.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ Don't use this parameter. Use @Group@ instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupQuery' value with any optional fields omitted.
mkUpdateGroupQuery
    :: Types.ResourceQuery -- ^ 'resourceQuery'
    -> UpdateGroupQuery
mkUpdateGroupQuery resourceQuery
  = UpdateGroupQuery'{resourceQuery, group = Core.Nothing,
                      groupName = Core.Nothing}

-- | The resource query to determine which AWS resources are members of this resource group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqResourceQuery :: Lens.Lens' UpdateGroupQuery Types.ResourceQuery
ugqResourceQuery = Lens.field @"resourceQuery"
{-# INLINEABLE ugqResourceQuery #-}
{-# DEPRECATED resourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead"  #-}

-- | The name or the ARN of the resource group to query.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqGroup :: Lens.Lens' UpdateGroupQuery (Core.Maybe Types.GroupString)
ugqGroup = Lens.field @"group"
{-# INLINEABLE ugqGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqGroupName :: Lens.Lens' UpdateGroupQuery (Core.Maybe Types.GroupName)
ugqGroupName = Lens.field @"groupName"
{-# INLINEABLE ugqGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery UpdateGroupQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGroupQuery where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateGroupQuery where
        toJSON UpdateGroupQuery{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceQuery" Core..= resourceQuery),
                  ("Group" Core..=) Core.<$> group,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest UpdateGroupQuery where
        type Rs UpdateGroupQuery = UpdateGroupQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/update-group-query",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGroupQueryResponse' Core.<$>
                   (x Core..:? "GroupQuery") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGroupQueryResponse' smart constructor.
data UpdateGroupQueryResponse = UpdateGroupQueryResponse'
  { groupQuery :: Core.Maybe Types.GroupQuery
    -- ^ The updated resource query associated with the resource group after the update.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupQueryResponse' value with any optional fields omitted.
mkUpdateGroupQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGroupQueryResponse
mkUpdateGroupQueryResponse responseStatus
  = UpdateGroupQueryResponse'{groupQuery = Core.Nothing,
                              responseStatus}

-- | The updated resource query associated with the resource group after the update.
--
-- /Note:/ Consider using 'groupQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqrrsGroupQuery :: Lens.Lens' UpdateGroupQueryResponse (Core.Maybe Types.GroupQuery)
ugqrrsGroupQuery = Lens.field @"groupQuery"
{-# INLINEABLE ugqrrsGroupQuery #-}
{-# DEPRECATED groupQuery "Use generic-lens or generic-optics with 'groupQuery' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqrrsResponseStatus :: Lens.Lens' UpdateGroupQueryResponse Core.Int
ugqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

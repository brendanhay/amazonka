{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource query associated with the specified resource group.
module Network.AWS.ResourceGroups.GetGroupQuery
    (
    -- * Creating a request
      GetGroupQuery (..)
    , mkGetGroupQuery
    -- ** Request lenses
    , ggqGroup
    , ggqGroupName

    -- * Destructuring the response
    , GetGroupQueryResponse (..)
    , mkGetGroupQueryResponse
    -- ** Response lenses
    , ggqrrsGroupQuery
    , ggqrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupQuery' smart constructor.
data GetGroupQuery = GetGroupQuery'
  { group :: Core.Maybe Types.GroupString
    -- ^ The name or the ARN of the resource group to query.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ Don't use this parameter. Use @Group@ instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupQuery' value with any optional fields omitted.
mkGetGroupQuery
    :: GetGroupQuery
mkGetGroupQuery
  = GetGroupQuery'{group = Core.Nothing, groupName = Core.Nothing}

-- | The name or the ARN of the resource group to query.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqGroup :: Lens.Lens' GetGroupQuery (Core.Maybe Types.GroupString)
ggqGroup = Lens.field @"group"
{-# INLINEABLE ggqGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqGroupName :: Lens.Lens' GetGroupQuery (Core.Maybe Types.GroupName)
ggqGroupName = Lens.field @"groupName"
{-# INLINEABLE ggqGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery GetGroupQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGroupQuery where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetGroupQuery where
        toJSON GetGroupQuery{..}
          = Core.object
              (Core.catMaybes
                 [("Group" Core..=) Core.<$> group,
                  ("GroupName" Core..=) Core.<$> groupName])

instance Core.AWSRequest GetGroupQuery where
        type Rs GetGroupQuery = GetGroupQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/get-group-query",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGroupQueryResponse' Core.<$>
                   (x Core..:? "GroupQuery") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGroupQueryResponse' smart constructor.
data GetGroupQueryResponse = GetGroupQueryResponse'
  { groupQuery :: Core.Maybe Types.GroupQuery
    -- ^ The resource query associated with the specified group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupQueryResponse' value with any optional fields omitted.
mkGetGroupQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGroupQueryResponse
mkGetGroupQueryResponse responseStatus
  = GetGroupQueryResponse'{groupQuery = Core.Nothing, responseStatus}

-- | The resource query associated with the specified group.
--
-- /Note:/ Consider using 'groupQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqrrsGroupQuery :: Lens.Lens' GetGroupQueryResponse (Core.Maybe Types.GroupQuery)
ggqrrsGroupQuery = Lens.field @"groupQuery"
{-# INLINEABLE ggqrrsGroupQuery #-}
{-# DEPRECATED groupQuery "Use generic-lens or generic-optics with 'groupQuery' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqrrsResponseStatus :: Lens.Lens' GetGroupQueryResponse Core.Int
ggqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

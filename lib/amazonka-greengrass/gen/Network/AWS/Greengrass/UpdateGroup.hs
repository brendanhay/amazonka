{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group.
module Network.AWS.Greengrass.UpdateGroup
    (
    -- * Creating a request
      UpdateGroup (..)
    , mkUpdateGroup
    -- ** Request lenses
    , ugGroupId
    , ugName

    -- * Destructuring the response
    , UpdateGroupResponse (..)
    , mkUpdateGroupResponse
    -- ** Response lenses
    , ugrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup
    :: Core.Text -- ^ 'groupId'
    -> UpdateGroup
mkUpdateGroup groupId = UpdateGroup'{groupId, name = Core.Nothing}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupId :: Lens.Lens' UpdateGroup Core.Text
ugGroupId = Lens.field @"groupId"
{-# INLINEABLE ugGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugName :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
ugName = Lens.field @"name"
{-# INLINEABLE ugName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGroup where
        toHeaders UpdateGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGroup where
        toJSON UpdateGroup{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/greengrass/groups/" Core.<> Core.toText groupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
newtype UpdateGroupResponse = UpdateGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupResponse' value with any optional fields omitted.
mkUpdateGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGroupResponse
mkUpdateGroupResponse responseStatus
  = UpdateGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGroupResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

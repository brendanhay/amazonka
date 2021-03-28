{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dynamic thing group.
module Network.AWS.IoT.DeleteDynamicThingGroup
    (
    -- * Creating a request
      DeleteDynamicThingGroup (..)
    , mkDeleteDynamicThingGroup
    -- ** Request lenses
    , ddtgThingGroupName
    , ddtgExpectedVersion

    -- * Destructuring the response
    , DeleteDynamicThingGroupResponse (..)
    , mkDeleteDynamicThingGroupResponse
    -- ** Response lenses
    , ddtgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDynamicThingGroup' smart constructor.
data DeleteDynamicThingGroup = DeleteDynamicThingGroup'
  { thingGroupName :: Types.ThingGroupName
    -- ^ The name of the dynamic thing group to delete.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the dynamic thing group to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDynamicThingGroup' value with any optional fields omitted.
mkDeleteDynamicThingGroup
    :: Types.ThingGroupName -- ^ 'thingGroupName'
    -> DeleteDynamicThingGroup
mkDeleteDynamicThingGroup thingGroupName
  = DeleteDynamicThingGroup'{thingGroupName,
                             expectedVersion = Core.Nothing}

-- | The name of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgThingGroupName :: Lens.Lens' DeleteDynamicThingGroup Types.ThingGroupName
ddtgThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE ddtgThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The expected version of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgExpectedVersion :: Lens.Lens' DeleteDynamicThingGroup (Core.Maybe Core.Integer)
ddtgExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE ddtgExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

instance Core.ToQuery DeleteDynamicThingGroup where
        toQuery DeleteDynamicThingGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "expectedVersion")
              expectedVersion

instance Core.ToHeaders DeleteDynamicThingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDynamicThingGroup where
        type Rs DeleteDynamicThingGroup = DeleteDynamicThingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/dynamic-thing-groups/" Core.<> Core.toText thingGroupName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDynamicThingGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDynamicThingGroupResponse' smart constructor.
newtype DeleteDynamicThingGroupResponse = DeleteDynamicThingGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDynamicThingGroupResponse' value with any optional fields omitted.
mkDeleteDynamicThingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDynamicThingGroupResponse
mkDeleteDynamicThingGroupResponse responseStatus
  = DeleteDynamicThingGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgrrsResponseStatus :: Lens.Lens' DeleteDynamicThingGroupResponse Core.Int
ddtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

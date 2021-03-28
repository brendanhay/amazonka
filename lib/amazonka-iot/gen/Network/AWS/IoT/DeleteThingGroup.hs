{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a thing group.
module Network.AWS.IoT.DeleteThingGroup
    (
    -- * Creating a request
      DeleteThingGroup (..)
    , mkDeleteThingGroup
    -- ** Request lenses
    , dThingGroupName
    , dExpectedVersion

    -- * Destructuring the response
    , DeleteThingGroupResponse (..)
    , mkDeleteThingGroupResponse
    -- ** Response lenses
    , dtgrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { thingGroupName :: Types.ThingGroupName
    -- ^ The name of the thing group to delete.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the thing group to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingGroup' value with any optional fields omitted.
mkDeleteThingGroup
    :: Types.ThingGroupName -- ^ 'thingGroupName'
    -> DeleteThingGroup
mkDeleteThingGroup thingGroupName
  = DeleteThingGroup'{thingGroupName, expectedVersion = Core.Nothing}

-- | The name of the thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingGroupName :: Lens.Lens' DeleteThingGroup Types.ThingGroupName
dThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE dThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The expected version of the thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpectedVersion :: Lens.Lens' DeleteThingGroup (Core.Maybe Core.Integer)
dExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE dExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

instance Core.ToQuery DeleteThingGroup where
        toQuery DeleteThingGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "expectedVersion")
              expectedVersion

instance Core.ToHeaders DeleteThingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteThingGroup where
        type Rs DeleteThingGroup = DeleteThingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/thing-groups/" Core.<> Core.toText thingGroupName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteThingGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteThingGroupResponse' smart constructor.
newtype DeleteThingGroupResponse = DeleteThingGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingGroupResponse' value with any optional fields omitted.
mkDeleteThingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteThingGroupResponse
mkDeleteThingGroupResponse responseStatus
  = DeleteThingGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrfrsResponseStatus :: Lens.Lens' DeleteThingGroupResponse Core.Int
dtgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

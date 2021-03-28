{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the specified thing from the specified group.
--
-- You must specify either a @thingGroupArn@ or a @thingGroupName@ to identify the thing group and either a @thingArn@ or a @thingName@ to identify the thing to remove from the thing group. 
module Network.AWS.IoT.RemoveThingFromThingGroup
    (
    -- * Creating a request
      RemoveThingFromThingGroup (..)
    , mkRemoveThingFromThingGroup
    -- ** Request lenses
    , rtftgThingArn
    , rtftgThingGroupArn
    , rtftgThingGroupName
    , rtftgThingName

    -- * Destructuring the response
    , RemoveThingFromThingGroupResponse (..)
    , mkRemoveThingFromThingGroupResponse
    -- ** Response lenses
    , rtftgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { thingArn :: Core.Maybe Types.ThingArn
    -- ^ The ARN of the thing to remove from the group.
  , thingGroupArn :: Core.Maybe Types.ThingGroupArn
    -- ^ The group ARN.
  , thingGroupName :: Core.Maybe Types.ThingGroupName
    -- ^ The group name.
  , thingName :: Core.Maybe Types.ThingName
    -- ^ The name of the thing to remove from the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveThingFromThingGroup' value with any optional fields omitted.
mkRemoveThingFromThingGroup
    :: RemoveThingFromThingGroup
mkRemoveThingFromThingGroup
  = RemoveThingFromThingGroup'{thingArn = Core.Nothing,
                               thingGroupArn = Core.Nothing, thingGroupName = Core.Nothing,
                               thingName = Core.Nothing}

-- | The ARN of the thing to remove from the group.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingArn :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Types.ThingArn)
rtftgThingArn = Lens.field @"thingArn"
{-# INLINEABLE rtftgThingArn #-}
{-# DEPRECATED thingArn "Use generic-lens or generic-optics with 'thingArn' instead"  #-}

-- | The group ARN.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingGroupArn :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Types.ThingGroupArn)
rtftgThingGroupArn = Lens.field @"thingGroupArn"
{-# INLINEABLE rtftgThingGroupArn #-}
{-# DEPRECATED thingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead"  #-}

-- | The group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingGroupName :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Types.ThingGroupName)
rtftgThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE rtftgThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The name of the thing to remove from the group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingName :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Types.ThingName)
rtftgThingName = Lens.field @"thingName"
{-# INLINEABLE rtftgThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

instance Core.ToQuery RemoveThingFromThingGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveThingFromThingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RemoveThingFromThingGroup where
        toJSON RemoveThingFromThingGroup{..}
          = Core.object
              (Core.catMaybes
                 [("thingArn" Core..=) Core.<$> thingArn,
                  ("thingGroupArn" Core..=) Core.<$> thingGroupArn,
                  ("thingGroupName" Core..=) Core.<$> thingGroupName,
                  ("thingName" Core..=) Core.<$> thingName])

instance Core.AWSRequest RemoveThingFromThingGroup where
        type Rs RemoveThingFromThingGroup =
             RemoveThingFromThingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/thing-groups/removeThingFromThingGroup",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RemoveThingFromThingGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveThingFromThingGroupResponse' smart constructor.
newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveThingFromThingGroupResponse' value with any optional fields omitted.
mkRemoveThingFromThingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveThingFromThingGroupResponse
mkRemoveThingFromThingGroupResponse responseStatus
  = RemoveThingFromThingGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgrrsResponseStatus :: Lens.Lens' RemoveThingFromThingGroupResponse Core.Int
rtftgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtftgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

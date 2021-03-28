{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceEventStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the start time for a scheduled Amazon EC2 instance event.
module Network.AWS.EC2.ModifyInstanceEventStartTime
    (
    -- * Creating a request
      ModifyInstanceEventStartTime (..)
    , mkModifyInstanceEventStartTime
    -- ** Request lenses
    , miestInstanceId
    , miestInstanceEventId
    , miestNotBefore
    , miestDryRun

    -- * Destructuring the response
    , ModifyInstanceEventStartTimeResponse (..)
    , mkModifyInstanceEventStartTimeResponse
    -- ** Response lenses
    , miestrrsEvent
    , miestrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceEventStartTime' smart constructor.
data ModifyInstanceEventStartTime = ModifyInstanceEventStartTime'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance with the scheduled event.
  , instanceEventId :: Core.Text
    -- ^ The ID of the event whose date and time you are modifying.
  , notBefore :: Core.UTCTime
    -- ^ The new date and time when the event will take place.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyInstanceEventStartTime' value with any optional fields omitted.
mkModifyInstanceEventStartTime
    :: Types.InstanceId -- ^ 'instanceId'
    -> Core.Text -- ^ 'instanceEventId'
    -> Core.UTCTime -- ^ 'notBefore'
    -> ModifyInstanceEventStartTime
mkModifyInstanceEventStartTime instanceId instanceEventId notBefore
  = ModifyInstanceEventStartTime'{instanceId, instanceEventId,
                                  notBefore, dryRun = Core.Nothing}

-- | The ID of the instance with the scheduled event.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestInstanceId :: Lens.Lens' ModifyInstanceEventStartTime Types.InstanceId
miestInstanceId = Lens.field @"instanceId"
{-# INLINEABLE miestInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The ID of the event whose date and time you are modifying.
--
-- /Note:/ Consider using 'instanceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestInstanceEventId :: Lens.Lens' ModifyInstanceEventStartTime Core.Text
miestInstanceEventId = Lens.field @"instanceEventId"
{-# INLINEABLE miestInstanceEventId #-}
{-# DEPRECATED instanceEventId "Use generic-lens or generic-optics with 'instanceEventId' instead"  #-}

-- | The new date and time when the event will take place.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestNotBefore :: Lens.Lens' ModifyInstanceEventStartTime Core.UTCTime
miestNotBefore = Lens.field @"notBefore"
{-# INLINEABLE miestNotBefore #-}
{-# DEPRECATED notBefore "Use generic-lens or generic-optics with 'notBefore' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestDryRun :: Lens.Lens' ModifyInstanceEventStartTime (Core.Maybe Core.Bool)
miestDryRun = Lens.field @"dryRun"
{-# INLINEABLE miestDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyInstanceEventStartTime where
        toQuery ModifyInstanceEventStartTime{..}
          = Core.toQueryPair "Action"
              ("ModifyInstanceEventStartTime" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "InstanceEventId" instanceEventId
              Core.<> Core.toQueryPair "NotBefore" notBefore
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyInstanceEventStartTime where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyInstanceEventStartTime where
        type Rs ModifyInstanceEventStartTime =
             ModifyInstanceEventStartTimeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyInstanceEventStartTimeResponse' Core.<$>
                   (x Core..@? "event") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceEventStartTimeResponse' smart constructor.
data ModifyInstanceEventStartTimeResponse = ModifyInstanceEventStartTimeResponse'
  { event :: Core.Maybe Types.InstanceStatusEvent
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyInstanceEventStartTimeResponse' value with any optional fields omitted.
mkModifyInstanceEventStartTimeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyInstanceEventStartTimeResponse
mkModifyInstanceEventStartTimeResponse responseStatus
  = ModifyInstanceEventStartTimeResponse'{event = Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestrrsEvent :: Lens.Lens' ModifyInstanceEventStartTimeResponse (Core.Maybe Types.InstanceStatusEvent)
miestrrsEvent = Lens.field @"event"
{-# INLINEABLE miestrrsEvent #-}
{-# DEPRECATED event "Use generic-lens or generic-optics with 'event' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestrrsResponseStatus :: Lens.Lens' ModifyInstanceEventStartTimeResponse Core.Int
miestrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE miestrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

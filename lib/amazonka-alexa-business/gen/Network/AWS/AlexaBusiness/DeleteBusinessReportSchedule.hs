{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recurring report delivery schedule with the specified schedule ARN.
module Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
    (
    -- * Creating a request
      DeleteBusinessReportSchedule (..)
    , mkDeleteBusinessReportSchedule
    -- ** Request lenses
    , dbrsScheduleArn

    -- * Destructuring the response
    , DeleteBusinessReportScheduleResponse (..)
    , mkDeleteBusinessReportScheduleResponse
    -- ** Response lenses
    , dbrsrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBusinessReportSchedule' smart constructor.
newtype DeleteBusinessReportSchedule = DeleteBusinessReportSchedule'
  { scheduleArn :: Types.Arn
    -- ^ The ARN of the business report schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBusinessReportSchedule' value with any optional fields omitted.
mkDeleteBusinessReportSchedule
    :: Types.Arn -- ^ 'scheduleArn'
    -> DeleteBusinessReportSchedule
mkDeleteBusinessReportSchedule scheduleArn
  = DeleteBusinessReportSchedule'{scheduleArn}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsScheduleArn :: Lens.Lens' DeleteBusinessReportSchedule Types.Arn
dbrsScheduleArn = Lens.field @"scheduleArn"
{-# INLINEABLE dbrsScheduleArn #-}
{-# DEPRECATED scheduleArn "Use generic-lens or generic-optics with 'scheduleArn' instead"  #-}

instance Core.ToQuery DeleteBusinessReportSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBusinessReportSchedule where
        toHeaders DeleteBusinessReportSchedule{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.DeleteBusinessReportSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBusinessReportSchedule where
        toJSON DeleteBusinessReportSchedule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ScheduleArn" Core..= scheduleArn)])

instance Core.AWSRequest DeleteBusinessReportSchedule where
        type Rs DeleteBusinessReportSchedule =
             DeleteBusinessReportScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteBusinessReportScheduleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBusinessReportScheduleResponse' smart constructor.
newtype DeleteBusinessReportScheduleResponse = DeleteBusinessReportScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBusinessReportScheduleResponse' value with any optional fields omitted.
mkDeleteBusinessReportScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBusinessReportScheduleResponse
mkDeleteBusinessReportScheduleResponse responseStatus
  = DeleteBusinessReportScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsrrsResponseStatus :: Lens.Lens' DeleteBusinessReportScheduleResponse Core.Int
dbrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Network.AWS.SageMaker.UpdateMonitoringSchedule
    (
    -- * Creating a request
      UpdateMonitoringSchedule (..)
    , mkUpdateMonitoringSchedule
    -- ** Request lenses
    , umsMonitoringScheduleName
    , umsMonitoringScheduleConfig

    -- * Destructuring the response
    , UpdateMonitoringScheduleResponse (..)
    , mkUpdateMonitoringScheduleResponse
    -- ** Response lenses
    , umsrrsMonitoringScheduleArn
    , umsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { monitoringScheduleName :: Types.MonitoringScheduleName
    -- ^ The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
  , monitoringScheduleConfig :: Types.MonitoringScheduleConfig
    -- ^ The configuration object that specifies the monitoring schedule and defines the monitoring job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMonitoringSchedule' value with any optional fields omitted.
mkUpdateMonitoringSchedule
    :: Types.MonitoringScheduleName -- ^ 'monitoringScheduleName'
    -> Types.MonitoringScheduleConfig -- ^ 'monitoringScheduleConfig'
    -> UpdateMonitoringSchedule
mkUpdateMonitoringSchedule monitoringScheduleName
  monitoringScheduleConfig
  = UpdateMonitoringSchedule'{monitoringScheduleName,
                              monitoringScheduleConfig}

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Types.MonitoringScheduleName
umsMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# INLINEABLE umsMonitoringScheduleName #-}
{-# DEPRECATED monitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead"  #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule Types.MonitoringScheduleConfig
umsMonitoringScheduleConfig = Lens.field @"monitoringScheduleConfig"
{-# INLINEABLE umsMonitoringScheduleConfig #-}
{-# DEPRECATED monitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead"  #-}

instance Core.ToQuery UpdateMonitoringSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMonitoringSchedule where
        toHeaders UpdateMonitoringSchedule{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateMonitoringSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMonitoringSchedule where
        toJSON UpdateMonitoringSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MonitoringScheduleName" Core..= monitoringScheduleName),
                  Core.Just
                    ("MonitoringScheduleConfig" Core..= monitoringScheduleConfig)])

instance Core.AWSRequest UpdateMonitoringSchedule where
        type Rs UpdateMonitoringSchedule = UpdateMonitoringScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMonitoringScheduleResponse' Core.<$>
                   (x Core..: "MonitoringScheduleArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { monitoringScheduleArn :: Types.MonitoringScheduleArn
    -- ^ The Amazon Resource Name (ARN) of the monitoring schedule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMonitoringScheduleResponse' value with any optional fields omitted.
mkUpdateMonitoringScheduleResponse
    :: Types.MonitoringScheduleArn -- ^ 'monitoringScheduleArn'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateMonitoringScheduleResponse
mkUpdateMonitoringScheduleResponse monitoringScheduleArn
  responseStatus
  = UpdateMonitoringScheduleResponse'{monitoringScheduleArn,
                                      responseStatus}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrrsMonitoringScheduleArn :: Lens.Lens' UpdateMonitoringScheduleResponse Types.MonitoringScheduleArn
umsrrsMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# INLINEABLE umsrrsMonitoringScheduleArn #-}
{-# DEPRECATED monitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrrsResponseStatus :: Lens.Lens' UpdateMonitoringScheduleResponse Core.Int
umsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

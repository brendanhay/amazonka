{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the schedule for a monitoring job.
module Network.AWS.SageMaker.DescribeMonitoringSchedule
    (
    -- * Creating a request
      DescribeMonitoringSchedule (..)
    , mkDescribeMonitoringSchedule
    -- ** Request lenses
    , dmsMonitoringScheduleName

    -- * Destructuring the response
    , DescribeMonitoringScheduleResponse (..)
    , mkDescribeMonitoringScheduleResponse
    -- ** Response lenses
    , dmsrrsMonitoringScheduleArn
    , dmsrrsMonitoringScheduleName
    , dmsrrsMonitoringScheduleStatus
    , dmsrrsCreationTime
    , dmsrrsLastModifiedTime
    , dmsrrsMonitoringScheduleConfig
    , dmsrrsEndpointName
    , dmsrrsFailureReason
    , dmsrrsLastMonitoringExecutionSummary
    , dmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeMonitoringSchedule' smart constructor.
newtype DescribeMonitoringSchedule = DescribeMonitoringSchedule'
  { monitoringScheduleName :: Types.MonitoringScheduleName
    -- ^ Name of a previously created monitoring schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMonitoringSchedule' value with any optional fields omitted.
mkDescribeMonitoringSchedule
    :: Types.MonitoringScheduleName -- ^ 'monitoringScheduleName'
    -> DescribeMonitoringSchedule
mkDescribeMonitoringSchedule monitoringScheduleName
  = DescribeMonitoringSchedule'{monitoringScheduleName}

-- | Name of a previously created monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsMonitoringScheduleName :: Lens.Lens' DescribeMonitoringSchedule Types.MonitoringScheduleName
dmsMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# INLINEABLE dmsMonitoringScheduleName #-}
{-# DEPRECATED monitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead"  #-}

instance Core.ToQuery DescribeMonitoringSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMonitoringSchedule where
        toHeaders DescribeMonitoringSchedule{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.DescribeMonitoringSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMonitoringSchedule where
        toJSON DescribeMonitoringSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MonitoringScheduleName" Core..= monitoringScheduleName)])

instance Core.AWSRequest DescribeMonitoringSchedule where
        type Rs DescribeMonitoringSchedule =
             DescribeMonitoringScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMonitoringScheduleResponse' Core.<$>
                   (x Core..: "MonitoringScheduleArn") Core.<*>
                     x Core..: "MonitoringScheduleName"
                     Core.<*> x Core..: "MonitoringScheduleStatus"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "LastModifiedTime"
                     Core.<*> x Core..: "MonitoringScheduleConfig"
                     Core.<*> x Core..:? "EndpointName"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "LastMonitoringExecutionSummary"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeMonitoringScheduleResponse' smart constructor.
data DescribeMonitoringScheduleResponse = DescribeMonitoringScheduleResponse'
  { monitoringScheduleArn :: Types.MonitoringScheduleArn
    -- ^ The Amazon Resource Name (ARN) of the monitoring schedule.
  , monitoringScheduleName :: Types.MonitoringScheduleName
    -- ^ Name of the monitoring schedule.
  , monitoringScheduleStatus :: Types.ScheduleStatus
    -- ^ The status of an monitoring job.
  , creationTime :: Core.NominalDiffTime
    -- ^ The time at which the monitoring job was created.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ The time at which the monitoring job was last modified.
  , monitoringScheduleConfig :: Types.MonitoringScheduleConfig
    -- ^ The configuration object that specifies the monitoring schedule and defines the monitoring job.
  , endpointName :: Core.Maybe Types.EndpointName
    -- ^ The name of the endpoint for the monitoring job.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
  , lastMonitoringExecutionSummary :: Core.Maybe Types.MonitoringExecutionSummary
    -- ^ Describes metadata on the last execution to run, if there was one.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeMonitoringScheduleResponse' value with any optional fields omitted.
mkDescribeMonitoringScheduleResponse
    :: Types.MonitoringScheduleArn -- ^ 'monitoringScheduleArn'
    -> Types.MonitoringScheduleName -- ^ 'monitoringScheduleName'
    -> Types.ScheduleStatus -- ^ 'monitoringScheduleStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Types.MonitoringScheduleConfig -- ^ 'monitoringScheduleConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeMonitoringScheduleResponse
mkDescribeMonitoringScheduleResponse monitoringScheduleArn
  monitoringScheduleName monitoringScheduleStatus creationTime
  lastModifiedTime monitoringScheduleConfig responseStatus
  = DescribeMonitoringScheduleResponse'{monitoringScheduleArn,
                                        monitoringScheduleName, monitoringScheduleStatus,
                                        creationTime, lastModifiedTime, monitoringScheduleConfig,
                                        endpointName = Core.Nothing, failureReason = Core.Nothing,
                                        lastMonitoringExecutionSummary = Core.Nothing,
                                        responseStatus}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsMonitoringScheduleArn :: Lens.Lens' DescribeMonitoringScheduleResponse Types.MonitoringScheduleArn
dmsrrsMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# INLINEABLE dmsrrsMonitoringScheduleArn #-}
{-# DEPRECATED monitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead"  #-}

-- | Name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsMonitoringScheduleName :: Lens.Lens' DescribeMonitoringScheduleResponse Types.MonitoringScheduleName
dmsrrsMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# INLINEABLE dmsrrsMonitoringScheduleName #-}
{-# DEPRECATED monitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead"  #-}

-- | The status of an monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsMonitoringScheduleStatus :: Lens.Lens' DescribeMonitoringScheduleResponse Types.ScheduleStatus
dmsrrsMonitoringScheduleStatus = Lens.field @"monitoringScheduleStatus"
{-# INLINEABLE dmsrrsMonitoringScheduleStatus #-}
{-# DEPRECATED monitoringScheduleStatus "Use generic-lens or generic-optics with 'monitoringScheduleStatus' instead"  #-}

-- | The time at which the monitoring job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsCreationTime :: Lens.Lens' DescribeMonitoringScheduleResponse Core.NominalDiffTime
dmsrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dmsrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The time at which the monitoring job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsLastModifiedTime :: Lens.Lens' DescribeMonitoringScheduleResponse Core.NominalDiffTime
dmsrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dmsrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsMonitoringScheduleConfig :: Lens.Lens' DescribeMonitoringScheduleResponse Types.MonitoringScheduleConfig
dmsrrsMonitoringScheduleConfig = Lens.field @"monitoringScheduleConfig"
{-# INLINEABLE dmsrrsMonitoringScheduleConfig #-}
{-# DEPRECATED monitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead"  #-}

-- | The name of the endpoint for the monitoring job.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsEndpointName :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe Types.EndpointName)
dmsrrsEndpointName = Lens.field @"endpointName"
{-# INLINEABLE dmsrrsEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsFailureReason :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe Types.FailureReason)
dmsrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dmsrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Describes metadata on the last execution to run, if there was one.
--
-- /Note:/ Consider using 'lastMonitoringExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsLastMonitoringExecutionSummary :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe Types.MonitoringExecutionSummary)
dmsrrsLastMonitoringExecutionSummary = Lens.field @"lastMonitoringExecutionSummary"
{-# INLINEABLE dmsrrsLastMonitoringExecutionSummary #-}
{-# DEPRECATED lastMonitoringExecutionSummary "Use generic-lens or generic-optics with 'lastMonitoringExecutionSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsResponseStatus :: Lens.Lens' DescribeMonitoringScheduleResponse Core.Int
dmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

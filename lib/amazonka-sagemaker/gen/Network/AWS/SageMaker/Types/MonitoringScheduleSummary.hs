{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringScheduleSummary
  ( MonitoringScheduleSummary (..)
  -- * Smart constructor
  , mkMonitoringScheduleSummary
  -- * Lenses
  , mssMonitoringScheduleName
  , mssMonitoringScheduleArn
  , mssCreationTime
  , mssLastModifiedTime
  , mssMonitoringScheduleStatus
  , mssEndpointName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointName as Types
import qualified Network.AWS.SageMaker.Types.MonitoringScheduleArn as Types
import qualified Network.AWS.SageMaker.Types.MonitoringScheduleName as Types
import qualified Network.AWS.SageMaker.Types.ScheduleStatus as Types

-- | Summarizes the monitoring schedule.
--
-- /See:/ 'mkMonitoringScheduleSummary' smart constructor.
data MonitoringScheduleSummary = MonitoringScheduleSummary'
  { monitoringScheduleName :: Types.MonitoringScheduleName
    -- ^ The name of the monitoring schedule.
  , monitoringScheduleArn :: Types.MonitoringScheduleArn
    -- ^ The Amazon Resource Name (ARN) of the monitoring schedule.
  , creationTime :: Core.NominalDiffTime
    -- ^ The creation time of the monitoring schedule.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ The last time the monitoring schedule was modified.
  , monitoringScheduleStatus :: Types.ScheduleStatus
    -- ^ The status of the monitoring schedule.
  , endpointName :: Core.Maybe Types.EndpointName
    -- ^ The name of the endpoint using the monitoring schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MonitoringScheduleSummary' value with any optional fields omitted.
mkMonitoringScheduleSummary
    :: Types.MonitoringScheduleName -- ^ 'monitoringScheduleName'
    -> Types.MonitoringScheduleArn -- ^ 'monitoringScheduleArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Types.ScheduleStatus -- ^ 'monitoringScheduleStatus'
    -> MonitoringScheduleSummary
mkMonitoringScheduleSummary monitoringScheduleName
  monitoringScheduleArn creationTime lastModifiedTime
  monitoringScheduleStatus
  = MonitoringScheduleSummary'{monitoringScheduleName,
                               monitoringScheduleArn, creationTime, lastModifiedTime,
                               monitoringScheduleStatus, endpointName = Core.Nothing}

-- | The name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleName :: Lens.Lens' MonitoringScheduleSummary Types.MonitoringScheduleName
mssMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# INLINEABLE mssMonitoringScheduleName #-}
{-# DEPRECATED monitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleArn :: Lens.Lens' MonitoringScheduleSummary Types.MonitoringScheduleArn
mssMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# INLINEABLE mssMonitoringScheduleArn #-}
{-# DEPRECATED monitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead"  #-}

-- | The creation time of the monitoring schedule.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssCreationTime :: Lens.Lens' MonitoringScheduleSummary Core.NominalDiffTime
mssCreationTime = Lens.field @"creationTime"
{-# INLINEABLE mssCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The last time the monitoring schedule was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssLastModifiedTime :: Lens.Lens' MonitoringScheduleSummary Core.NominalDiffTime
mssLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE mssLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The status of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleStatus :: Lens.Lens' MonitoringScheduleSummary Types.ScheduleStatus
mssMonitoringScheduleStatus = Lens.field @"monitoringScheduleStatus"
{-# INLINEABLE mssMonitoringScheduleStatus #-}
{-# DEPRECATED monitoringScheduleStatus "Use generic-lens or generic-optics with 'monitoringScheduleStatus' instead"  #-}

-- | The name of the endpoint using the monitoring schedule.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEndpointName :: Lens.Lens' MonitoringScheduleSummary (Core.Maybe Types.EndpointName)
mssEndpointName = Lens.field @"endpointName"
{-# INLINEABLE mssEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

instance Core.FromJSON MonitoringScheduleSummary where
        parseJSON
          = Core.withObject "MonitoringScheduleSummary" Core.$
              \ x ->
                MonitoringScheduleSummary' Core.<$>
                  (x Core..: "MonitoringScheduleName") Core.<*>
                    x Core..: "MonitoringScheduleArn"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "LastModifiedTime"
                    Core.<*> x Core..: "MonitoringScheduleStatus"
                    Core.<*> x Core..:? "EndpointName"

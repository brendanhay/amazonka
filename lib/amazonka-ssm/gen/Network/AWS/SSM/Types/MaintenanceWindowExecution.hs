{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowExecution
  ( MaintenanceWindowExecution (..)
  -- * Smart constructor
  , mkMaintenanceWindowExecution
  -- * Lenses
  , mweEndTime
  , mweStartTime
  , mweStatus
  , mweStatusDetails
  , mweWindowExecutionId
  , mweWindowId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatusDetails as Types
import qualified Network.AWS.SSM.Types.WindowExecutionId as Types
import qualified Network.AWS.SSM.Types.WindowId as Types

-- | Describes the information about an execution of a maintenance window. 
--
-- /See:/ 'mkMaintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the execution finished.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the execution started.
  , status :: Core.Maybe Types.MaintenanceWindowExecutionStatus
    -- ^ The status of the execution.
  , statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails
    -- ^ The details explaining the Status. Only available for certain status values.
  , windowExecutionId :: Core.Maybe Types.WindowExecutionId
    -- ^ The ID of the maintenance window execution.
  , windowId :: Core.Maybe Types.WindowId
    -- ^ The ID of the maintenance window.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MaintenanceWindowExecution' value with any optional fields omitted.
mkMaintenanceWindowExecution
    :: MaintenanceWindowExecution
mkMaintenanceWindowExecution
  = MaintenanceWindowExecution'{endTime = Core.Nothing,
                                startTime = Core.Nothing, status = Core.Nothing,
                                statusDetails = Core.Nothing, windowExecutionId = Core.Nothing,
                                windowId = Core.Nothing}

-- | The time the execution finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweEndTime :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.NominalDiffTime)
mweEndTime = Lens.field @"endTime"
{-# INLINEABLE mweEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStartTime :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.NominalDiffTime)
mweStartTime = Lens.field @"startTime"
{-# INLINEABLE mweStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStatus :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Types.MaintenanceWindowExecutionStatus)
mweStatus = Lens.field @"status"
{-# INLINEABLE mweStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The details explaining the Status. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStatusDetails :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
mweStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE mweStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The ID of the maintenance window execution.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweWindowExecutionId :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Types.WindowExecutionId)
mweWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE mweWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweWindowId :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Types.WindowId)
mweWindowId = Lens.field @"windowId"
{-# INLINEABLE mweWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

instance Core.FromJSON MaintenanceWindowExecution where
        parseJSON
          = Core.withObject "MaintenanceWindowExecution" Core.$
              \ x ->
                MaintenanceWindowExecution' Core.<$>
                  (x Core..:? "EndTime") Core.<*> x Core..:? "StartTime" Core.<*>
                    x Core..:? "Status"
                    Core.<*> x Core..:? "StatusDetails"
                    Core.<*> x Core..:? "WindowExecutionId"
                    Core.<*> x Core..:? "WindowId"

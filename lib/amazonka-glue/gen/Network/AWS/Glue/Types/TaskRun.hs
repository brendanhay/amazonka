{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TaskRun
  ( TaskRun (..)
  -- * Smart constructor
  , mkTaskRun
  -- * Lenses
  , trCompletedOn
  , trErrorString
  , trExecutionTime
  , trLastModifiedOn
  , trLogGroupName
  , trProperties
  , trStartedOn
  , trStatus
  , trTaskRunId
  , trTransformId
  ) where

import qualified Network.AWS.Glue.Types.ErrorString as Types
import qualified Network.AWS.Glue.Types.LogGroupName as Types
import qualified Network.AWS.Glue.Types.TaskRunId as Types
import qualified Network.AWS.Glue.Types.TaskRunProperties as Types
import qualified Network.AWS.Glue.Types.TaskStatusType as Types
import qualified Network.AWS.Glue.Types.TransformId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The sampling parameters that are associated with the machine learning transform.
--
-- /See:/ 'mkTaskRun' smart constructor.
data TaskRun = TaskRun'
  { completedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The last point in time that the requested task run was completed.
  , errorString :: Core.Maybe Types.ErrorString
    -- ^ The list of error strings associated with this task run.
  , executionTime :: Core.Maybe Core.Int
    -- ^ The amount of time (in seconds) that the task run consumed resources.
  , lastModifiedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The last point in time that the requested task run was updated.
  , logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ The names of the log group for secure logging, associated with this task run.
  , properties :: Core.Maybe Types.TaskRunProperties
    -- ^ Specifies configuration properties associated with this task run.
  , startedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that this task run started.
  , status :: Core.Maybe Types.TaskStatusType
    -- ^ The current status of the requested task run.
  , taskRunId :: Core.Maybe Types.TaskRunId
    -- ^ The unique identifier for this task run.
  , transformId :: Core.Maybe Types.TransformId
    -- ^ The unique identifier for the transform.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TaskRun' value with any optional fields omitted.
mkTaskRun
    :: TaskRun
mkTaskRun
  = TaskRun'{completedOn = Core.Nothing, errorString = Core.Nothing,
             executionTime = Core.Nothing, lastModifiedOn = Core.Nothing,
             logGroupName = Core.Nothing, properties = Core.Nothing,
             startedOn = Core.Nothing, status = Core.Nothing,
             taskRunId = Core.Nothing, transformId = Core.Nothing}

-- | The last point in time that the requested task run was completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCompletedOn :: Lens.Lens' TaskRun (Core.Maybe Core.NominalDiffTime)
trCompletedOn = Lens.field @"completedOn"
{-# INLINEABLE trCompletedOn #-}
{-# DEPRECATED completedOn "Use generic-lens or generic-optics with 'completedOn' instead"  #-}

-- | The list of error strings associated with this task run.
--
-- /Note:/ Consider using 'errorString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trErrorString :: Lens.Lens' TaskRun (Core.Maybe Types.ErrorString)
trErrorString = Lens.field @"errorString"
{-# INLINEABLE trErrorString #-}
{-# DEPRECATED errorString "Use generic-lens or generic-optics with 'errorString' instead"  #-}

-- | The amount of time (in seconds) that the task run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trExecutionTime :: Lens.Lens' TaskRun (Core.Maybe Core.Int)
trExecutionTime = Lens.field @"executionTime"
{-# INLINEABLE trExecutionTime #-}
{-# DEPRECATED executionTime "Use generic-lens or generic-optics with 'executionTime' instead"  #-}

-- | The last point in time that the requested task run was updated.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trLastModifiedOn :: Lens.Lens' TaskRun (Core.Maybe Core.NominalDiffTime)
trLastModifiedOn = Lens.field @"lastModifiedOn"
{-# INLINEABLE trLastModifiedOn #-}
{-# DEPRECATED lastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead"  #-}

-- | The names of the log group for secure logging, associated with this task run.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trLogGroupName :: Lens.Lens' TaskRun (Core.Maybe Types.LogGroupName)
trLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE trLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | Specifies configuration properties associated with this task run.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trProperties :: Lens.Lens' TaskRun (Core.Maybe Types.TaskRunProperties)
trProperties = Lens.field @"properties"
{-# INLINEABLE trProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

-- | The date and time that this task run started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStartedOn :: Lens.Lens' TaskRun (Core.Maybe Core.NominalDiffTime)
trStartedOn = Lens.field @"startedOn"
{-# INLINEABLE trStartedOn #-}
{-# DEPRECATED startedOn "Use generic-lens or generic-optics with 'startedOn' instead"  #-}

-- | The current status of the requested task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStatus :: Lens.Lens' TaskRun (Core.Maybe Types.TaskStatusType)
trStatus = Lens.field @"status"
{-# INLINEABLE trStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The unique identifier for this task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTaskRunId :: Lens.Lens' TaskRun (Core.Maybe Types.TaskRunId)
trTaskRunId = Lens.field @"taskRunId"
{-# INLINEABLE trTaskRunId #-}
{-# DEPRECATED taskRunId "Use generic-lens or generic-optics with 'taskRunId' instead"  #-}

-- | The unique identifier for the transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTransformId :: Lens.Lens' TaskRun (Core.Maybe Types.TransformId)
trTransformId = Lens.field @"transformId"
{-# INLINEABLE trTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

instance Core.FromJSON TaskRun where
        parseJSON
          = Core.withObject "TaskRun" Core.$
              \ x ->
                TaskRun' Core.<$>
                  (x Core..:? "CompletedOn") Core.<*> x Core..:? "ErrorString"
                    Core.<*> x Core..:? "ExecutionTime"
                    Core.<*> x Core..:? "LastModifiedOn"
                    Core.<*> x Core..:? "LogGroupName"
                    Core.<*> x Core..:? "Properties"
                    Core.<*> x Core..:? "StartedOn"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TaskRunId"
                    Core.<*> x Core..:? "TransformId"

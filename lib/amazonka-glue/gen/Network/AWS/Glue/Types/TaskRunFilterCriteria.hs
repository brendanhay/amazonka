{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TaskRunFilterCriteria
  ( TaskRunFilterCriteria (..)
  -- * Smart constructor
  , mkTaskRunFilterCriteria
  -- * Lenses
  , trfcStartedAfter
  , trfcStartedBefore
  , trfcStatus
  , trfcTaskRunType
  ) where

import qualified Network.AWS.Glue.Types.TaskStatusType as Types
import qualified Network.AWS.Glue.Types.TaskType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria that are used to filter the task runs for the machine learning transform.
--
-- /See:/ 'mkTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { startedAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Filter on task runs started after this date.
  , startedBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Filter on task runs started before this date.
  , status :: Core.Maybe Types.TaskStatusType
    -- ^ The current status of the task run.
  , taskRunType :: Core.Maybe Types.TaskType
    -- ^ The type of task run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TaskRunFilterCriteria' value with any optional fields omitted.
mkTaskRunFilterCriteria
    :: TaskRunFilterCriteria
mkTaskRunFilterCriteria
  = TaskRunFilterCriteria'{startedAfter = Core.Nothing,
                           startedBefore = Core.Nothing, status = Core.Nothing,
                           taskRunType = Core.Nothing}

-- | Filter on task runs started after this date.
--
-- /Note:/ Consider using 'startedAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStartedAfter :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Core.NominalDiffTime)
trfcStartedAfter = Lens.field @"startedAfter"
{-# INLINEABLE trfcStartedAfter #-}
{-# DEPRECATED startedAfter "Use generic-lens or generic-optics with 'startedAfter' instead"  #-}

-- | Filter on task runs started before this date.
--
-- /Note:/ Consider using 'startedBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStartedBefore :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Core.NominalDiffTime)
trfcStartedBefore = Lens.field @"startedBefore"
{-# INLINEABLE trfcStartedBefore #-}
{-# DEPRECATED startedBefore "Use generic-lens or generic-optics with 'startedBefore' instead"  #-}

-- | The current status of the task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStatus :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Types.TaskStatusType)
trfcStatus = Lens.field @"status"
{-# INLINEABLE trfcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The type of task run.
--
-- /Note:/ Consider using 'taskRunType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcTaskRunType :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Types.TaskType)
trfcTaskRunType = Lens.field @"taskRunType"
{-# INLINEABLE trfcTaskRunType #-}
{-# DEPRECATED taskRunType "Use generic-lens or generic-optics with 'taskRunType' instead"  #-}

instance Core.FromJSON TaskRunFilterCriteria where
        toJSON TaskRunFilterCriteria{..}
          = Core.object
              (Core.catMaybes
                 [("StartedAfter" Core..=) Core.<$> startedAfter,
                  ("StartedBefore" Core..=) Core.<$> startedBefore,
                  ("Status" Core..=) Core.<$> status,
                  ("TaskRunType" Core..=) Core.<$> taskRunType])

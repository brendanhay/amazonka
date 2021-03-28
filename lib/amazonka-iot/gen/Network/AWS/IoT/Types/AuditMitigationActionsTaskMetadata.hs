{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
  ( AuditMitigationActionsTaskMetadata (..)
  -- * Smart constructor
  , mkAuditMitigationActionsTaskMetadata
  -- * Lenses
  , amatmStartTime
  , amatmTaskId
  , amatmTaskStatus
  ) where

import qualified Network.AWS.IoT.Types.AuditMitigationActionsTaskId as Types
import qualified Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an audit mitigation actions task that is returned by @ListAuditMitigationActionsTasks@ .
--
-- /See:/ 'mkAuditMitigationActionsTaskMetadata' smart constructor.
data AuditMitigationActionsTaskMetadata = AuditMitigationActionsTaskMetadata'
  { startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the audit mitigation actions task was started.
  , taskId :: Core.Maybe Types.AuditMitigationActionsTaskId
    -- ^ The unique identifier for the task.
  , taskStatus :: Core.Maybe Types.AuditMitigationActionsTaskStatus
    -- ^ The current state of the audit mitigation actions task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AuditMitigationActionsTaskMetadata' value with any optional fields omitted.
mkAuditMitigationActionsTaskMetadata
    :: AuditMitigationActionsTaskMetadata
mkAuditMitigationActionsTaskMetadata
  = AuditMitigationActionsTaskMetadata'{startTime = Core.Nothing,
                                        taskId = Core.Nothing, taskStatus = Core.Nothing}

-- | The time at which the audit mitigation actions task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmStartTime :: Lens.Lens' AuditMitigationActionsTaskMetadata (Core.Maybe Core.NominalDiffTime)
amatmStartTime = Lens.field @"startTime"
{-# INLINEABLE amatmStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The unique identifier for the task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmTaskId :: Lens.Lens' AuditMitigationActionsTaskMetadata (Core.Maybe Types.AuditMitigationActionsTaskId)
amatmTaskId = Lens.field @"taskId"
{-# INLINEABLE amatmTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The current state of the audit mitigation actions task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmTaskStatus :: Lens.Lens' AuditMitigationActionsTaskMetadata (Core.Maybe Types.AuditMitigationActionsTaskStatus)
amatmTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE amatmTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

instance Core.FromJSON AuditMitigationActionsTaskMetadata where
        parseJSON
          = Core.withObject "AuditMitigationActionsTaskMetadata" Core.$
              \ x ->
                AuditMitigationActionsTaskMetadata' Core.<$>
                  (x Core..:? "startTime") Core.<*> x Core..:? "taskId" Core.<*>
                    x Core..:? "taskStatus"

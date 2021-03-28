{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditTaskMetadata
  ( AuditTaskMetadata (..)
  -- * Smart constructor
  , mkAuditTaskMetadata
  -- * Lenses
  , atmTaskId
  , atmTaskStatus
  , atmTaskType
  ) where

import qualified Network.AWS.IoT.Types.AuditTaskId as Types
import qualified Network.AWS.IoT.Types.AuditTaskStatus as Types
import qualified Network.AWS.IoT.Types.AuditTaskType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The audits that were performed.
--
-- /See:/ 'mkAuditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { taskId :: Core.Maybe Types.AuditTaskId
    -- ^ The ID of this audit.
  , taskStatus :: Core.Maybe Types.AuditTaskStatus
    -- ^ The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
  , taskType :: Core.Maybe Types.AuditTaskType
    -- ^ The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuditTaskMetadata' value with any optional fields omitted.
mkAuditTaskMetadata
    :: AuditTaskMetadata
mkAuditTaskMetadata
  = AuditTaskMetadata'{taskId = Core.Nothing,
                       taskStatus = Core.Nothing, taskType = Core.Nothing}

-- | The ID of this audit.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskId :: Lens.Lens' AuditTaskMetadata (Core.Maybe Types.AuditTaskId)
atmTaskId = Lens.field @"taskId"
{-# INLINEABLE atmTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskStatus :: Lens.Lens' AuditTaskMetadata (Core.Maybe Types.AuditTaskStatus)
atmTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE atmTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskType :: Lens.Lens' AuditTaskMetadata (Core.Maybe Types.AuditTaskType)
atmTaskType = Lens.field @"taskType"
{-# INLINEABLE atmTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

instance Core.FromJSON AuditTaskMetadata where
        parseJSON
          = Core.withObject "AuditTaskMetadata" Core.$
              \ x ->
                AuditTaskMetadata' Core.<$>
                  (x Core..:? "taskId") Core.<*> x Core..:? "taskStatus" Core.<*>
                    x Core..:? "taskType"

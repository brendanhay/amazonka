{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceExecutionSummary
  ( ComplianceExecutionSummary (..),

    -- * Smart constructor
    mkComplianceExecutionSummary,

    -- * Lenses
    cesExecutionTime,
    cesExecutionId,
    cesExecutionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ComplianceExecutionId as Types
import qualified Network.AWS.SSM.Types.ComplianceExecutionType as Types

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /See:/ 'mkComplianceExecutionSummary' smart constructor.
data ComplianceExecutionSummary = ComplianceExecutionSummary'
  { -- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
    executionTime :: Core.NominalDiffTime,
    -- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
    executionId :: Core.Maybe Types.ComplianceExecutionId,
    -- | The type of execution. For example, @Command@ is a valid execution type.
    executionType :: Core.Maybe Types.ComplianceExecutionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ComplianceExecutionSummary' value with any optional fields omitted.
mkComplianceExecutionSummary ::
  -- | 'executionTime'
  Core.NominalDiffTime ->
  ComplianceExecutionSummary
mkComplianceExecutionSummary executionTime =
  ComplianceExecutionSummary'
    { executionTime,
      executionId = Core.Nothing,
      executionType = Core.Nothing
    }

-- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionTime :: Lens.Lens' ComplianceExecutionSummary Core.NominalDiffTime
cesExecutionTime = Lens.field @"executionTime"
{-# DEPRECATED cesExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionId :: Lens.Lens' ComplianceExecutionSummary (Core.Maybe Types.ComplianceExecutionId)
cesExecutionId = Lens.field @"executionId"
{-# DEPRECATED cesExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The type of execution. For example, @Command@ is a valid execution type.
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionType :: Lens.Lens' ComplianceExecutionSummary (Core.Maybe Types.ComplianceExecutionType)
cesExecutionType = Lens.field @"executionType"
{-# DEPRECATED cesExecutionType "Use generic-lens or generic-optics with 'executionType' instead." #-}

instance Core.FromJSON ComplianceExecutionSummary where
  toJSON ComplianceExecutionSummary {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ExecutionTime" Core..= executionTime),
            ("ExecutionId" Core..=) Core.<$> executionId,
            ("ExecutionType" Core..=) Core.<$> executionType
          ]
      )

instance Core.FromJSON ComplianceExecutionSummary where
  parseJSON =
    Core.withObject "ComplianceExecutionSummary" Core.$
      \x ->
        ComplianceExecutionSummary'
          Core.<$> (x Core..: "ExecutionTime")
          Core.<*> (x Core..:? "ExecutionId")
          Core.<*> (x Core..:? "ExecutionType")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStatus
  ( RemediationExecutionStatus (..),

    -- * Smart constructor
    mkRemediationExecutionStatus,

    -- * Lenses
    rInvocationTime,
    rLastUpdatedTime,
    rResourceKey,
    rState,
    rStepDetails,
  )
where

import qualified Network.AWS.Config.Types.RemediationExecutionState as Types
import qualified Network.AWS.Config.Types.RemediationExecutionStep as Types
import qualified Network.AWS.Config.Types.ResourceKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides details of the current status of the invoked remediation action for that resource.
--
-- /See:/ 'mkRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { -- | Start time when the remediation was executed.
    invocationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time when the remediation execution was last updated.
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime,
    resourceKey :: Core.Maybe Types.ResourceKey,
    -- | ENUM of the values.
    state :: Core.Maybe Types.RemediationExecutionState,
    -- | Details of every step.
    stepDetails :: Core.Maybe [Types.RemediationExecutionStep]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RemediationExecutionStatus' value with any optional fields omitted.
mkRemediationExecutionStatus ::
  RemediationExecutionStatus
mkRemediationExecutionStatus =
  RemediationExecutionStatus'
    { invocationTime = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      resourceKey = Core.Nothing,
      state = Core.Nothing,
      stepDetails = Core.Nothing
    }

-- | Start time when the remediation was executed.
--
-- /Note:/ Consider using 'invocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInvocationTime :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Core.NominalDiffTime)
rInvocationTime = Lens.field @"invocationTime"
{-# DEPRECATED rInvocationTime "Use generic-lens or generic-optics with 'invocationTime' instead." #-}

-- | The time when the remediation execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Core.NominalDiffTime)
rLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED rLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceKey :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Types.ResourceKey)
rResourceKey = Lens.field @"resourceKey"
{-# DEPRECATED rResourceKey "Use generic-lens or generic-optics with 'resourceKey' instead." #-}

-- | ENUM of the values.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Types.RemediationExecutionState)
rState = Lens.field @"state"
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Details of every step.
--
-- /Note:/ Consider using 'stepDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStepDetails :: Lens.Lens' RemediationExecutionStatus (Core.Maybe [Types.RemediationExecutionStep])
rStepDetails = Lens.field @"stepDetails"
{-# DEPRECATED rStepDetails "Use generic-lens or generic-optics with 'stepDetails' instead." #-}

instance Core.FromJSON RemediationExecutionStatus where
  parseJSON =
    Core.withObject "RemediationExecutionStatus" Core.$
      \x ->
        RemediationExecutionStatus'
          Core.<$> (x Core..:? "InvocationTime")
          Core.<*> (x Core..:? "LastUpdatedTime")
          Core.<*> (x Core..:? "ResourceKey")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "StepDetails")

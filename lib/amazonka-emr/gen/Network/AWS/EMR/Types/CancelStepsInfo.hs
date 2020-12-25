{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CancelStepsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CancelStepsInfo
  ( CancelStepsInfo (..),

    -- * Smart constructor
    mkCancelStepsInfo,

    -- * Lenses
    csiReason,
    csiStatus,
    csiStepId,
  )
where

import qualified Network.AWS.EMR.Types.CancelStepsRequestStatus as Types
import qualified Network.AWS.EMR.Types.StepId as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.
--
-- /See:/ 'mkCancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
  { -- | The reason for the failure if the CancelSteps request fails.
    reason :: Core.Maybe Types.String,
    -- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
    status :: Core.Maybe Types.CancelStepsRequestStatus,
    -- | The encrypted StepId of a step.
    stepId :: Core.Maybe Types.StepId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelStepsInfo' value with any optional fields omitted.
mkCancelStepsInfo ::
  CancelStepsInfo
mkCancelStepsInfo =
  CancelStepsInfo'
    { reason = Core.Nothing,
      status = Core.Nothing,
      stepId = Core.Nothing
    }

-- | The reason for the failure if the CancelSteps request fails.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiReason :: Lens.Lens' CancelStepsInfo (Core.Maybe Types.String)
csiReason = Lens.field @"reason"
{-# DEPRECATED csiReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStatus :: Lens.Lens' CancelStepsInfo (Core.Maybe Types.CancelStepsRequestStatus)
csiStatus = Lens.field @"status"
{-# DEPRECATED csiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The encrypted StepId of a step.
--
-- /Note:/ Consider using 'stepId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStepId :: Lens.Lens' CancelStepsInfo (Core.Maybe Types.StepId)
csiStepId = Lens.field @"stepId"
{-# DEPRECATED csiStepId "Use generic-lens or generic-optics with 'stepId' instead." #-}

instance Core.FromJSON CancelStepsInfo where
  parseJSON =
    Core.withObject "CancelStepsInfo" Core.$
      \x ->
        CancelStepsInfo'
          Core.<$> (x Core..:? "Reason")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StepId")

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
    csiStatus,
    csiStepId,
    csiReason,
  )
where

import Network.AWS.EMR.Types.CancelStepsRequestStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.
--
-- /See:/ 'mkCancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
  { -- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
    status :: Lude.Maybe CancelStepsRequestStatus,
    -- | The encrypted StepId of a step.
    stepId :: Lude.Maybe Lude.Text,
    -- | The reason for the failure if the CancelSteps request fails.
    reason :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelStepsInfo' with the minimum fields required to make a request.
--
-- * 'status' - The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
-- * 'stepId' - The encrypted StepId of a step.
-- * 'reason' - The reason for the failure if the CancelSteps request fails.
mkCancelStepsInfo ::
  CancelStepsInfo
mkCancelStepsInfo =
  CancelStepsInfo'
    { status = Lude.Nothing,
      stepId = Lude.Nothing,
      reason = Lude.Nothing
    }

-- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStatus :: Lens.Lens' CancelStepsInfo (Lude.Maybe CancelStepsRequestStatus)
csiStatus = Lens.lens (status :: CancelStepsInfo -> Lude.Maybe CancelStepsRequestStatus) (\s a -> s {status = a} :: CancelStepsInfo)
{-# DEPRECATED csiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The encrypted StepId of a step.
--
-- /Note:/ Consider using 'stepId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStepId :: Lens.Lens' CancelStepsInfo (Lude.Maybe Lude.Text)
csiStepId = Lens.lens (stepId :: CancelStepsInfo -> Lude.Maybe Lude.Text) (\s a -> s {stepId = a} :: CancelStepsInfo)
{-# DEPRECATED csiStepId "Use generic-lens or generic-optics with 'stepId' instead." #-}

-- | The reason for the failure if the CancelSteps request fails.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiReason :: Lens.Lens' CancelStepsInfo (Lude.Maybe Lude.Text)
csiReason = Lens.lens (reason :: CancelStepsInfo -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: CancelStepsInfo)
{-# DEPRECATED csiReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.FromJSON CancelStepsInfo where
  parseJSON =
    Lude.withObject
      "CancelStepsInfo"
      ( \x ->
          CancelStepsInfo'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "StepId")
            Lude.<*> (x Lude..:? "Reason")
      )

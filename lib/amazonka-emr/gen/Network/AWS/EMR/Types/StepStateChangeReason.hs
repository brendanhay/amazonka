{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStateChangeReason
  ( StepStateChangeReason (..),

    -- * Smart constructor
    mkStepStateChangeReason,

    -- * Lenses
    sscrCode,
    sscrMessage,
  )
where

import Network.AWS.EMR.Types.StepStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the step state change reason.
--
-- /See:/ 'mkStepStateChangeReason' smart constructor.
data StepStateChangeReason = StepStateChangeReason'
  { code ::
      Lude.Maybe StepStateChangeReasonCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
-- * 'message' - The descriptive message for the state change reason.
mkStepStateChangeReason ::
  StepStateChangeReason
mkStepStateChangeReason =
  StepStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrCode :: Lens.Lens' StepStateChangeReason (Lude.Maybe StepStateChangeReasonCode)
sscrCode = Lens.lens (code :: StepStateChangeReason -> Lude.Maybe StepStateChangeReasonCode) (\s a -> s {code = a} :: StepStateChangeReason)
{-# DEPRECATED sscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The descriptive message for the state change reason.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrMessage :: Lens.Lens' StepStateChangeReason (Lude.Maybe Lude.Text)
sscrMessage = Lens.lens (message :: StepStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: StepStateChangeReason)
{-# DEPRECATED sscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON StepStateChangeReason where
  parseJSON =
    Lude.withObject
      "StepStateChangeReason"
      ( \x ->
          StepStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )

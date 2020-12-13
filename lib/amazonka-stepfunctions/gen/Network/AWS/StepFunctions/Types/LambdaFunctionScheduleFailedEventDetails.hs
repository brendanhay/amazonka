{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
  ( LambdaFunctionScheduleFailedEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionScheduleFailedEventDetails,

    -- * Lenses
    lError,
    lCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a failed lambda function schedule event that occurred during an execution.
--
-- /See:/ 'mkLambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { -- | The error code of the failure.
    error :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'error' - The error code of the failure.
-- * 'cause' - A more detailed explanation of the cause of the failure.
mkLambdaFunctionScheduleFailedEventDetails ::
  LambdaFunctionScheduleFailedEventDetails
mkLambdaFunctionScheduleFailedEventDetails =
  LambdaFunctionScheduleFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lError :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lError = Lens.lens (error :: LambdaFunctionScheduleFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: LambdaFunctionScheduleFailedEventDetails)
{-# DEPRECATED lError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCause :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lCause = Lens.lens (cause :: LambdaFunctionScheduleFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: LambdaFunctionScheduleFailedEventDetails)
{-# DEPRECATED lCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON LambdaFunctionScheduleFailedEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionScheduleFailedEventDetails"
      ( \x ->
          LambdaFunctionScheduleFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

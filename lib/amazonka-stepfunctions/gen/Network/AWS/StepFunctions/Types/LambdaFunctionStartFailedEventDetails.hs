-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
  ( LambdaFunctionStartFailedEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionStartFailedEventDetails,

    -- * Lenses
    lfsfedError,
    lfsfedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a lambda function that failed to start during an execution.
--
-- /See:/ 'mkLambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { error ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    cause ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionStartFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkLambdaFunctionStartFailedEventDetails ::
  LambdaFunctionStartFailedEventDetails
mkLambdaFunctionStartFailedEventDetails =
  LambdaFunctionStartFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsfedError :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lfsfedError = Lens.lens (error :: LambdaFunctionStartFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: LambdaFunctionStartFailedEventDetails)
{-# DEPRECATED lfsfedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsfedCause :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lfsfedCause = Lens.lens (cause :: LambdaFunctionStartFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: LambdaFunctionStartFailedEventDetails)
{-# DEPRECATED lfsfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON LambdaFunctionStartFailedEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionStartFailedEventDetails"
      ( \x ->
          LambdaFunctionStartFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

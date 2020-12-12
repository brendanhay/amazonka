{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
  ( LambdaFunctionFailedEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionFailedEventDetails,

    -- * Lenses
    lffedError,
    lffedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a lambda function that failed during an execution.
--
-- /See:/ 'mkLambdaFunctionFailedEventDetails' smart constructor.
data LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails'
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

-- | Creates a value of 'LambdaFunctionFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkLambdaFunctionFailedEventDetails ::
  LambdaFunctionFailedEventDetails
mkLambdaFunctionFailedEventDetails =
  LambdaFunctionFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffedError :: Lens.Lens' LambdaFunctionFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lffedError = Lens.lens (error :: LambdaFunctionFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: LambdaFunctionFailedEventDetails)
{-# DEPRECATED lffedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffedCause :: Lens.Lens' LambdaFunctionFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lffedCause = Lens.lens (cause :: LambdaFunctionFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: LambdaFunctionFailedEventDetails)
{-# DEPRECATED lffedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON LambdaFunctionFailedEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionFailedEventDetails"
      ( \x ->
          LambdaFunctionFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

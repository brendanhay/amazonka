{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
  ( LambdaFunctionTimedOutEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionTimedOutEventDetails,

    -- * Lenses
    lftoedError,
    lftoedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a lambda function timeout that occurred during an execution.
--
-- /See:/ 'mkLambdaFunctionTimedOutEventDetails' smart constructor.
data LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails'
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

-- | Creates a value of 'LambdaFunctionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the timeout.
-- * 'error' - The error code of the failure.
mkLambdaFunctionTimedOutEventDetails ::
  LambdaFunctionTimedOutEventDetails
mkLambdaFunctionTimedOutEventDetails =
  LambdaFunctionTimedOutEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoedError :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lftoedError = Lens.lens (error :: LambdaFunctionTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: LambdaFunctionTimedOutEventDetails)
{-# DEPRECATED lftoedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the timeout.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoedCause :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lftoedCause = Lens.lens (cause :: LambdaFunctionTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: LambdaFunctionTimedOutEventDetails)
{-# DEPRECATED lftoedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON LambdaFunctionTimedOutEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionTimedOutEventDetails"
      ( \x ->
          LambdaFunctionTimedOutEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

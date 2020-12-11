-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
  ( ExecutionAbortedEventDetails (..),

    -- * Smart constructor
    mkExecutionAbortedEventDetails,

    -- * Lenses
    eaedError,
    eaedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an abort of an execution.
--
-- /See:/ 'mkExecutionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { error ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionAbortedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkExecutionAbortedEventDetails ::
  ExecutionAbortedEventDetails
mkExecutionAbortedEventDetails =
  ExecutionAbortedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaedError :: Lens.Lens' ExecutionAbortedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
eaedError = Lens.lens (error :: ExecutionAbortedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ExecutionAbortedEventDetails)
{-# DEPRECATED eaedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaedCause :: Lens.Lens' ExecutionAbortedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
eaedCause = Lens.lens (cause :: ExecutionAbortedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ExecutionAbortedEventDetails)
{-# DEPRECATED eaedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ExecutionAbortedEventDetails where
  parseJSON =
    Lude.withObject
      "ExecutionAbortedEventDetails"
      ( \x ->
          ExecutionAbortedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

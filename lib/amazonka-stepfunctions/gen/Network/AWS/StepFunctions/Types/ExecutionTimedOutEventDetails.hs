-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
  ( ExecutionTimedOutEventDetails (..),

    -- * Smart constructor
    mkExecutionTimedOutEventDetails,

    -- * Lenses
    etoedError,
    etoedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the execution timeout that occurred during the execution.
--
-- /See:/ 'mkExecutionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { error ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the timeout.
-- * 'error' - The error code of the failure.
mkExecutionTimedOutEventDetails ::
  ExecutionTimedOutEventDetails
mkExecutionTimedOutEventDetails =
  ExecutionTimedOutEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etoedError :: Lens.Lens' ExecutionTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
etoedError = Lens.lens (error :: ExecutionTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ExecutionTimedOutEventDetails)
{-# DEPRECATED etoedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the timeout.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etoedCause :: Lens.Lens' ExecutionTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
etoedCause = Lens.lens (cause :: ExecutionTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ExecutionTimedOutEventDetails)
{-# DEPRECATED etoedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ExecutionTimedOutEventDetails where
  parseJSON =
    Lude.withObject
      "ExecutionTimedOutEventDetails"
      ( \x ->
          ExecutionTimedOutEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

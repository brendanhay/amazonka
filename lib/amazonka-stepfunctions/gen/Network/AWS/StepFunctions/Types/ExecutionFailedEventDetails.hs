{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
  ( ExecutionFailedEventDetails (..),

    -- * Smart constructor
    mkExecutionFailedEventDetails,

    -- * Lenses
    efedError,
    efedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an execution failure event.
--
-- /See:/ 'mkExecutionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { error ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkExecutionFailedEventDetails ::
  ExecutionFailedEventDetails
mkExecutionFailedEventDetails =
  ExecutionFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efedError :: Lens.Lens' ExecutionFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
efedError = Lens.lens (error :: ExecutionFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ExecutionFailedEventDetails)
{-# DEPRECATED efedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efedCause :: Lens.Lens' ExecutionFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
efedCause = Lens.lens (cause :: ExecutionFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ExecutionFailedEventDetails)
{-# DEPRECATED efedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ExecutionFailedEventDetails where
  parseJSON =
    Lude.withObject
      "ExecutionFailedEventDetails"
      ( \x ->
          ExecutionFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

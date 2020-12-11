-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
  ( ActivityFailedEventDetails (..),

    -- * Smart constructor
    mkActivityFailedEventDetails,

    -- * Lenses
    afedError,
    afedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an activity that failed during an execution.
--
-- /See:/ 'mkActivityFailedEventDetails' smart constructor.
data ActivityFailedEventDetails = ActivityFailedEventDetails'
  { error ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkActivityFailedEventDetails ::
  ActivityFailedEventDetails
mkActivityFailedEventDetails =
  ActivityFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afedError :: Lens.Lens' ActivityFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
afedError = Lens.lens (error :: ActivityFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ActivityFailedEventDetails)
{-# DEPRECATED afedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afedCause :: Lens.Lens' ActivityFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
afedCause = Lens.lens (cause :: ActivityFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ActivityFailedEventDetails)
{-# DEPRECATED afedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ActivityFailedEventDetails where
  parseJSON =
    Lude.withObject
      "ActivityFailedEventDetails"
      ( \x ->
          ActivityFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

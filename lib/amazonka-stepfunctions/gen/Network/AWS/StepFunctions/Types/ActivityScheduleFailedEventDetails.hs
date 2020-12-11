-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
  ( ActivityScheduleFailedEventDetails (..),

    -- * Smart constructor
    mkActivityScheduleFailedEventDetails,

    -- * Lenses
    asfedError,
    asfedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an activity schedule failure that occurred during an execution.
--
-- /See:/ 'mkActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
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

-- | Creates a value of 'ActivityScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
mkActivityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
mkActivityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfedError :: Lens.Lens' ActivityScheduleFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
asfedError = Lens.lens (error :: ActivityScheduleFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ActivityScheduleFailedEventDetails)
{-# DEPRECATED asfedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfedCause :: Lens.Lens' ActivityScheduleFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
asfedCause = Lens.lens (cause :: ActivityScheduleFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ActivityScheduleFailedEventDetails)
{-# DEPRECATED asfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ActivityScheduleFailedEventDetails where
  parseJSON =
    Lude.withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

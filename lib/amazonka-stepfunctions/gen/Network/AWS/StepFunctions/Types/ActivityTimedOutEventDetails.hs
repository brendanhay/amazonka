{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
  ( ActivityTimedOutEventDetails (..),

    -- * Smart constructor
    mkActivityTimedOutEventDetails,

    -- * Lenses
    atoedError,
    atoedCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an activity timeout that occurred during an execution.
--
-- /See:/ 'mkActivityTimedOutEventDetails' smart constructor.
data ActivityTimedOutEventDetails = ActivityTimedOutEventDetails'
  { error ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTimedOutEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the timeout.
-- * 'error' - The error code of the failure.
mkActivityTimedOutEventDetails ::
  ActivityTimedOutEventDetails
mkActivityTimedOutEventDetails =
  ActivityTimedOutEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atoedError :: Lens.Lens' ActivityTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
atoedError = Lens.lens (error :: ActivityTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: ActivityTimedOutEventDetails)
{-# DEPRECATED atoedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the timeout.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atoedCause :: Lens.Lens' ActivityTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
atoedCause = Lens.lens (cause :: ActivityTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: ActivityTimedOutEventDetails)
{-# DEPRECATED atoedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance Lude.FromJSON ActivityTimedOutEventDetails where
  parseJSON =
    Lude.withObject
      "ActivityTimedOutEventDetails"
      ( \x ->
          ActivityTimedOutEventDetails'
            Lude.<$> (x Lude..:? "error") Lude.<*> (x Lude..:? "cause")
      )

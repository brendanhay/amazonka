-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
  ( RepositoryTriggerExecutionFailure (..),

    -- * Smart constructor
    mkRepositoryTriggerExecutionFailure,

    -- * Lenses
    rtefFailureMessage,
    rtefTrigger,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A trigger failed to run.
--
-- /See:/ 'mkRepositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { failureMessage ::
      Lude.Maybe Lude.Text,
    trigger ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepositoryTriggerExecutionFailure' with the minimum fields required to make a request.
--
-- * 'failureMessage' - Message information about the trigger that did not run.
-- * 'trigger' - The name of the trigger that did not run.
mkRepositoryTriggerExecutionFailure ::
  RepositoryTriggerExecutionFailure
mkRepositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
    { failureMessage = Lude.Nothing,
      trigger = Lude.Nothing
    }

-- | Message information about the trigger that did not run.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtefFailureMessage :: Lens.Lens' RepositoryTriggerExecutionFailure (Lude.Maybe Lude.Text)
rtefFailureMessage = Lens.lens (failureMessage :: RepositoryTriggerExecutionFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: RepositoryTriggerExecutionFailure)
{-# DEPRECATED rtefFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The name of the trigger that did not run.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtefTrigger :: Lens.Lens' RepositoryTriggerExecutionFailure (Lude.Maybe Lude.Text)
rtefTrigger = Lens.lens (trigger :: RepositoryTriggerExecutionFailure -> Lude.Maybe Lude.Text) (\s a -> s {trigger = a} :: RepositoryTriggerExecutionFailure)
{-# DEPRECATED rtefTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Lude.FromJSON RepositoryTriggerExecutionFailure where
  parseJSON =
    Lude.withObject
      "RepositoryTriggerExecutionFailure"
      ( \x ->
          RepositoryTriggerExecutionFailure'
            Lude.<$> (x Lude..:? "failureMessage") Lude.<*> (x Lude..:? "trigger")
      )

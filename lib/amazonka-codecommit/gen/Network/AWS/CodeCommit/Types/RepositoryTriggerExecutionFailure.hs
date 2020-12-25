{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailureMessage as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryTriggerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A trigger failed to run.
--
-- /See:/ 'mkRepositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { -- | Message information about the trigger that did not run.
    failureMessage :: Core.Maybe Types.RepositoryTriggerExecutionFailureMessage,
    -- | The name of the trigger that did not run.
    trigger :: Core.Maybe Types.RepositoryTriggerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RepositoryTriggerExecutionFailure' value with any optional fields omitted.
mkRepositoryTriggerExecutionFailure ::
  RepositoryTriggerExecutionFailure
mkRepositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
    { failureMessage = Core.Nothing,
      trigger = Core.Nothing
    }

-- | Message information about the trigger that did not run.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtefFailureMessage :: Lens.Lens' RepositoryTriggerExecutionFailure (Core.Maybe Types.RepositoryTriggerExecutionFailureMessage)
rtefFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED rtefFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The name of the trigger that did not run.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtefTrigger :: Lens.Lens' RepositoryTriggerExecutionFailure (Core.Maybe Types.RepositoryTriggerName)
rtefTrigger = Lens.field @"trigger"
{-# DEPRECATED rtefTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Core.FromJSON RepositoryTriggerExecutionFailure where
  parseJSON =
    Core.withObject "RepositoryTriggerExecutionFailure" Core.$
      \x ->
        RepositoryTriggerExecutionFailure'
          Core.<$> (x Core..:? "failureMessage") Core.<*> (x Core..:? "trigger")

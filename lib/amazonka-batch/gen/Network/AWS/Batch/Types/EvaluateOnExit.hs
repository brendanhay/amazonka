{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.EvaluateOnExit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.EvaluateOnExit
  ( EvaluateOnExit (..),

    -- * Smart constructor
    mkEvaluateOnExit,

    -- * Lenses
    eoeAction,
    eoeOnExitCode,
    eoeOnReason,
    eoeOnStatusReason,
  )
where

import qualified Network.AWS.Batch.Types.OnExitCode as Types
import qualified Network.AWS.Batch.Types.OnReason as Types
import qualified Network.AWS.Batch.Types.OnStatusReason as Types
import qualified Network.AWS.Batch.Types.RetryAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a set of conditions to be met, and an action to take (@RETRY@ or @EXIT@ ) if all conditions are met.
--
-- /See:/ 'mkEvaluateOnExit' smart constructor.
data EvaluateOnExit = EvaluateOnExit'
  { -- | Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
    action :: Types.RetryAction,
    -- | Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onExitCode :: Core.Maybe Types.OnExitCode,
    -- | Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onReason :: Core.Maybe Types.OnReason,
    -- | Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onStatusReason :: Core.Maybe Types.OnStatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluateOnExit' value with any optional fields omitted.
mkEvaluateOnExit ::
  -- | 'action'
  Types.RetryAction ->
  EvaluateOnExit
mkEvaluateOnExit action =
  EvaluateOnExit'
    { action,
      onExitCode = Core.Nothing,
      onReason = Core.Nothing,
      onStatusReason = Core.Nothing
    }

-- | Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeAction :: Lens.Lens' EvaluateOnExit Types.RetryAction
eoeAction = Lens.field @"action"
{-# DEPRECATED eoeAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onExitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnExitCode :: Lens.Lens' EvaluateOnExit (Core.Maybe Types.OnExitCode)
eoeOnExitCode = Lens.field @"onExitCode"
{-# DEPRECATED eoeOnExitCode "Use generic-lens or generic-optics with 'onExitCode' instead." #-}

-- | Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnReason :: Lens.Lens' EvaluateOnExit (Core.Maybe Types.OnReason)
eoeOnReason = Lens.field @"onReason"
{-# DEPRECATED eoeOnReason "Use generic-lens or generic-optics with 'onReason' instead." #-}

-- | Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnStatusReason :: Lens.Lens' EvaluateOnExit (Core.Maybe Types.OnStatusReason)
eoeOnStatusReason = Lens.field @"onStatusReason"
{-# DEPRECATED eoeOnStatusReason "Use generic-lens or generic-optics with 'onStatusReason' instead." #-}

instance Core.FromJSON EvaluateOnExit where
  toJSON EvaluateOnExit {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("action" Core..= action),
            ("onExitCode" Core..=) Core.<$> onExitCode,
            ("onReason" Core..=) Core.<$> onReason,
            ("onStatusReason" Core..=) Core.<$> onStatusReason
          ]
      )

instance Core.FromJSON EvaluateOnExit where
  parseJSON =
    Core.withObject "EvaluateOnExit" Core.$
      \x ->
        EvaluateOnExit'
          Core.<$> (x Core..: "action")
          Core.<*> (x Core..:? "onExitCode")
          Core.<*> (x Core..:? "onReason")
          Core.<*> (x Core..:? "onStatusReason")

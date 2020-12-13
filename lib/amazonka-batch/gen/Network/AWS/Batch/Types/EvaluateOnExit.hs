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
    eoeOnExitCode,
    eoeOnReason,
    eoeAction,
    eoeOnStatusReason,
  )
where

import Network.AWS.Batch.Types.RetryAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a set of conditions to be met, and an action to take (@RETRY@ or @EXIT@ ) if all conditions are met.
--
-- /See:/ 'mkEvaluateOnExit' smart constructor.
data EvaluateOnExit = EvaluateOnExit'
  { -- | Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onExitCode :: Lude.Maybe Lude.Text,
    -- | Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onReason :: Lude.Maybe Lude.Text,
    -- | Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
    action :: RetryAction,
    -- | Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
    onStatusReason :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluateOnExit' with the minimum fields required to make a request.
--
-- * 'onExitCode' - Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
-- * 'onReason' - Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
-- * 'action' - Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
-- * 'onStatusReason' - Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
mkEvaluateOnExit ::
  -- | 'action'
  RetryAction ->
  EvaluateOnExit
mkEvaluateOnExit pAction_ =
  EvaluateOnExit'
    { onExitCode = Lude.Nothing,
      onReason = Lude.Nothing,
      action = pAction_,
      onStatusReason = Lude.Nothing
    }

-- | Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onExitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnExitCode :: Lens.Lens' EvaluateOnExit (Lude.Maybe Lude.Text)
eoeOnExitCode = Lens.lens (onExitCode :: EvaluateOnExit -> Lude.Maybe Lude.Text) (\s a -> s {onExitCode = a} :: EvaluateOnExit)
{-# DEPRECATED eoeOnExitCode "Use generic-lens or generic-optics with 'onExitCode' instead." #-}

-- | Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnReason :: Lens.Lens' EvaluateOnExit (Lude.Maybe Lude.Text)
eoeOnReason = Lens.lens (onReason :: EvaluateOnExit -> Lude.Maybe Lude.Text) (\s a -> s {onReason = a} :: EvaluateOnExit)
{-# DEPRECATED eoeOnReason "Use generic-lens or generic-optics with 'onReason' instead." #-}

-- | Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeAction :: Lens.Lens' EvaluateOnExit RetryAction
eoeAction = Lens.lens (action :: EvaluateOnExit -> RetryAction) (\s a -> s {action = a} :: EvaluateOnExit)
{-# DEPRECATED eoeAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- /Note:/ Consider using 'onStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoeOnStatusReason :: Lens.Lens' EvaluateOnExit (Lude.Maybe Lude.Text)
eoeOnStatusReason = Lens.lens (onStatusReason :: EvaluateOnExit -> Lude.Maybe Lude.Text) (\s a -> s {onStatusReason = a} :: EvaluateOnExit)
{-# DEPRECATED eoeOnStatusReason "Use generic-lens or generic-optics with 'onStatusReason' instead." #-}

instance Lude.FromJSON EvaluateOnExit where
  parseJSON =
    Lude.withObject
      "EvaluateOnExit"
      ( \x ->
          EvaluateOnExit'
            Lude.<$> (x Lude..:? "onExitCode")
            Lude.<*> (x Lude..:? "onReason")
            Lude.<*> (x Lude..: "action")
            Lude.<*> (x Lude..:? "onStatusReason")
      )

instance Lude.ToJSON EvaluateOnExit where
  toJSON EvaluateOnExit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("onExitCode" Lude..=) Lude.<$> onExitCode,
            ("onReason" Lude..=) Lude.<$> onReason,
            Lude.Just ("action" Lude..= action),
            ("onStatusReason" Lude..=) Lude.<$> onStatusReason
          ]
      )

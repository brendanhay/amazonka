{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.EvaluateOnExit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.EvaluateOnExit where

import Network.AWS.Batch.Types.RetryAction
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a set of conditions to be met, and an action to take (@RETRY@
-- or @EXIT@) if all conditions are met.
--
-- /See:/ 'newEvaluateOnExit' smart constructor.
data EvaluateOnExit = EvaluateOnExit'
  { -- | Contains a glob pattern to match against the decimal representation of
    -- the @ExitCode@ returned for a job. The pattern can be up to 512
    -- characters in length. It can contain only numbers, and can optionally
    -- end with an asterisk (*) so that only the start of the string needs to
    -- be an exact match.
    onExitCode :: Prelude.Maybe Prelude.Text,
    -- | Contains a glob pattern to match against the @Reason@ returned for a
    -- job. The pattern can be up to 512 characters in length. It can contain
    -- letters, numbers, periods (.), colons (:), and white space (including
    -- spaces and tabs). It can optionally end with an asterisk (*) so that
    -- only the start of the string needs to be an exact match.
    onReason :: Prelude.Maybe Prelude.Text,
    -- | Contains a glob pattern to match against the @StatusReason@ returned for
    -- a job. The pattern can be up to 512 characters in length. It can contain
    -- letters, numbers, periods (.), colons (:), and white space (including
    -- spaces or tabs). It can optionally end with an asterisk (*) so that only
    -- the start of the string needs to be an exact match.
    onStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Specifies the action to take if all of the specified conditions
    -- (@onStatusReason@, @onReason@, and @onExitCode@) are met. The values
    -- aren\'t case sensitive.
    action :: RetryAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateOnExit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onExitCode', 'evaluateOnExit_onExitCode' - Contains a glob pattern to match against the decimal representation of
-- the @ExitCode@ returned for a job. The pattern can be up to 512
-- characters in length. It can contain only numbers, and can optionally
-- end with an asterisk (*) so that only the start of the string needs to
-- be an exact match.
--
-- 'onReason', 'evaluateOnExit_onReason' - Contains a glob pattern to match against the @Reason@ returned for a
-- job. The pattern can be up to 512 characters in length. It can contain
-- letters, numbers, periods (.), colons (:), and white space (including
-- spaces and tabs). It can optionally end with an asterisk (*) so that
-- only the start of the string needs to be an exact match.
--
-- 'onStatusReason', 'evaluateOnExit_onStatusReason' - Contains a glob pattern to match against the @StatusReason@ returned for
-- a job. The pattern can be up to 512 characters in length. It can contain
-- letters, numbers, periods (.), colons (:), and white space (including
-- spaces or tabs). It can optionally end with an asterisk (*) so that only
-- the start of the string needs to be an exact match.
--
-- 'action', 'evaluateOnExit_action' - Specifies the action to take if all of the specified conditions
-- (@onStatusReason@, @onReason@, and @onExitCode@) are met. The values
-- aren\'t case sensitive.
newEvaluateOnExit ::
  -- | 'action'
  RetryAction ->
  EvaluateOnExit
newEvaluateOnExit pAction_ =
  EvaluateOnExit'
    { onExitCode = Prelude.Nothing,
      onReason = Prelude.Nothing,
      onStatusReason = Prelude.Nothing,
      action = pAction_
    }

-- | Contains a glob pattern to match against the decimal representation of
-- the @ExitCode@ returned for a job. The pattern can be up to 512
-- characters in length. It can contain only numbers, and can optionally
-- end with an asterisk (*) so that only the start of the string needs to
-- be an exact match.
evaluateOnExit_onExitCode :: Lens.Lens' EvaluateOnExit (Prelude.Maybe Prelude.Text)
evaluateOnExit_onExitCode = Lens.lens (\EvaluateOnExit' {onExitCode} -> onExitCode) (\s@EvaluateOnExit' {} a -> s {onExitCode = a} :: EvaluateOnExit)

-- | Contains a glob pattern to match against the @Reason@ returned for a
-- job. The pattern can be up to 512 characters in length. It can contain
-- letters, numbers, periods (.), colons (:), and white space (including
-- spaces and tabs). It can optionally end with an asterisk (*) so that
-- only the start of the string needs to be an exact match.
evaluateOnExit_onReason :: Lens.Lens' EvaluateOnExit (Prelude.Maybe Prelude.Text)
evaluateOnExit_onReason = Lens.lens (\EvaluateOnExit' {onReason} -> onReason) (\s@EvaluateOnExit' {} a -> s {onReason = a} :: EvaluateOnExit)

-- | Contains a glob pattern to match against the @StatusReason@ returned for
-- a job. The pattern can be up to 512 characters in length. It can contain
-- letters, numbers, periods (.), colons (:), and white space (including
-- spaces or tabs). It can optionally end with an asterisk (*) so that only
-- the start of the string needs to be an exact match.
evaluateOnExit_onStatusReason :: Lens.Lens' EvaluateOnExit (Prelude.Maybe Prelude.Text)
evaluateOnExit_onStatusReason = Lens.lens (\EvaluateOnExit' {onStatusReason} -> onStatusReason) (\s@EvaluateOnExit' {} a -> s {onStatusReason = a} :: EvaluateOnExit)

-- | Specifies the action to take if all of the specified conditions
-- (@onStatusReason@, @onReason@, and @onExitCode@) are met. The values
-- aren\'t case sensitive.
evaluateOnExit_action :: Lens.Lens' EvaluateOnExit RetryAction
evaluateOnExit_action = Lens.lens (\EvaluateOnExit' {action} -> action) (\s@EvaluateOnExit' {} a -> s {action = a} :: EvaluateOnExit)

instance Core.FromJSON EvaluateOnExit where
  parseJSON =
    Core.withObject
      "EvaluateOnExit"
      ( \x ->
          EvaluateOnExit'
            Prelude.<$> (x Core..:? "onExitCode")
            Prelude.<*> (x Core..:? "onReason")
            Prelude.<*> (x Core..:? "onStatusReason")
            Prelude.<*> (x Core..: "action")
      )

instance Prelude.Hashable EvaluateOnExit

instance Prelude.NFData EvaluateOnExit

instance Core.ToJSON EvaluateOnExit where
  toJSON EvaluateOnExit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("onExitCode" Core..=) Prelude.<$> onExitCode,
            ("onReason" Core..=) Prelude.<$> onReason,
            ("onStatusReason" Core..=)
              Prelude.<$> onStatusReason,
            Prelude.Just ("action" Core..= action)
          ]
      )

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
-- Module      : Amazonka.RBin.Types.RuleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.RuleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.LockState
import Amazonka.RBin.Types.RetentionPeriod

-- | Information about a Recycle Bin retention rule.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | The lock state for the retention rule.
    --
    -- -   @locked@ - The retention rule is locked and can\'t be modified or
    --     deleted.
    --
    -- -   @pending_unlock@ - The retention rule has been unlocked but it is
    --     still within the unlock delay period. The retention rule can be
    --     modified or deleted only after the unlock delay period has expired.
    --
    -- -   @unlocked@ - The retention rule is unlocked and it can be modified
    --     or deleted by any user with the required permissions.
    --
    -- -   @null@ - The retention rule has never been locked. Once a retention
    --     rule has been locked, it can transition between the @locked@ and
    --     @unlocked@ states only; it can never transition back to @null@.
    lockState :: Prelude.Maybe LockState,
    -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the retention period for which the retention rule is
    -- to retain resources.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockState', 'ruleSummary_lockState' - The lock state for the retention rule.
--
-- -   @locked@ - The retention rule is locked and can\'t be modified or
--     deleted.
--
-- -   @pending_unlock@ - The retention rule has been unlocked but it is
--     still within the unlock delay period. The retention rule can be
--     modified or deleted only after the unlock delay period has expired.
--
-- -   @unlocked@ - The retention rule is unlocked and it can be modified
--     or deleted by any user with the required permissions.
--
-- -   @null@ - The retention rule has never been locked. Once a retention
--     rule has been locked, it can transition between the @locked@ and
--     @unlocked@ states only; it can never transition back to @null@.
--
-- 'description', 'ruleSummary_description' - The retention rule description.
--
-- 'retentionPeriod', 'ruleSummary_retentionPeriod' - Information about the retention period for which the retention rule is
-- to retain resources.
--
-- 'identifier', 'ruleSummary_identifier' - The unique ID of the retention rule.
newRuleSummary ::
  RuleSummary
newRuleSummary =
  RuleSummary'
    { lockState = Prelude.Nothing,
      description = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | The lock state for the retention rule.
--
-- -   @locked@ - The retention rule is locked and can\'t be modified or
--     deleted.
--
-- -   @pending_unlock@ - The retention rule has been unlocked but it is
--     still within the unlock delay period. The retention rule can be
--     modified or deleted only after the unlock delay period has expired.
--
-- -   @unlocked@ - The retention rule is unlocked and it can be modified
--     or deleted by any user with the required permissions.
--
-- -   @null@ - The retention rule has never been locked. Once a retention
--     rule has been locked, it can transition between the @locked@ and
--     @unlocked@ states only; it can never transition back to @null@.
ruleSummary_lockState :: Lens.Lens' RuleSummary (Prelude.Maybe LockState)
ruleSummary_lockState = Lens.lens (\RuleSummary' {lockState} -> lockState) (\s@RuleSummary' {} a -> s {lockState = a} :: RuleSummary)

-- | The retention rule description.
ruleSummary_description :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_description = Lens.lens (\RuleSummary' {description} -> description) (\s@RuleSummary' {} a -> s {description = a} :: RuleSummary)

-- | Information about the retention period for which the retention rule is
-- to retain resources.
ruleSummary_retentionPeriod :: Lens.Lens' RuleSummary (Prelude.Maybe RetentionPeriod)
ruleSummary_retentionPeriod = Lens.lens (\RuleSummary' {retentionPeriod} -> retentionPeriod) (\s@RuleSummary' {} a -> s {retentionPeriod = a} :: RuleSummary)

-- | The unique ID of the retention rule.
ruleSummary_identifier :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_identifier = Lens.lens (\RuleSummary' {identifier} -> identifier) (\s@RuleSummary' {} a -> s {identifier = a} :: RuleSummary)

instance Core.FromJSON RuleSummary where
  parseJSON =
    Core.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Core..:? "LockState")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RetentionPeriod")
            Prelude.<*> (x Core..:? "Identifier")
      )

instance Prelude.Hashable RuleSummary where
  hashWithSalt _salt RuleSummary' {..} =
    _salt `Prelude.hashWithSalt` lockState
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData RuleSummary where
  rnf RuleSummary' {..} =
    Prelude.rnf lockState
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf identifier

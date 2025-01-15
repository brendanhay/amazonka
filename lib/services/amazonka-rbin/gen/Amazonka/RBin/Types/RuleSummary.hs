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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.RuleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.LockState
import Amazonka.RBin.Types.RetentionPeriod

-- | Information about a Recycle Bin retention rule.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text,
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
    lockState :: Prelude.Maybe LockState,
    -- | Information about the retention period for which the retention rule is
    -- to retain resources.
    retentionPeriod :: Prelude.Maybe RetentionPeriod
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
-- 'description', 'ruleSummary_description' - The retention rule description.
--
-- 'identifier', 'ruleSummary_identifier' - The unique ID of the retention rule.
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
-- 'retentionPeriod', 'ruleSummary_retentionPeriod' - Information about the retention period for which the retention rule is
-- to retain resources.
newRuleSummary ::
  RuleSummary
newRuleSummary =
  RuleSummary'
    { description = Prelude.Nothing,
      identifier = Prelude.Nothing,
      lockState = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing
    }

-- | The retention rule description.
ruleSummary_description :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_description = Lens.lens (\RuleSummary' {description} -> description) (\s@RuleSummary' {} a -> s {description = a} :: RuleSummary)

-- | The unique ID of the retention rule.
ruleSummary_identifier :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_identifier = Lens.lens (\RuleSummary' {identifier} -> identifier) (\s@RuleSummary' {} a -> s {identifier = a} :: RuleSummary)

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

-- | Information about the retention period for which the retention rule is
-- to retain resources.
ruleSummary_retentionPeriod :: Lens.Lens' RuleSummary (Prelude.Maybe RetentionPeriod)
ruleSummary_retentionPeriod = Lens.lens (\RuleSummary' {retentionPeriod} -> retentionPeriod) (\s@RuleSummary' {} a -> s {retentionPeriod = a} :: RuleSummary)

instance Data.FromJSON RuleSummary where
  parseJSON =
    Data.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Identifier")
            Prelude.<*> (x Data..:? "LockState")
            Prelude.<*> (x Data..:? "RetentionPeriod")
      )

instance Prelude.Hashable RuleSummary where
  hashWithSalt _salt RuleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` lockState
      `Prelude.hashWithSalt` retentionPeriod

instance Prelude.NFData RuleSummary where
  rnf RuleSummary' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf identifier `Prelude.seq`
        Prelude.rnf lockState `Prelude.seq`
          Prelude.rnf retentionPeriod

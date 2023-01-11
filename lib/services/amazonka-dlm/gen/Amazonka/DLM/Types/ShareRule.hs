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
-- Module      : Amazonka.DLM.Types.ShareRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.ShareRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot policies only]__ Specifies a rule for sharing snapshots
-- across Amazon Web Services accounts.
--
-- /See:/ 'newShareRule' smart constructor.
data ShareRule = ShareRule'
  { -- | The period after which snapshots that are shared with other Amazon Web
    -- Services accounts are automatically unshared.
    unshareInterval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time for the automatic unsharing interval.
    unshareIntervalUnit :: Prelude.Maybe RetentionIntervalUnitValues,
    -- | The IDs of the Amazon Web Services accounts with which to share the
    -- snapshots.
    targetAccounts :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unshareInterval', 'shareRule_unshareInterval' - The period after which snapshots that are shared with other Amazon Web
-- Services accounts are automatically unshared.
--
-- 'unshareIntervalUnit', 'shareRule_unshareIntervalUnit' - The unit of time for the automatic unsharing interval.
--
-- 'targetAccounts', 'shareRule_targetAccounts' - The IDs of the Amazon Web Services accounts with which to share the
-- snapshots.
newShareRule ::
  -- | 'targetAccounts'
  Prelude.NonEmpty Prelude.Text ->
  ShareRule
newShareRule pTargetAccounts_ =
  ShareRule'
    { unshareInterval = Prelude.Nothing,
      unshareIntervalUnit = Prelude.Nothing,
      targetAccounts =
        Lens.coerced Lens.# pTargetAccounts_
    }

-- | The period after which snapshots that are shared with other Amazon Web
-- Services accounts are automatically unshared.
shareRule_unshareInterval :: Lens.Lens' ShareRule (Prelude.Maybe Prelude.Natural)
shareRule_unshareInterval = Lens.lens (\ShareRule' {unshareInterval} -> unshareInterval) (\s@ShareRule' {} a -> s {unshareInterval = a} :: ShareRule)

-- | The unit of time for the automatic unsharing interval.
shareRule_unshareIntervalUnit :: Lens.Lens' ShareRule (Prelude.Maybe RetentionIntervalUnitValues)
shareRule_unshareIntervalUnit = Lens.lens (\ShareRule' {unshareIntervalUnit} -> unshareIntervalUnit) (\s@ShareRule' {} a -> s {unshareIntervalUnit = a} :: ShareRule)

-- | The IDs of the Amazon Web Services accounts with which to share the
-- snapshots.
shareRule_targetAccounts :: Lens.Lens' ShareRule (Prelude.NonEmpty Prelude.Text)
shareRule_targetAccounts = Lens.lens (\ShareRule' {targetAccounts} -> targetAccounts) (\s@ShareRule' {} a -> s {targetAccounts = a} :: ShareRule) Prelude.. Lens.coerced

instance Data.FromJSON ShareRule where
  parseJSON =
    Data.withObject
      "ShareRule"
      ( \x ->
          ShareRule'
            Prelude.<$> (x Data..:? "UnshareInterval")
            Prelude.<*> (x Data..:? "UnshareIntervalUnit")
            Prelude.<*> (x Data..: "TargetAccounts")
      )

instance Prelude.Hashable ShareRule where
  hashWithSalt _salt ShareRule' {..} =
    _salt `Prelude.hashWithSalt` unshareInterval
      `Prelude.hashWithSalt` unshareIntervalUnit
      `Prelude.hashWithSalt` targetAccounts

instance Prelude.NFData ShareRule where
  rnf ShareRule' {..} =
    Prelude.rnf unshareInterval
      `Prelude.seq` Prelude.rnf unshareIntervalUnit
      `Prelude.seq` Prelude.rnf targetAccounts

instance Data.ToJSON ShareRule where
  toJSON ShareRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UnshareInterval" Data..=)
              Prelude.<$> unshareInterval,
            ("UnshareIntervalUnit" Data..=)
              Prelude.<$> unshareIntervalUnit,
            Prelude.Just
              ("TargetAccounts" Data..= targetAccounts)
          ]
      )

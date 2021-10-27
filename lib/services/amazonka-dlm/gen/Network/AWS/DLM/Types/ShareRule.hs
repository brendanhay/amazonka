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
-- Module      : Network.AWS.DLM.Types.ShareRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types.ShareRule where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types.RetentionIntervalUnitValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a rule for sharing snapshots across Amazon Web Services
-- accounts.
--
-- /See:/ 'newShareRule' smart constructor.
data ShareRule = ShareRule'
  { -- | The unit of time for the automatic unsharing interval.
    unshareIntervalUnit :: Prelude.Maybe RetentionIntervalUnitValues,
    -- | The period after which snapshots that are shared with other Amazon Web
    -- Services accounts are automatically unshared.
    unshareInterval :: Prelude.Maybe Prelude.Natural,
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
-- 'unshareIntervalUnit', 'shareRule_unshareIntervalUnit' - The unit of time for the automatic unsharing interval.
--
-- 'unshareInterval', 'shareRule_unshareInterval' - The period after which snapshots that are shared with other Amazon Web
-- Services accounts are automatically unshared.
--
-- 'targetAccounts', 'shareRule_targetAccounts' - The IDs of the Amazon Web Services accounts with which to share the
-- snapshots.
newShareRule ::
  -- | 'targetAccounts'
  Prelude.NonEmpty Prelude.Text ->
  ShareRule
newShareRule pTargetAccounts_ =
  ShareRule'
    { unshareIntervalUnit = Prelude.Nothing,
      unshareInterval = Prelude.Nothing,
      targetAccounts =
        Lens.coerced Lens.# pTargetAccounts_
    }

-- | The unit of time for the automatic unsharing interval.
shareRule_unshareIntervalUnit :: Lens.Lens' ShareRule (Prelude.Maybe RetentionIntervalUnitValues)
shareRule_unshareIntervalUnit = Lens.lens (\ShareRule' {unshareIntervalUnit} -> unshareIntervalUnit) (\s@ShareRule' {} a -> s {unshareIntervalUnit = a} :: ShareRule)

-- | The period after which snapshots that are shared with other Amazon Web
-- Services accounts are automatically unshared.
shareRule_unshareInterval :: Lens.Lens' ShareRule (Prelude.Maybe Prelude.Natural)
shareRule_unshareInterval = Lens.lens (\ShareRule' {unshareInterval} -> unshareInterval) (\s@ShareRule' {} a -> s {unshareInterval = a} :: ShareRule)

-- | The IDs of the Amazon Web Services accounts with which to share the
-- snapshots.
shareRule_targetAccounts :: Lens.Lens' ShareRule (Prelude.NonEmpty Prelude.Text)
shareRule_targetAccounts = Lens.lens (\ShareRule' {targetAccounts} -> targetAccounts) (\s@ShareRule' {} a -> s {targetAccounts = a} :: ShareRule) Prelude.. Lens.coerced

instance Core.FromJSON ShareRule where
  parseJSON =
    Core.withObject
      "ShareRule"
      ( \x ->
          ShareRule'
            Prelude.<$> (x Core..:? "UnshareIntervalUnit")
            Prelude.<*> (x Core..:? "UnshareInterval")
            Prelude.<*> (x Core..: "TargetAccounts")
      )

instance Prelude.Hashable ShareRule

instance Prelude.NFData ShareRule

instance Core.ToJSON ShareRule where
  toJSON ShareRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UnshareIntervalUnit" Core..=)
              Prelude.<$> unshareIntervalUnit,
            ("UnshareInterval" Core..=)
              Prelude.<$> unshareInterval,
            Prelude.Just
              ("TargetAccounts" Core..= targetAccounts)
          ]
      )

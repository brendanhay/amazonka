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
-- Module      : Amazonka.ElasticSearch.Types.UpgradeStepItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.UpgradeStepItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticSearch.Types.UpgradeStatus
import Amazonka.ElasticSearch.Types.UpgradeStep
import qualified Amazonka.Prelude as Prelude

-- | Represents a single step of the Upgrade or Upgrade Eligibility Check
-- workflow.
--
-- /See:/ 'newUpgradeStepItem' smart constructor.
data UpgradeStepItem = UpgradeStepItem'
  { -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
    -- does through:
    --
    -- -   PreUpgradeCheck
    -- -   Snapshot
    -- -   Upgrade
    upgradeStep :: Prelude.Maybe UpgradeStep,
    -- | A list of strings containing detailed information about the errors
    -- encountered in a particular step.
    issues :: Prelude.Maybe [Prelude.Text],
    -- | The Floating point value representing progress percentage of a
    -- particular step.
    progressPercent :: Prelude.Maybe Prelude.Double,
    -- | The status of a particular step during an upgrade. The status can take
    -- one of the following values:
    --
    -- -   In Progress
    -- -   Succeeded
    -- -   Succeeded with Issues
    -- -   Failed
    upgradeStepStatus :: Prelude.Maybe UpgradeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeStepItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upgradeStep', 'upgradeStepItem_upgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
--
-- 'issues', 'upgradeStepItem_issues' - A list of strings containing detailed information about the errors
-- encountered in a particular step.
--
-- 'progressPercent', 'upgradeStepItem_progressPercent' - The Floating point value representing progress percentage of a
-- particular step.
--
-- 'upgradeStepStatus', 'upgradeStepItem_upgradeStepStatus' - The status of a particular step during an upgrade. The status can take
-- one of the following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
newUpgradeStepItem ::
  UpgradeStepItem
newUpgradeStepItem =
  UpgradeStepItem'
    { upgradeStep = Prelude.Nothing,
      issues = Prelude.Nothing,
      progressPercent = Prelude.Nothing,
      upgradeStepStatus = Prelude.Nothing
    }

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
upgradeStepItem_upgradeStep :: Lens.Lens' UpgradeStepItem (Prelude.Maybe UpgradeStep)
upgradeStepItem_upgradeStep = Lens.lens (\UpgradeStepItem' {upgradeStep} -> upgradeStep) (\s@UpgradeStepItem' {} a -> s {upgradeStep = a} :: UpgradeStepItem)

-- | A list of strings containing detailed information about the errors
-- encountered in a particular step.
upgradeStepItem_issues :: Lens.Lens' UpgradeStepItem (Prelude.Maybe [Prelude.Text])
upgradeStepItem_issues = Lens.lens (\UpgradeStepItem' {issues} -> issues) (\s@UpgradeStepItem' {} a -> s {issues = a} :: UpgradeStepItem) Prelude.. Lens.mapping Lens.coerced

-- | The Floating point value representing progress percentage of a
-- particular step.
upgradeStepItem_progressPercent :: Lens.Lens' UpgradeStepItem (Prelude.Maybe Prelude.Double)
upgradeStepItem_progressPercent = Lens.lens (\UpgradeStepItem' {progressPercent} -> progressPercent) (\s@UpgradeStepItem' {} a -> s {progressPercent = a} :: UpgradeStepItem)

-- | The status of a particular step during an upgrade. The status can take
-- one of the following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
upgradeStepItem_upgradeStepStatus :: Lens.Lens' UpgradeStepItem (Prelude.Maybe UpgradeStatus)
upgradeStepItem_upgradeStepStatus = Lens.lens (\UpgradeStepItem' {upgradeStepStatus} -> upgradeStepStatus) (\s@UpgradeStepItem' {} a -> s {upgradeStepStatus = a} :: UpgradeStepItem)

instance Core.FromJSON UpgradeStepItem where
  parseJSON =
    Core.withObject
      "UpgradeStepItem"
      ( \x ->
          UpgradeStepItem'
            Prelude.<$> (x Core..:? "UpgradeStep")
            Prelude.<*> (x Core..:? "Issues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProgressPercent")
            Prelude.<*> (x Core..:? "UpgradeStepStatus")
      )

instance Prelude.Hashable UpgradeStepItem where
  hashWithSalt _salt UpgradeStepItem' {..} =
    _salt `Prelude.hashWithSalt` upgradeStep
      `Prelude.hashWithSalt` issues
      `Prelude.hashWithSalt` progressPercent
      `Prelude.hashWithSalt` upgradeStepStatus

instance Prelude.NFData UpgradeStepItem where
  rnf UpgradeStepItem' {..} =
    Prelude.rnf upgradeStep
      `Prelude.seq` Prelude.rnf issues
      `Prelude.seq` Prelude.rnf progressPercent
      `Prelude.seq` Prelude.rnf upgradeStepStatus

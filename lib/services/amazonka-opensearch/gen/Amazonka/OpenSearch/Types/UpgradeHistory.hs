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
-- Module      : Amazonka.OpenSearch.Types.UpgradeHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.UpgradeHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.UpgradeStatus
import Amazonka.OpenSearch.Types.UpgradeStepItem
import qualified Amazonka.Prelude as Prelude

-- | History of the last 10 upgrades and upgrade eligibility checks for an
-- Amazon OpenSearch Service domain.
--
-- /See:/ 'newUpgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { -- | UTC timestamp at which the upgrade API call was made, in the format
    -- @yyyy-MM-ddTHH:mm:ssZ@.
    startTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of each step performed as part of a specific upgrade or upgrade
    -- eligibility check.
    stepsList :: Prelude.Maybe [UpgradeStepItem],
    -- | A string that describes the upgrade.
    upgradeName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the upgrade. The status can take one of the
    -- following values:
    --
    -- -   In Progress
    --
    -- -   Succeeded
    --
    -- -   Succeeded with Issues
    --
    -- -   Failed
    upgradeStatus :: Prelude.Maybe UpgradeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimestamp', 'upgradeHistory_startTimestamp' - UTC timestamp at which the upgrade API call was made, in the format
-- @yyyy-MM-ddTHH:mm:ssZ@.
--
-- 'stepsList', 'upgradeHistory_stepsList' - A list of each step performed as part of a specific upgrade or upgrade
-- eligibility check.
--
-- 'upgradeName', 'upgradeHistory_upgradeName' - A string that describes the upgrade.
--
-- 'upgradeStatus', 'upgradeHistory_upgradeStatus' - The current status of the upgrade. The status can take one of the
-- following values:
--
-- -   In Progress
--
-- -   Succeeded
--
-- -   Succeeded with Issues
--
-- -   Failed
newUpgradeHistory ::
  UpgradeHistory
newUpgradeHistory =
  UpgradeHistory'
    { startTimestamp = Prelude.Nothing,
      stepsList = Prelude.Nothing,
      upgradeName = Prelude.Nothing,
      upgradeStatus = Prelude.Nothing
    }

-- | UTC timestamp at which the upgrade API call was made, in the format
-- @yyyy-MM-ddTHH:mm:ssZ@.
upgradeHistory_startTimestamp :: Lens.Lens' UpgradeHistory (Prelude.Maybe Prelude.UTCTime)
upgradeHistory_startTimestamp = Lens.lens (\UpgradeHistory' {startTimestamp} -> startTimestamp) (\s@UpgradeHistory' {} a -> s {startTimestamp = a} :: UpgradeHistory) Prelude.. Lens.mapping Data._Time

-- | A list of each step performed as part of a specific upgrade or upgrade
-- eligibility check.
upgradeHistory_stepsList :: Lens.Lens' UpgradeHistory (Prelude.Maybe [UpgradeStepItem])
upgradeHistory_stepsList = Lens.lens (\UpgradeHistory' {stepsList} -> stepsList) (\s@UpgradeHistory' {} a -> s {stepsList = a} :: UpgradeHistory) Prelude.. Lens.mapping Lens.coerced

-- | A string that describes the upgrade.
upgradeHistory_upgradeName :: Lens.Lens' UpgradeHistory (Prelude.Maybe Prelude.Text)
upgradeHistory_upgradeName = Lens.lens (\UpgradeHistory' {upgradeName} -> upgradeName) (\s@UpgradeHistory' {} a -> s {upgradeName = a} :: UpgradeHistory)

-- | The current status of the upgrade. The status can take one of the
-- following values:
--
-- -   In Progress
--
-- -   Succeeded
--
-- -   Succeeded with Issues
--
-- -   Failed
upgradeHistory_upgradeStatus :: Lens.Lens' UpgradeHistory (Prelude.Maybe UpgradeStatus)
upgradeHistory_upgradeStatus = Lens.lens (\UpgradeHistory' {upgradeStatus} -> upgradeStatus) (\s@UpgradeHistory' {} a -> s {upgradeStatus = a} :: UpgradeHistory)

instance Data.FromJSON UpgradeHistory where
  parseJSON =
    Data.withObject
      "UpgradeHistory"
      ( \x ->
          UpgradeHistory'
            Prelude.<$> (x Data..:? "StartTimestamp")
            Prelude.<*> (x Data..:? "StepsList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpgradeName")
            Prelude.<*> (x Data..:? "UpgradeStatus")
      )

instance Prelude.Hashable UpgradeHistory where
  hashWithSalt _salt UpgradeHistory' {..} =
    _salt
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` stepsList
      `Prelude.hashWithSalt` upgradeName
      `Prelude.hashWithSalt` upgradeStatus

instance Prelude.NFData UpgradeHistory where
  rnf UpgradeHistory' {..} =
    Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf stepsList
      `Prelude.seq` Prelude.rnf upgradeName
      `Prelude.seq` Prelude.rnf upgradeStatus

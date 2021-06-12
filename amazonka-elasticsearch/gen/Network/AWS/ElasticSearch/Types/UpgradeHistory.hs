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
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeHistory where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import qualified Network.AWS.Lens as Lens

-- | History of the last 10 Upgrades and Upgrade Eligibility Checks.
--
-- /See:/ 'newUpgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { -- | A string that describes the update briefly
    upgradeName :: Core.Maybe Core.Text,
    -- | UTC Timestamp at which the Upgrade API call was made in
    -- \"yyyy-MM-ddTHH:mm:ssZ\" format.
    startTimestamp :: Core.Maybe Core.POSIX,
    -- | The overall status of the update. The status can take one of the
    -- following values:
    --
    -- -   In Progress
    -- -   Succeeded
    -- -   Succeeded with Issues
    -- -   Failed
    upgradeStatus :: Core.Maybe UpgradeStatus,
    -- | A list of @ UpgradeStepItem @ s representing information about each step
    -- performed as pard of a specific Upgrade or Upgrade Eligibility Check.
    stepsList :: Core.Maybe [UpgradeStepItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upgradeName', 'upgradeHistory_upgradeName' - A string that describes the update briefly
--
-- 'startTimestamp', 'upgradeHistory_startTimestamp' - UTC Timestamp at which the Upgrade API call was made in
-- \"yyyy-MM-ddTHH:mm:ssZ\" format.
--
-- 'upgradeStatus', 'upgradeHistory_upgradeStatus' - The overall status of the update. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
--
-- 'stepsList', 'upgradeHistory_stepsList' - A list of @ UpgradeStepItem @ s representing information about each step
-- performed as pard of a specific Upgrade or Upgrade Eligibility Check.
newUpgradeHistory ::
  UpgradeHistory
newUpgradeHistory =
  UpgradeHistory'
    { upgradeName = Core.Nothing,
      startTimestamp = Core.Nothing,
      upgradeStatus = Core.Nothing,
      stepsList = Core.Nothing
    }

-- | A string that describes the update briefly
upgradeHistory_upgradeName :: Lens.Lens' UpgradeHistory (Core.Maybe Core.Text)
upgradeHistory_upgradeName = Lens.lens (\UpgradeHistory' {upgradeName} -> upgradeName) (\s@UpgradeHistory' {} a -> s {upgradeName = a} :: UpgradeHistory)

-- | UTC Timestamp at which the Upgrade API call was made in
-- \"yyyy-MM-ddTHH:mm:ssZ\" format.
upgradeHistory_startTimestamp :: Lens.Lens' UpgradeHistory (Core.Maybe Core.UTCTime)
upgradeHistory_startTimestamp = Lens.lens (\UpgradeHistory' {startTimestamp} -> startTimestamp) (\s@UpgradeHistory' {} a -> s {startTimestamp = a} :: UpgradeHistory) Core.. Lens.mapping Core._Time

-- | The overall status of the update. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
upgradeHistory_upgradeStatus :: Lens.Lens' UpgradeHistory (Core.Maybe UpgradeStatus)
upgradeHistory_upgradeStatus = Lens.lens (\UpgradeHistory' {upgradeStatus} -> upgradeStatus) (\s@UpgradeHistory' {} a -> s {upgradeStatus = a} :: UpgradeHistory)

-- | A list of @ UpgradeStepItem @ s representing information about each step
-- performed as pard of a specific Upgrade or Upgrade Eligibility Check.
upgradeHistory_stepsList :: Lens.Lens' UpgradeHistory (Core.Maybe [UpgradeStepItem])
upgradeHistory_stepsList = Lens.lens (\UpgradeHistory' {stepsList} -> stepsList) (\s@UpgradeHistory' {} a -> s {stepsList = a} :: UpgradeHistory) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON UpgradeHistory where
  parseJSON =
    Core.withObject
      "UpgradeHistory"
      ( \x ->
          UpgradeHistory'
            Core.<$> (x Core..:? "UpgradeName")
            Core.<*> (x Core..:? "StartTimestamp")
            Core.<*> (x Core..:? "UpgradeStatus")
            Core.<*> (x Core..:? "StepsList" Core..!= Core.mempty)
      )

instance Core.Hashable UpgradeHistory

instance Core.NFData UpgradeHistory

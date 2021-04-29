{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | History of the last 10 Upgrades and Upgrade Eligibility Checks.
--
-- /See:/ 'newUpgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { -- | A string that describes the update briefly
    upgradeName :: Prelude.Maybe Prelude.Text,
    -- | UTC Timestamp at which the Upgrade API call was made in
    -- \"yyyy-MM-ddTHH:mm:ssZ\" format.
    startTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The overall status of the update. The status can take one of the
    -- following values:
    --
    -- -   In Progress
    -- -   Succeeded
    -- -   Succeeded with Issues
    -- -   Failed
    upgradeStatus :: Prelude.Maybe UpgradeStatus,
    -- | A list of @ UpgradeStepItem @ s representing information about each step
    -- performed as pard of a specific Upgrade or Upgrade Eligibility Check.
    stepsList :: Prelude.Maybe [UpgradeStepItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { upgradeName = Prelude.Nothing,
      startTimestamp = Prelude.Nothing,
      upgradeStatus = Prelude.Nothing,
      stepsList = Prelude.Nothing
    }

-- | A string that describes the update briefly
upgradeHistory_upgradeName :: Lens.Lens' UpgradeHistory (Prelude.Maybe Prelude.Text)
upgradeHistory_upgradeName = Lens.lens (\UpgradeHistory' {upgradeName} -> upgradeName) (\s@UpgradeHistory' {} a -> s {upgradeName = a} :: UpgradeHistory)

-- | UTC Timestamp at which the Upgrade API call was made in
-- \"yyyy-MM-ddTHH:mm:ssZ\" format.
upgradeHistory_startTimestamp :: Lens.Lens' UpgradeHistory (Prelude.Maybe Prelude.UTCTime)
upgradeHistory_startTimestamp = Lens.lens (\UpgradeHistory' {startTimestamp} -> startTimestamp) (\s@UpgradeHistory' {} a -> s {startTimestamp = a} :: UpgradeHistory) Prelude.. Lens.mapping Prelude._Time

-- | The overall status of the update. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
upgradeHistory_upgradeStatus :: Lens.Lens' UpgradeHistory (Prelude.Maybe UpgradeStatus)
upgradeHistory_upgradeStatus = Lens.lens (\UpgradeHistory' {upgradeStatus} -> upgradeStatus) (\s@UpgradeHistory' {} a -> s {upgradeStatus = a} :: UpgradeHistory)

-- | A list of @ UpgradeStepItem @ s representing information about each step
-- performed as pard of a specific Upgrade or Upgrade Eligibility Check.
upgradeHistory_stepsList :: Lens.Lens' UpgradeHistory (Prelude.Maybe [UpgradeStepItem])
upgradeHistory_stepsList = Lens.lens (\UpgradeHistory' {stepsList} -> stepsList) (\s@UpgradeHistory' {} a -> s {stepsList = a} :: UpgradeHistory) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON UpgradeHistory where
  parseJSON =
    Prelude.withObject
      "UpgradeHistory"
      ( \x ->
          UpgradeHistory'
            Prelude.<$> (x Prelude..:? "UpgradeName")
            Prelude.<*> (x Prelude..:? "StartTimestamp")
            Prelude.<*> (x Prelude..:? "UpgradeStatus")
            Prelude.<*> ( x Prelude..:? "StepsList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable UpgradeHistory

instance Prelude.NFData UpgradeHistory

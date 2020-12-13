{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeHistory
  ( UpgradeHistory (..),

    -- * Smart constructor
    mkUpgradeHistory,

    -- * Lenses
    uhUpgradeStatus,
    uhStepsList,
    uhUpgradeName,
    uhStartTimestamp,
  )
where

import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | History of the last 10 Upgrades and Upgrade Eligibility Checks.
--
-- /See:/ 'mkUpgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { -- | The overall status of the update. The status can take one of the following values:
    --
    --     * In Progress
    --
    --     * Succeeded
    --
    --     * Succeeded with Issues
    --
    --     * Failed
    upgradeStatus :: Lude.Maybe UpgradeStatus,
    -- | A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check.
    stepsList :: Lude.Maybe [UpgradeStepItem],
    -- | A string that describes the update briefly
    upgradeName :: Lude.Maybe Lude.Text,
    -- | UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
    startTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeHistory' with the minimum fields required to make a request.
--
-- * 'upgradeStatus' - The overall status of the update. The status can take one of the following values:
--
--     * In Progress
--
--     * Succeeded
--
--     * Succeeded with Issues
--
--     * Failed
--
--
-- * 'stepsList' - A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check.
-- * 'upgradeName' - A string that describes the update briefly
-- * 'startTimestamp' - UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
mkUpgradeHistory ::
  UpgradeHistory
mkUpgradeHistory =
  UpgradeHistory'
    { upgradeStatus = Lude.Nothing,
      stepsList = Lude.Nothing,
      upgradeName = Lude.Nothing,
      startTimestamp = Lude.Nothing
    }

-- | The overall status of the update. The status can take one of the following values:
--
--     * In Progress
--
--     * Succeeded
--
--     * Succeeded with Issues
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'upgradeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhUpgradeStatus :: Lens.Lens' UpgradeHistory (Lude.Maybe UpgradeStatus)
uhUpgradeStatus = Lens.lens (upgradeStatus :: UpgradeHistory -> Lude.Maybe UpgradeStatus) (\s a -> s {upgradeStatus = a} :: UpgradeHistory)
{-# DEPRECATED uhUpgradeStatus "Use generic-lens or generic-optics with 'upgradeStatus' instead." #-}

-- | A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check.
--
-- /Note:/ Consider using 'stepsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhStepsList :: Lens.Lens' UpgradeHistory (Lude.Maybe [UpgradeStepItem])
uhStepsList = Lens.lens (stepsList :: UpgradeHistory -> Lude.Maybe [UpgradeStepItem]) (\s a -> s {stepsList = a} :: UpgradeHistory)
{-# DEPRECATED uhStepsList "Use generic-lens or generic-optics with 'stepsList' instead." #-}

-- | A string that describes the update briefly
--
-- /Note:/ Consider using 'upgradeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhUpgradeName :: Lens.Lens' UpgradeHistory (Lude.Maybe Lude.Text)
uhUpgradeName = Lens.lens (upgradeName :: UpgradeHistory -> Lude.Maybe Lude.Text) (\s a -> s {upgradeName = a} :: UpgradeHistory)
{-# DEPRECATED uhUpgradeName "Use generic-lens or generic-optics with 'upgradeName' instead." #-}

-- | UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhStartTimestamp :: Lens.Lens' UpgradeHistory (Lude.Maybe Lude.Timestamp)
uhStartTimestamp = Lens.lens (startTimestamp :: UpgradeHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTimestamp = a} :: UpgradeHistory)
{-# DEPRECATED uhStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

instance Lude.FromJSON UpgradeHistory where
  parseJSON =
    Lude.withObject
      "UpgradeHistory"
      ( \x ->
          UpgradeHistory'
            Lude.<$> (x Lude..:? "UpgradeStatus")
            Lude.<*> (x Lude..:? "StepsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UpgradeName")
            Lude.<*> (x Lude..:? "StartTimestamp")
      )

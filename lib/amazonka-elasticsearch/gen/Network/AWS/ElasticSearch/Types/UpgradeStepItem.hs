{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStepItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStepItem
  ( UpgradeStepItem (..),

    -- * Smart constructor
    mkUpgradeStepItem,

    -- * Lenses
    usiUpgradeStepStatus,
    usiProgressPercent,
    usiIssues,
    usiUpgradeStep,
  )
where

import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStep
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single step of the Upgrade or Upgrade Eligibility Check workflow.
--
-- /See:/ 'mkUpgradeStepItem' smart constructor.
data UpgradeStepItem = UpgradeStepItem'
  { -- | The status of a particular step during an upgrade. The status can take one of the following values:
    --
    --     * In Progress
    --
    --     * Succeeded
    --
    --     * Succeeded with Issues
    --
    --     * Failed
    upgradeStepStatus :: Lude.Maybe UpgradeStatus,
    -- | The Floating point value representing progress percentage of a particular step.
    progressPercent :: Lude.Maybe Lude.Double,
    -- | A list of strings containing detailed information about the errors encountered in a particular step.
    issues :: Lude.Maybe [Lude.Text],
    -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
    --
    --     * PreUpgradeCheck
    --
    --     * Snapshot
    --
    --     * Upgrade
    upgradeStep :: Lude.Maybe UpgradeStep
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeStepItem' with the minimum fields required to make a request.
--
-- * 'upgradeStepStatus' - The status of a particular step during an upgrade. The status can take one of the following values:
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
-- * 'progressPercent' - The Floating point value representing progress percentage of a particular step.
-- * 'issues' - A list of strings containing detailed information about the errors encountered in a particular step.
-- * 'upgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
--
--     * PreUpgradeCheck
--
--     * Snapshot
--
--     * Upgrade
mkUpgradeStepItem ::
  UpgradeStepItem
mkUpgradeStepItem =
  UpgradeStepItem'
    { upgradeStepStatus = Lude.Nothing,
      progressPercent = Lude.Nothing,
      issues = Lude.Nothing,
      upgradeStep = Lude.Nothing
    }

-- | The status of a particular step during an upgrade. The status can take one of the following values:
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
-- /Note:/ Consider using 'upgradeStepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiUpgradeStepStatus :: Lens.Lens' UpgradeStepItem (Lude.Maybe UpgradeStatus)
usiUpgradeStepStatus = Lens.lens (upgradeStepStatus :: UpgradeStepItem -> Lude.Maybe UpgradeStatus) (\s a -> s {upgradeStepStatus = a} :: UpgradeStepItem)
{-# DEPRECATED usiUpgradeStepStatus "Use generic-lens or generic-optics with 'upgradeStepStatus' instead." #-}

-- | The Floating point value representing progress percentage of a particular step.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiProgressPercent :: Lens.Lens' UpgradeStepItem (Lude.Maybe Lude.Double)
usiProgressPercent = Lens.lens (progressPercent :: UpgradeStepItem -> Lude.Maybe Lude.Double) (\s a -> s {progressPercent = a} :: UpgradeStepItem)
{-# DEPRECATED usiProgressPercent "Use generic-lens or generic-optics with 'progressPercent' instead." #-}

-- | A list of strings containing detailed information about the errors encountered in a particular step.
--
-- /Note:/ Consider using 'issues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiIssues :: Lens.Lens' UpgradeStepItem (Lude.Maybe [Lude.Text])
usiIssues = Lens.lens (issues :: UpgradeStepItem -> Lude.Maybe [Lude.Text]) (\s a -> s {issues = a} :: UpgradeStepItem)
{-# DEPRECATED usiIssues "Use generic-lens or generic-optics with 'issues' instead." #-}

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
--
--     * PreUpgradeCheck
--
--     * Snapshot
--
--     * Upgrade
--
--
--
-- /Note:/ Consider using 'upgradeStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiUpgradeStep :: Lens.Lens' UpgradeStepItem (Lude.Maybe UpgradeStep)
usiUpgradeStep = Lens.lens (upgradeStep :: UpgradeStepItem -> Lude.Maybe UpgradeStep) (\s a -> s {upgradeStep = a} :: UpgradeStepItem)
{-# DEPRECATED usiUpgradeStep "Use generic-lens or generic-optics with 'upgradeStep' instead." #-}

instance Lude.FromJSON UpgradeStepItem where
  parseJSON =
    Lude.withObject
      "UpgradeStepItem"
      ( \x ->
          UpgradeStepItem'
            Lude.<$> (x Lude..:? "UpgradeStepStatus")
            Lude.<*> (x Lude..:? "ProgressPercent")
            Lude.<*> (x Lude..:? "Issues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UpgradeStep")
      )

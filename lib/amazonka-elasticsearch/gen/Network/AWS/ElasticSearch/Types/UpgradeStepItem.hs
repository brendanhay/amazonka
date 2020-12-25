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
    usiIssues,
    usiProgressPercent,
    usiUpgradeStep,
    usiUpgradeStepStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.Issue as Types
import qualified Network.AWS.ElasticSearch.Types.UpgradeStatus as Types
import qualified Network.AWS.ElasticSearch.Types.UpgradeStep as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single step of the Upgrade or Upgrade Eligibility Check workflow.
--
-- /See:/ 'mkUpgradeStepItem' smart constructor.
data UpgradeStepItem = UpgradeStepItem'
  { -- | A list of strings containing detailed information about the errors encountered in a particular step.
    issues :: Core.Maybe [Types.Issue],
    -- | The Floating point value representing progress percentage of a particular step.
    progressPercent :: Core.Maybe Core.Double,
    -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
    --
    --     * PreUpgradeCheck
    --
    --     * Snapshot
    --
    --     * Upgrade
    upgradeStep :: Core.Maybe Types.UpgradeStep,
    -- | The status of a particular step during an upgrade. The status can take one of the following values:
    --
    --     * In Progress
    --
    --     * Succeeded
    --
    --     * Succeeded with Issues
    --
    --     * Failed
    upgradeStepStatus :: Core.Maybe Types.UpgradeStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeStepItem' value with any optional fields omitted.
mkUpgradeStepItem ::
  UpgradeStepItem
mkUpgradeStepItem =
  UpgradeStepItem'
    { issues = Core.Nothing,
      progressPercent = Core.Nothing,
      upgradeStep = Core.Nothing,
      upgradeStepStatus = Core.Nothing
    }

-- | A list of strings containing detailed information about the errors encountered in a particular step.
--
-- /Note:/ Consider using 'issues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiIssues :: Lens.Lens' UpgradeStepItem (Core.Maybe [Types.Issue])
usiIssues = Lens.field @"issues"
{-# DEPRECATED usiIssues "Use generic-lens or generic-optics with 'issues' instead." #-}

-- | The Floating point value representing progress percentage of a particular step.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiProgressPercent :: Lens.Lens' UpgradeStepItem (Core.Maybe Core.Double)
usiProgressPercent = Lens.field @"progressPercent"
{-# DEPRECATED usiProgressPercent "Use generic-lens or generic-optics with 'progressPercent' instead." #-}

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
usiUpgradeStep :: Lens.Lens' UpgradeStepItem (Core.Maybe Types.UpgradeStep)
usiUpgradeStep = Lens.field @"upgradeStep"
{-# DEPRECATED usiUpgradeStep "Use generic-lens or generic-optics with 'upgradeStep' instead." #-}

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
usiUpgradeStepStatus :: Lens.Lens' UpgradeStepItem (Core.Maybe Types.UpgradeStatus)
usiUpgradeStepStatus = Lens.field @"upgradeStepStatus"
{-# DEPRECATED usiUpgradeStepStatus "Use generic-lens or generic-optics with 'upgradeStepStatus' instead." #-}

instance Core.FromJSON UpgradeStepItem where
  parseJSON =
    Core.withObject "UpgradeStepItem" Core.$
      \x ->
        UpgradeStepItem'
          Core.<$> (x Core..:? "Issues")
          Core.<*> (x Core..:? "ProgressPercent")
          Core.<*> (x Core..:? "UpgradeStep")
          Core.<*> (x Core..:? "UpgradeStepStatus")

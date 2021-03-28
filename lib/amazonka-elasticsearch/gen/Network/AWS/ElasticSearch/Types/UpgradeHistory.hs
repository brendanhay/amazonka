{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.UpgradeHistory
  ( UpgradeHistory (..)
  -- * Smart constructor
  , mkUpgradeHistory
  -- * Lenses
  , uhStartTimestamp
  , uhStepsList
  , uhUpgradeName
  , uhUpgradeStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.UpgradeName as Types
import qualified Network.AWS.ElasticSearch.Types.UpgradeStatus as Types
import qualified Network.AWS.ElasticSearch.Types.UpgradeStepItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | History of the last 10 Upgrades and Upgrade Eligibility Checks.
--
-- /See:/ 'mkUpgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { startTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
  , stepsList :: Core.Maybe [Types.UpgradeStepItem]
    -- ^ A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check. 
  , upgradeName :: Core.Maybe Types.UpgradeName
    -- ^ A string that describes the update briefly
  , upgradeStatus :: Core.Maybe Types.UpgradeStatus
    -- ^ The overall status of the update. The status can take one of the following values: 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpgradeHistory' value with any optional fields omitted.
mkUpgradeHistory
    :: UpgradeHistory
mkUpgradeHistory
  = UpgradeHistory'{startTimestamp = Core.Nothing,
                    stepsList = Core.Nothing, upgradeName = Core.Nothing,
                    upgradeStatus = Core.Nothing}

-- | UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhStartTimestamp :: Lens.Lens' UpgradeHistory (Core.Maybe Core.NominalDiffTime)
uhStartTimestamp = Lens.field @"startTimestamp"
{-# INLINEABLE uhStartTimestamp #-}
{-# DEPRECATED startTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead"  #-}

-- | A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check. 
--
-- /Note:/ Consider using 'stepsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhStepsList :: Lens.Lens' UpgradeHistory (Core.Maybe [Types.UpgradeStepItem])
uhStepsList = Lens.field @"stepsList"
{-# INLINEABLE uhStepsList #-}
{-# DEPRECATED stepsList "Use generic-lens or generic-optics with 'stepsList' instead"  #-}

-- | A string that describes the update briefly
--
-- /Note:/ Consider using 'upgradeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhUpgradeName :: Lens.Lens' UpgradeHistory (Core.Maybe Types.UpgradeName)
uhUpgradeName = Lens.field @"upgradeName"
{-# INLINEABLE uhUpgradeName #-}
{-# DEPRECATED upgradeName "Use generic-lens or generic-optics with 'upgradeName' instead"  #-}

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
uhUpgradeStatus :: Lens.Lens' UpgradeHistory (Core.Maybe Types.UpgradeStatus)
uhUpgradeStatus = Lens.field @"upgradeStatus"
{-# INLINEABLE uhUpgradeStatus #-}
{-# DEPRECATED upgradeStatus "Use generic-lens or generic-optics with 'upgradeStatus' instead"  #-}

instance Core.FromJSON UpgradeHistory where
        parseJSON
          = Core.withObject "UpgradeHistory" Core.$
              \ x ->
                UpgradeHistory' Core.<$>
                  (x Core..:? "StartTimestamp") Core.<*> x Core..:? "StepsList"
                    Core.<*> x Core..:? "UpgradeName"
                    Core.<*> x Core..:? "UpgradeStatus"

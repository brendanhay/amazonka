{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
  ( ConfigRuleComplianceSummaryFilters (..)
  -- * Smart constructor
  , mkConfigRuleComplianceSummaryFilters
  -- * Lenses
  , crcsfAccountId
  , crcsfAwsRegion
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the results based on the account IDs and regions.
--
-- /See:/ 'mkConfigRuleComplianceSummaryFilters' smart constructor.
data ConfigRuleComplianceSummaryFilters = ConfigRuleComplianceSummaryFilters'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit account ID of the source account.
  , awsRegion :: Core.Maybe Types.AwsRegion
    -- ^ The source region where the data is aggregated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigRuleComplianceSummaryFilters' value with any optional fields omitted.
mkConfigRuleComplianceSummaryFilters
    :: ConfigRuleComplianceSummaryFilters
mkConfigRuleComplianceSummaryFilters
  = ConfigRuleComplianceSummaryFilters'{accountId = Core.Nothing,
                                        awsRegion = Core.Nothing}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcsfAccountId :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Core.Maybe Types.AccountId)
crcsfAccountId = Lens.field @"accountId"
{-# INLINEABLE crcsfAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The source region where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcsfAwsRegion :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Core.Maybe Types.AwsRegion)
crcsfAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE crcsfAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

instance Core.FromJSON ConfigRuleComplianceSummaryFilters where
        toJSON ConfigRuleComplianceSummaryFilters{..}
          = Core.object
              (Core.catMaybes
                 [("AccountId" Core..=) Core.<$> accountId,
                  ("AwsRegion" Core..=) Core.<$> awsRegion])

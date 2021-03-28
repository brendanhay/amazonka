{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AccountAggregationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.AccountAggregationSource
  ( AccountAggregationSource (..)
  -- * Smart constructor
  , mkAccountAggregationSource
  -- * Lenses
  , aasAccountIds
  , aasAllAwsRegions
  , aasAwsRegions
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of accounts and regions.
--
-- /See:/ 'mkAccountAggregationSource' smart constructor.
data AccountAggregationSource = AccountAggregationSource'
  { accountIds :: Core.NonEmpty Types.AccountId
    -- ^ The 12-digit account ID of the account being aggregated. 
  , allAwsRegions :: Core.Maybe Core.Bool
    -- ^ If true, aggregate existing AWS Config regions and future regions.
  , awsRegions :: Core.Maybe (Core.NonEmpty Core.Text)
    -- ^ The source regions being aggregated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountAggregationSource' value with any optional fields omitted.
mkAccountAggregationSource
    :: Core.NonEmpty Types.AccountId -- ^ 'accountIds'
    -> AccountAggregationSource
mkAccountAggregationSource accountIds
  = AccountAggregationSource'{accountIds,
                              allAwsRegions = Core.Nothing, awsRegions = Core.Nothing}

-- | The 12-digit account ID of the account being aggregated. 
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAccountIds :: Lens.Lens' AccountAggregationSource (Core.NonEmpty Types.AccountId)
aasAccountIds = Lens.field @"accountIds"
{-# INLINEABLE aasAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

-- | If true, aggregate existing AWS Config regions and future regions.
--
-- /Note:/ Consider using 'allAwsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllAwsRegions :: Lens.Lens' AccountAggregationSource (Core.Maybe Core.Bool)
aasAllAwsRegions = Lens.field @"allAwsRegions"
{-# INLINEABLE aasAllAwsRegions #-}
{-# DEPRECATED allAwsRegions "Use generic-lens or generic-optics with 'allAwsRegions' instead"  #-}

-- | The source regions being aggregated.
--
-- /Note:/ Consider using 'awsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAwsRegions :: Lens.Lens' AccountAggregationSource (Core.Maybe (Core.NonEmpty Core.Text))
aasAwsRegions = Lens.field @"awsRegions"
{-# INLINEABLE aasAwsRegions #-}
{-# DEPRECATED awsRegions "Use generic-lens or generic-optics with 'awsRegions' instead"  #-}

instance Core.FromJSON AccountAggregationSource where
        toJSON AccountAggregationSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountIds" Core..= accountIds),
                  ("AllAwsRegions" Core..=) Core.<$> allAwsRegions,
                  ("AwsRegions" Core..=) Core.<$> awsRegions])

instance Core.FromJSON AccountAggregationSource where
        parseJSON
          = Core.withObject "AccountAggregationSource" Core.$
              \ x ->
                AccountAggregationSource' Core.<$>
                  (x Core..: "AccountIds") Core.<*> x Core..:? "AllAwsRegions"
                    Core.<*> x Core..:? "AwsRegions"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AccountAggregationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AccountAggregationSource
  ( AccountAggregationSource (..),

    -- * Smart constructor
    mkAccountAggregationSource,

    -- * Lenses
    aasAccountIds,
    aasAllAwsRegions,
    aasAwsRegions,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of accounts and regions.
--
-- /See:/ 'mkAccountAggregationSource' smart constructor.
data AccountAggregationSource = AccountAggregationSource'
  { -- | The 12-digit account ID of the account being aggregated.
    accountIds :: Core.NonEmpty Types.AccountId,
    -- | If true, aggregate existing AWS Config regions and future regions.
    allAwsRegions :: Core.Maybe Core.Bool,
    -- | The source regions being aggregated.
    awsRegions :: Core.Maybe (Core.NonEmpty Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountAggregationSource' value with any optional fields omitted.
mkAccountAggregationSource ::
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  AccountAggregationSource
mkAccountAggregationSource accountIds =
  AccountAggregationSource'
    { accountIds,
      allAwsRegions = Core.Nothing,
      awsRegions = Core.Nothing
    }

-- | The 12-digit account ID of the account being aggregated.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAccountIds :: Lens.Lens' AccountAggregationSource (Core.NonEmpty Types.AccountId)
aasAccountIds = Lens.field @"accountIds"
{-# DEPRECATED aasAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | If true, aggregate existing AWS Config regions and future regions.
--
-- /Note:/ Consider using 'allAwsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllAwsRegions :: Lens.Lens' AccountAggregationSource (Core.Maybe Core.Bool)
aasAllAwsRegions = Lens.field @"allAwsRegions"
{-# DEPRECATED aasAllAwsRegions "Use generic-lens or generic-optics with 'allAwsRegions' instead." #-}

-- | The source regions being aggregated.
--
-- /Note:/ Consider using 'awsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAwsRegions :: Lens.Lens' AccountAggregationSource (Core.Maybe (Core.NonEmpty Types.String))
aasAwsRegions = Lens.field @"awsRegions"
{-# DEPRECATED aasAwsRegions "Use generic-lens or generic-optics with 'awsRegions' instead." #-}

instance Core.FromJSON AccountAggregationSource where
  toJSON AccountAggregationSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountIds" Core..= accountIds),
            ("AllAwsRegions" Core..=) Core.<$> allAwsRegions,
            ("AwsRegions" Core..=) Core.<$> awsRegions
          ]
      )

instance Core.FromJSON AccountAggregationSource where
  parseJSON =
    Core.withObject "AccountAggregationSource" Core.$
      \x ->
        AccountAggregationSource'
          Core.<$> (x Core..: "AccountIds")
          Core.<*> (x Core..:? "AllAwsRegions")
          Core.<*> (x Core..:? "AwsRegions")

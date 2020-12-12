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
    aasAWSRegions,
    aasAllAWSRegions,
    aasAccountIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of accounts and regions.
--
-- /See:/ 'mkAccountAggregationSource' smart constructor.
data AccountAggregationSource = AccountAggregationSource'
  { awsRegions ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    allAWSRegions :: Lude.Maybe Lude.Bool,
    accountIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountAggregationSource' with the minimum fields required to make a request.
--
-- * 'accountIds' - The 12-digit account ID of the account being aggregated.
-- * 'allAWSRegions' - If true, aggregate existing AWS Config regions and future regions.
-- * 'awsRegions' - The source regions being aggregated.
mkAccountAggregationSource ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  AccountAggregationSource
mkAccountAggregationSource pAccountIds_ =
  AccountAggregationSource'
    { awsRegions = Lude.Nothing,
      allAWSRegions = Lude.Nothing,
      accountIds = pAccountIds_
    }

-- | The source regions being aggregated.
--
-- /Note:/ Consider using 'awsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAWSRegions :: Lens.Lens' AccountAggregationSource (Lude.Maybe (Lude.NonEmpty Lude.Text))
aasAWSRegions = Lens.lens (awsRegions :: AccountAggregationSource -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {awsRegions = a} :: AccountAggregationSource)
{-# DEPRECATED aasAWSRegions "Use generic-lens or generic-optics with 'awsRegions' instead." #-}

-- | If true, aggregate existing AWS Config regions and future regions.
--
-- /Note:/ Consider using 'allAWSRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAllAWSRegions :: Lens.Lens' AccountAggregationSource (Lude.Maybe Lude.Bool)
aasAllAWSRegions = Lens.lens (allAWSRegions :: AccountAggregationSource -> Lude.Maybe Lude.Bool) (\s a -> s {allAWSRegions = a} :: AccountAggregationSource)
{-# DEPRECATED aasAllAWSRegions "Use generic-lens or generic-optics with 'allAWSRegions' instead." #-}

-- | The 12-digit account ID of the account being aggregated.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasAccountIds :: Lens.Lens' AccountAggregationSource (Lude.NonEmpty Lude.Text)
aasAccountIds = Lens.lens (accountIds :: AccountAggregationSource -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: AccountAggregationSource)
{-# DEPRECATED aasAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.FromJSON AccountAggregationSource where
  parseJSON =
    Lude.withObject
      "AccountAggregationSource"
      ( \x ->
          AccountAggregationSource'
            Lude.<$> (x Lude..:? "AwsRegions")
            Lude.<*> (x Lude..:? "AllAwsRegions")
            Lude.<*> (x Lude..: "AccountIds")
      )

instance Lude.ToJSON AccountAggregationSource where
  toJSON AccountAggregationSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AwsRegions" Lude..=) Lude.<$> awsRegions,
            ("AllAwsRegions" Lude..=) Lude.<$> allAWSRegions,
            Lude.Just ("AccountIds" Lude..= accountIds)
          ]
      )

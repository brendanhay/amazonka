{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
  ( ConfigRuleComplianceSummaryFilters (..),

    -- * Smart constructor
    mkConfigRuleComplianceSummaryFilters,

    -- * Lenses
    crcsfAccountId,
    crcsfAWSRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the results based on the account IDs and regions.
--
-- /See:/ 'mkConfigRuleComplianceSummaryFilters' smart constructor.
data ConfigRuleComplianceSummaryFilters = ConfigRuleComplianceSummaryFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Lude.Maybe Lude.Text,
    -- | The source region where the data is aggregated.
    awsRegion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigRuleComplianceSummaryFilters' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the source account.
-- * 'awsRegion' - The source region where the data is aggregated.
mkConfigRuleComplianceSummaryFilters ::
  ConfigRuleComplianceSummaryFilters
mkConfigRuleComplianceSummaryFilters =
  ConfigRuleComplianceSummaryFilters'
    { accountId = Lude.Nothing,
      awsRegion = Lude.Nothing
    }

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcsfAccountId :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Lude.Maybe Lude.Text)
crcsfAccountId = Lens.lens (accountId :: ConfigRuleComplianceSummaryFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ConfigRuleComplianceSummaryFilters)
{-# DEPRECATED crcsfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcsfAWSRegion :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Lude.Maybe Lude.Text)
crcsfAWSRegion = Lens.lens (awsRegion :: ConfigRuleComplianceSummaryFilters -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: ConfigRuleComplianceSummaryFilters)
{-# DEPRECATED crcsfAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.ToJSON ConfigRuleComplianceSummaryFilters where
  toJSON ConfigRuleComplianceSummaryFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccountId" Lude..=) Lude.<$> accountId,
            ("AwsRegion" Lude..=) Lude.<$> awsRegion
          ]
      )

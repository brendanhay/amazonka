{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceFilters
  ( ConfigRuleComplianceFilters (..),

    -- * Smart constructor
    mkConfigRuleComplianceFilters,

    -- * Lenses
    crcfConfigRuleName,
    crcfAccountId,
    crcfComplianceType,
    crcfAWSRegion,
  )
where

import Network.AWS.Config.Types.ComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the compliance results based on account ID, region, compliance type, and rule name.
--
-- /See:/ 'mkConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { configRuleName ::
      Lude.Maybe Lude.Text,
    accountId :: Lude.Maybe Lude.Text,
    complianceType ::
      Lude.Maybe ComplianceType,
    awsRegion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigRuleComplianceFilters' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the source account.
-- * 'awsRegion' - The source region where the data is aggregated.
-- * 'complianceType' - The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
-- * 'configRuleName' - The name of the AWS Config rule.
mkConfigRuleComplianceFilters ::
  ConfigRuleComplianceFilters
mkConfigRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { configRuleName = Lude.Nothing,
      accountId = Lude.Nothing,
      complianceType = Lude.Nothing,
      awsRegion = Lude.Nothing
    }

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfConfigRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Lude.Maybe Lude.Text)
crcfConfigRuleName = Lens.lens (configRuleName :: ConfigRuleComplianceFilters -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: ConfigRuleComplianceFilters)
{-# DEPRECATED crcfConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfAccountId :: Lens.Lens' ConfigRuleComplianceFilters (Lude.Maybe Lude.Text)
crcfAccountId = Lens.lens (accountId :: ConfigRuleComplianceFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ConfigRuleComplianceFilters)
{-# DEPRECATED crcfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfComplianceType :: Lens.Lens' ConfigRuleComplianceFilters (Lude.Maybe ComplianceType)
crcfComplianceType = Lens.lens (complianceType :: ConfigRuleComplianceFilters -> Lude.Maybe ComplianceType) (\s a -> s {complianceType = a} :: ConfigRuleComplianceFilters)
{-# DEPRECATED crcfComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The source region where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfAWSRegion :: Lens.Lens' ConfigRuleComplianceFilters (Lude.Maybe Lude.Text)
crcfAWSRegion = Lens.lens (awsRegion :: ConfigRuleComplianceFilters -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: ConfigRuleComplianceFilters)
{-# DEPRECATED crcfAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigRuleName" Lude..=) Lude.<$> configRuleName,
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("ComplianceType" Lude..=) Lude.<$> complianceType,
            ("AwsRegion" Lude..=) Lude.<$> awsRegion
          ]
      )

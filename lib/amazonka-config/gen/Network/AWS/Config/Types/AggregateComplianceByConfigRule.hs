{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateComplianceByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceByConfigRule
  ( AggregateComplianceByConfigRule (..),

    -- * Smart constructor
    mkAggregateComplianceByConfigRule,

    -- * Lenses
    acbcrCompliance,
    acbcrConfigRuleName,
    acbcrAccountId,
    acbcrAWSRegion,
  )
where

import Network.AWS.Config.Types.Compliance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether an AWS Config rule is compliant based on account ID, region, compliance, and rule name.
--
-- A rule is compliant if all of the resources that the rule evaluated comply with it. It is noncompliant if any of these resources do not comply.
--
-- /See:/ 'mkAggregateComplianceByConfigRule' smart constructor.
data AggregateComplianceByConfigRule = AggregateComplianceByConfigRule'
  { -- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
    compliance :: Lude.Maybe Compliance,
    -- | The name of the AWS Config rule.
    configRuleName :: Lude.Maybe Lude.Text,
    -- | The 12-digit account ID of the source account.
    accountId :: Lude.Maybe Lude.Text,
    -- | The source region from where the data is aggregated.
    awsRegion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AggregateComplianceByConfigRule' with the minimum fields required to make a request.
--
-- * 'compliance' - Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'accountId' - The 12-digit account ID of the source account.
-- * 'awsRegion' - The source region from where the data is aggregated.
mkAggregateComplianceByConfigRule ::
  AggregateComplianceByConfigRule
mkAggregateComplianceByConfigRule =
  AggregateComplianceByConfigRule'
    { compliance = Lude.Nothing,
      configRuleName = Lude.Nothing,
      accountId = Lude.Nothing,
      awsRegion = Lude.Nothing
    }

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrCompliance :: Lens.Lens' AggregateComplianceByConfigRule (Lude.Maybe Compliance)
acbcrCompliance = Lens.lens (compliance :: AggregateComplianceByConfigRule -> Lude.Maybe Compliance) (\s a -> s {compliance = a} :: AggregateComplianceByConfigRule)
{-# DEPRECATED acbcrCompliance "Use generic-lens or generic-optics with 'compliance' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrConfigRuleName :: Lens.Lens' AggregateComplianceByConfigRule (Lude.Maybe Lude.Text)
acbcrConfigRuleName = Lens.lens (configRuleName :: AggregateComplianceByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: AggregateComplianceByConfigRule)
{-# DEPRECATED acbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrAccountId :: Lens.Lens' AggregateComplianceByConfigRule (Lude.Maybe Lude.Text)
acbcrAccountId = Lens.lens (accountId :: AggregateComplianceByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AggregateComplianceByConfigRule)
{-# DEPRECATED acbcrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrAWSRegion :: Lens.Lens' AggregateComplianceByConfigRule (Lude.Maybe Lude.Text)
acbcrAWSRegion = Lens.lens (awsRegion :: AggregateComplianceByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: AggregateComplianceByConfigRule)
{-# DEPRECATED acbcrAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.FromJSON AggregateComplianceByConfigRule where
  parseJSON =
    Lude.withObject
      "AggregateComplianceByConfigRule"
      ( \x ->
          AggregateComplianceByConfigRule'
            Lude.<$> (x Lude..:? "Compliance")
            Lude.<*> (x Lude..:? "ConfigRuleName")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "AwsRegion")
      )

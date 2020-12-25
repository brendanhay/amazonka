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
    acbcrAccountId,
    acbcrAwsRegion,
    acbcrCompliance,
    acbcrConfigRuleName,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.Compliance as Types
import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether an AWS Config rule is compliant based on account ID, region, compliance, and rule name.
--
-- A rule is compliant if all of the resources that the rule evaluated comply with it. It is noncompliant if any of these resources do not comply.
--
-- /See:/ 'mkAggregateComplianceByConfigRule' smart constructor.
data AggregateComplianceByConfigRule = AggregateComplianceByConfigRule'
  { -- | The 12-digit account ID of the source account.
    accountId :: Core.Maybe Types.AccountId,
    -- | The source region from where the data is aggregated.
    awsRegion :: Core.Maybe Types.AwsRegion,
    -- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
    compliance :: Core.Maybe Types.Compliance,
    -- | The name of the AWS Config rule.
    configRuleName :: Core.Maybe Types.ConfigRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AggregateComplianceByConfigRule' value with any optional fields omitted.
mkAggregateComplianceByConfigRule ::
  AggregateComplianceByConfigRule
mkAggregateComplianceByConfigRule =
  AggregateComplianceByConfigRule'
    { accountId = Core.Nothing,
      awsRegion = Core.Nothing,
      compliance = Core.Nothing,
      configRuleName = Core.Nothing
    }

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrAccountId :: Lens.Lens' AggregateComplianceByConfigRule (Core.Maybe Types.AccountId)
acbcrAccountId = Lens.field @"accountId"
{-# DEPRECATED acbcrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrAwsRegion :: Lens.Lens' AggregateComplianceByConfigRule (Core.Maybe Types.AwsRegion)
acbcrAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED acbcrAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrCompliance :: Lens.Lens' AggregateComplianceByConfigRule (Core.Maybe Types.Compliance)
acbcrCompliance = Lens.field @"compliance"
{-# DEPRECATED acbcrCompliance "Use generic-lens or generic-optics with 'compliance' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acbcrConfigRuleName :: Lens.Lens' AggregateComplianceByConfigRule (Core.Maybe Types.ConfigRuleName)
acbcrConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED acbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Core.FromJSON AggregateComplianceByConfigRule where
  parseJSON =
    Core.withObject "AggregateComplianceByConfigRule" Core.$
      \x ->
        AggregateComplianceByConfigRule'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "AwsRegion")
          Core.<*> (x Core..:? "Compliance")
          Core.<*> (x Core..:? "ConfigRuleName")

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
    crcfAccountId,
    crcfAwsRegion,
    crcfComplianceType,
    crcfConfigRuleName,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.ComplianceType as Types
import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the compliance results based on account ID, region, compliance type, and rule name.
--
-- /See:/ 'mkConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Core.Maybe Types.AccountId,
    -- | The source region where the data is aggregated.
    awsRegion :: Core.Maybe Types.AwsRegion,
    -- | The rule compliance status.
    --
    -- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
    complianceType :: Core.Maybe Types.ComplianceType,
    -- | The name of the AWS Config rule.
    configRuleName :: Core.Maybe Types.ConfigRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigRuleComplianceFilters' value with any optional fields omitted.
mkConfigRuleComplianceFilters ::
  ConfigRuleComplianceFilters
mkConfigRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { accountId = Core.Nothing,
      awsRegion = Core.Nothing,
      complianceType = Core.Nothing,
      configRuleName = Core.Nothing
    }

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfAccountId :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Types.AccountId)
crcfAccountId = Lens.field @"accountId"
{-# DEPRECATED crcfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfAwsRegion :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Types.AwsRegion)
crcfAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED crcfAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfComplianceType :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Types.ComplianceType)
crcfComplianceType = Lens.field @"complianceType"
{-# DEPRECATED crcfComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcfConfigRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Types.ConfigRuleName)
crcfConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED crcfConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Core.FromJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("AwsRegion" Core..=) Core.<$> awsRegion,
            ("ComplianceType" Core..=) Core.<$> complianceType,
            ("ConfigRuleName" Core..=) Core.<$> configRuleName
          ]
      )

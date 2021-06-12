{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceFilters where

import Network.AWS.Config.Types.ComplianceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Filters the compliance results based on account ID, region, compliance
-- type, and rule name.
--
-- /See:/ 'newConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Core.Maybe Core.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Core.Maybe Core.Text,
    -- | The rule compliance status.
    --
    -- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports
    -- only @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support the
    -- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
    complianceType :: Core.Maybe ComplianceType,
    -- | The source region where the data is aggregated.
    awsRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigRuleComplianceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'configRuleComplianceFilters_accountId' - The 12-digit account ID of the source account.
--
-- 'configRuleName', 'configRuleComplianceFilters_configRuleName' - The name of the AWS Config rule.
--
-- 'complianceType', 'configRuleComplianceFilters_complianceType' - The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports
-- only @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- 'awsRegion', 'configRuleComplianceFilters_awsRegion' - The source region where the data is aggregated.
newConfigRuleComplianceFilters ::
  ConfigRuleComplianceFilters
newConfigRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { accountId =
        Core.Nothing,
      configRuleName = Core.Nothing,
      complianceType = Core.Nothing,
      awsRegion = Core.Nothing
    }

-- | The 12-digit account ID of the source account.
configRuleComplianceFilters_accountId :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Core.Text)
configRuleComplianceFilters_accountId = Lens.lens (\ConfigRuleComplianceFilters' {accountId} -> accountId) (\s@ConfigRuleComplianceFilters' {} a -> s {accountId = a} :: ConfigRuleComplianceFilters)

-- | The name of the AWS Config rule.
configRuleComplianceFilters_configRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Core.Text)
configRuleComplianceFilters_configRuleName = Lens.lens (\ConfigRuleComplianceFilters' {configRuleName} -> configRuleName) (\s@ConfigRuleComplianceFilters' {} a -> s {configRuleName = a} :: ConfigRuleComplianceFilters)

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports
-- only @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
configRuleComplianceFilters_complianceType :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe ComplianceType)
configRuleComplianceFilters_complianceType = Lens.lens (\ConfigRuleComplianceFilters' {complianceType} -> complianceType) (\s@ConfigRuleComplianceFilters' {} a -> s {complianceType = a} :: ConfigRuleComplianceFilters)

-- | The source region where the data is aggregated.
configRuleComplianceFilters_awsRegion :: Lens.Lens' ConfigRuleComplianceFilters (Core.Maybe Core.Text)
configRuleComplianceFilters_awsRegion = Lens.lens (\ConfigRuleComplianceFilters' {awsRegion} -> awsRegion) (\s@ConfigRuleComplianceFilters' {} a -> s {awsRegion = a} :: ConfigRuleComplianceFilters)

instance Core.Hashable ConfigRuleComplianceFilters

instance Core.NFData ConfigRuleComplianceFilters

instance Core.ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("ConfigRuleName" Core..=) Core.<$> configRuleName,
            ("ComplianceType" Core..=) Core.<$> complianceType,
            ("AwsRegion" Core..=) Core.<$> awsRegion
          ]
      )

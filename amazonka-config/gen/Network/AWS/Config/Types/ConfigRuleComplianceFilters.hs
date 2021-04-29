{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters the compliance results based on account ID, region, compliance
-- type, and rule name.
--
-- /See:/ 'newConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | The rule compliance status.
    --
    -- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports
    -- only @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support the
    -- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
    complianceType :: Prelude.Maybe ComplianceType,
    -- | The source region where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      awsRegion = Prelude.Nothing
    }

-- | The 12-digit account ID of the source account.
configRuleComplianceFilters_accountId :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_accountId = Lens.lens (\ConfigRuleComplianceFilters' {accountId} -> accountId) (\s@ConfigRuleComplianceFilters' {} a -> s {accountId = a} :: ConfigRuleComplianceFilters)

-- | The name of the AWS Config rule.
configRuleComplianceFilters_configRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_configRuleName = Lens.lens (\ConfigRuleComplianceFilters' {configRuleName} -> configRuleName) (\s@ConfigRuleComplianceFilters' {} a -> s {configRuleName = a} :: ConfigRuleComplianceFilters)

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, AWS Config supports
-- only @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
configRuleComplianceFilters_complianceType :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe ComplianceType)
configRuleComplianceFilters_complianceType = Lens.lens (\ConfigRuleComplianceFilters' {complianceType} -> complianceType) (\s@ConfigRuleComplianceFilters' {} a -> s {complianceType = a} :: ConfigRuleComplianceFilters)

-- | The source region where the data is aggregated.
configRuleComplianceFilters_awsRegion :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_awsRegion = Lens.lens (\ConfigRuleComplianceFilters' {awsRegion} -> awsRegion) (\s@ConfigRuleComplianceFilters' {} a -> s {awsRegion = a} :: ConfigRuleComplianceFilters)

instance Prelude.Hashable ConfigRuleComplianceFilters

instance Prelude.NFData ConfigRuleComplianceFilters

instance Prelude.ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("ConfigRuleName" Prelude..=)
              Prelude.<$> configRuleName,
            ("ComplianceType" Prelude..=)
              Prelude.<$> complianceType,
            ("AwsRegion" Prelude..=) Prelude.<$> awsRegion
          ]
      )

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
-- Module      : Amazonka.Config.Types.ConfigRuleComplianceFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigRuleComplianceFilters where

import Amazonka.Config.Types.ComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the compliance results based on account ID, region, compliance
-- type, and rule name.
--
-- /See:/ 'newConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source region where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The rule compliance status.
    --
    -- For the @ConfigRuleComplianceFilters@ data type, Config supports only
    -- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
    -- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
    complianceType :: Prelude.Maybe ComplianceType,
    -- | The name of the Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'awsRegion', 'configRuleComplianceFilters_awsRegion' - The source region where the data is aggregated.
--
-- 'complianceType', 'configRuleComplianceFilters_complianceType' - The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, Config supports only
-- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- 'configRuleName', 'configRuleComplianceFilters_configRuleName' - The name of the Config rule.
newConfigRuleComplianceFilters ::
  ConfigRuleComplianceFilters
newConfigRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { accountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      configRuleName = Prelude.Nothing
    }

-- | The 12-digit account ID of the source account.
configRuleComplianceFilters_accountId :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_accountId = Lens.lens (\ConfigRuleComplianceFilters' {accountId} -> accountId) (\s@ConfigRuleComplianceFilters' {} a -> s {accountId = a} :: ConfigRuleComplianceFilters)

-- | The source region where the data is aggregated.
configRuleComplianceFilters_awsRegion :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_awsRegion = Lens.lens (\ConfigRuleComplianceFilters' {awsRegion} -> awsRegion) (\s@ConfigRuleComplianceFilters' {} a -> s {awsRegion = a} :: ConfigRuleComplianceFilters)

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, Config supports only
-- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
configRuleComplianceFilters_complianceType :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe ComplianceType)
configRuleComplianceFilters_complianceType = Lens.lens (\ConfigRuleComplianceFilters' {complianceType} -> complianceType) (\s@ConfigRuleComplianceFilters' {} a -> s {complianceType = a} :: ConfigRuleComplianceFilters)

-- | The name of the Config rule.
configRuleComplianceFilters_configRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_configRuleName = Lens.lens (\ConfigRuleComplianceFilters' {configRuleName} -> configRuleName) (\s@ConfigRuleComplianceFilters' {} a -> s {configRuleName = a} :: ConfigRuleComplianceFilters)

instance Prelude.Hashable ConfigRuleComplianceFilters where
  hashWithSalt _salt ConfigRuleComplianceFilters' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` configRuleName

instance Prelude.NFData ConfigRuleComplianceFilters where
  rnf ConfigRuleComplianceFilters' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf configRuleName

instance Data.ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("AwsRegion" Data..=) Prelude.<$> awsRegion,
            ("ComplianceType" Data..=)
              Prelude.<$> complianceType,
            ("ConfigRuleName" Data..=)
              Prelude.<$> configRuleName
          ]
      )

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
-- Module      : Amazonka.Config.Types.ConfigRuleComplianceSummaryFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigRuleComplianceSummaryFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the results based on the account IDs and regions.
--
-- /See:/ 'newConfigRuleComplianceSummaryFilters' smart constructor.
data ConfigRuleComplianceSummaryFilters = ConfigRuleComplianceSummaryFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source region where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigRuleComplianceSummaryFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'configRuleComplianceSummaryFilters_accountId' - The 12-digit account ID of the source account.
--
-- 'awsRegion', 'configRuleComplianceSummaryFilters_awsRegion' - The source region where the data is aggregated.
newConfigRuleComplianceSummaryFilters ::
  ConfigRuleComplianceSummaryFilters
newConfigRuleComplianceSummaryFilters =
  ConfigRuleComplianceSummaryFilters'
    { accountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing
    }

-- | The 12-digit account ID of the source account.
configRuleComplianceSummaryFilters_accountId :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceSummaryFilters_accountId = Lens.lens (\ConfigRuleComplianceSummaryFilters' {accountId} -> accountId) (\s@ConfigRuleComplianceSummaryFilters' {} a -> s {accountId = a} :: ConfigRuleComplianceSummaryFilters)

-- | The source region where the data is aggregated.
configRuleComplianceSummaryFilters_awsRegion :: Lens.Lens' ConfigRuleComplianceSummaryFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceSummaryFilters_awsRegion = Lens.lens (\ConfigRuleComplianceSummaryFilters' {awsRegion} -> awsRegion) (\s@ConfigRuleComplianceSummaryFilters' {} a -> s {awsRegion = a} :: ConfigRuleComplianceSummaryFilters)

instance
  Prelude.Hashable
    ConfigRuleComplianceSummaryFilters
  where
  hashWithSalt
    _salt
    ConfigRuleComplianceSummaryFilters' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` awsRegion

instance
  Prelude.NFData
    ConfigRuleComplianceSummaryFilters
  where
  rnf ConfigRuleComplianceSummaryFilters' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion

instance
  Data.ToJSON
    ConfigRuleComplianceSummaryFilters
  where
  toJSON ConfigRuleComplianceSummaryFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("AwsRegion" Data..=) Prelude.<$> awsRegion
          ]
      )

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
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters the results based on the account IDs and regions.
--
-- /See:/ 'newConfigRuleComplianceSummaryFilters' smart constructor.
data ConfigRuleComplianceSummaryFilters = ConfigRuleComplianceSummaryFilters'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source region where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.NFData
    ConfigRuleComplianceSummaryFilters

instance
  Prelude.ToJSON
    ConfigRuleComplianceSummaryFilters
  where
  toJSON ConfigRuleComplianceSummaryFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("AwsRegion" Prelude..=) Prelude.<$> awsRegion
          ]
      )

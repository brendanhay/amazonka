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
-- Module      : Amazonka.Config.Types.AggregateConformancePackComplianceFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackComplianceFilters where

import Amazonka.Config.Types.ConformancePackComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the conformance packs based on an account ID, region, compliance
-- type, and the name of the conformance pack.
--
-- /See:/ 'newAggregateConformancePackComplianceFilters' smart constructor.
data AggregateConformancePackComplianceFilters = AggregateConformancePackComplianceFilters'
  { -- | The 12-digit Amazon Web Services account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source Amazon Web Services Region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The compliance status of the conformance pack.
    complianceType :: Prelude.Maybe ConformancePackComplianceType,
    -- | The name of the conformance pack.
    conformancePackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateConformancePackComplianceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'aggregateConformancePackComplianceFilters_accountId' - The 12-digit Amazon Web Services account ID of the source account.
--
-- 'awsRegion', 'aggregateConformancePackComplianceFilters_awsRegion' - The source Amazon Web Services Region from where the data is aggregated.
--
-- 'complianceType', 'aggregateConformancePackComplianceFilters_complianceType' - The compliance status of the conformance pack.
--
-- 'conformancePackName', 'aggregateConformancePackComplianceFilters_conformancePackName' - The name of the conformance pack.
newAggregateConformancePackComplianceFilters ::
  AggregateConformancePackComplianceFilters
newAggregateConformancePackComplianceFilters =
  AggregateConformancePackComplianceFilters'
    { accountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      conformancePackName =
        Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account ID of the source account.
aggregateConformancePackComplianceFilters_accountId :: Lens.Lens' AggregateConformancePackComplianceFilters (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceFilters_accountId = Lens.lens (\AggregateConformancePackComplianceFilters' {accountId} -> accountId) (\s@AggregateConformancePackComplianceFilters' {} a -> s {accountId = a} :: AggregateConformancePackComplianceFilters)

-- | The source Amazon Web Services Region from where the data is aggregated.
aggregateConformancePackComplianceFilters_awsRegion :: Lens.Lens' AggregateConformancePackComplianceFilters (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceFilters_awsRegion = Lens.lens (\AggregateConformancePackComplianceFilters' {awsRegion} -> awsRegion) (\s@AggregateConformancePackComplianceFilters' {} a -> s {awsRegion = a} :: AggregateConformancePackComplianceFilters)

-- | The compliance status of the conformance pack.
aggregateConformancePackComplianceFilters_complianceType :: Lens.Lens' AggregateConformancePackComplianceFilters (Prelude.Maybe ConformancePackComplianceType)
aggregateConformancePackComplianceFilters_complianceType = Lens.lens (\AggregateConformancePackComplianceFilters' {complianceType} -> complianceType) (\s@AggregateConformancePackComplianceFilters' {} a -> s {complianceType = a} :: AggregateConformancePackComplianceFilters)

-- | The name of the conformance pack.
aggregateConformancePackComplianceFilters_conformancePackName :: Lens.Lens' AggregateConformancePackComplianceFilters (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceFilters_conformancePackName = Lens.lens (\AggregateConformancePackComplianceFilters' {conformancePackName} -> conformancePackName) (\s@AggregateConformancePackComplianceFilters' {} a -> s {conformancePackName = a} :: AggregateConformancePackComplianceFilters)

instance
  Prelude.Hashable
    AggregateConformancePackComplianceFilters
  where
  hashWithSalt
    _salt
    AggregateConformancePackComplianceFilters' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` conformancePackName

instance
  Prelude.NFData
    AggregateConformancePackComplianceFilters
  where
  rnf AggregateConformancePackComplianceFilters' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf conformancePackName

instance
  Data.ToJSON
    AggregateConformancePackComplianceFilters
  where
  toJSON AggregateConformancePackComplianceFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("AwsRegion" Data..=) Prelude.<$> awsRegion,
            ("ComplianceType" Data..=)
              Prelude.<$> complianceType,
            ("ConformancePackName" Data..=)
              Prelude.<$> conformancePackName
          ]
      )

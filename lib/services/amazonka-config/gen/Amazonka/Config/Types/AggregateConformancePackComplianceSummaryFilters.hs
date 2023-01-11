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
-- Module      : Amazonka.Config.Types.AggregateConformancePackComplianceSummaryFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackComplianceSummaryFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters the results based on account ID and region.
--
-- /See:/ 'newAggregateConformancePackComplianceSummaryFilters' smart constructor.
data AggregateConformancePackComplianceSummaryFilters = AggregateConformancePackComplianceSummaryFilters'
  { -- | The 12-digit Amazon Web Services account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source Amazon Web Services Region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateConformancePackComplianceSummaryFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'aggregateConformancePackComplianceSummaryFilters_accountId' - The 12-digit Amazon Web Services account ID of the source account.
--
-- 'awsRegion', 'aggregateConformancePackComplianceSummaryFilters_awsRegion' - The source Amazon Web Services Region from where the data is aggregated.
newAggregateConformancePackComplianceSummaryFilters ::
  AggregateConformancePackComplianceSummaryFilters
newAggregateConformancePackComplianceSummaryFilters =
  AggregateConformancePackComplianceSummaryFilters'
    { accountId =
        Prelude.Nothing,
      awsRegion =
        Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account ID of the source account.
aggregateConformancePackComplianceSummaryFilters_accountId :: Lens.Lens' AggregateConformancePackComplianceSummaryFilters (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceSummaryFilters_accountId = Lens.lens (\AggregateConformancePackComplianceSummaryFilters' {accountId} -> accountId) (\s@AggregateConformancePackComplianceSummaryFilters' {} a -> s {accountId = a} :: AggregateConformancePackComplianceSummaryFilters)

-- | The source Amazon Web Services Region from where the data is aggregated.
aggregateConformancePackComplianceSummaryFilters_awsRegion :: Lens.Lens' AggregateConformancePackComplianceSummaryFilters (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceSummaryFilters_awsRegion = Lens.lens (\AggregateConformancePackComplianceSummaryFilters' {awsRegion} -> awsRegion) (\s@AggregateConformancePackComplianceSummaryFilters' {} a -> s {awsRegion = a} :: AggregateConformancePackComplianceSummaryFilters)

instance
  Prelude.Hashable
    AggregateConformancePackComplianceSummaryFilters
  where
  hashWithSalt
    _salt
    AggregateConformancePackComplianceSummaryFilters' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` awsRegion

instance
  Prelude.NFData
    AggregateConformancePackComplianceSummaryFilters
  where
  rnf
    AggregateConformancePackComplianceSummaryFilters' {..} =
      Prelude.rnf accountId
        `Prelude.seq` Prelude.rnf awsRegion

instance
  Data.ToJSON
    AggregateConformancePackComplianceSummaryFilters
  where
  toJSON
    AggregateConformancePackComplianceSummaryFilters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AccountId" Data..=) Prelude.<$> accountId,
              ("AwsRegion" Data..=) Prelude.<$> awsRegion
            ]
        )

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
-- Module      : Amazonka.Config.Types.AggregateConformancePackCompliance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackCompliance where

import Amazonka.Config.Types.ConformancePackComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the number of compliant and noncompliant rules within a
-- conformance pack. Also provides the compliance status of the conformance
-- pack and the total rule count which includes compliant rules,
-- noncompliant rules, and rules that cannot be evaluated due to
-- insufficient data.
--
-- A conformance pack is compliant if all of the rules in a conformance
-- packs are compliant. It is noncompliant if any of the rules are not
-- compliant. The compliance status of a conformance pack is
-- INSUFFICIENT_DATA only if all rules within a conformance pack cannot be
-- evaluated due to insufficient data. If some of the rules in a
-- conformance pack are compliant but the compliance status of other rules
-- in that same conformance pack is INSUFFICIENT_DATA, the conformance pack
-- shows compliant.
--
-- /See:/ 'newAggregateConformancePackCompliance' smart constructor.
data AggregateConformancePackCompliance = AggregateConformancePackCompliance'
  { -- | The number of noncompliant Config Rules.
    nonCompliantRuleCount :: Prelude.Maybe Prelude.Int,
    -- | Total number of compliant rules, noncompliant rules, and the rules that
    -- do not have any applicable resources to evaluate upon resulting in
    -- insufficient data.
    totalRuleCount :: Prelude.Maybe Prelude.Int,
    -- | The number of compliant Config Rules.
    compliantRuleCount :: Prelude.Maybe Prelude.Int,
    -- | The compliance status of the conformance pack.
    complianceType :: Prelude.Maybe ConformancePackComplianceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateConformancePackCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonCompliantRuleCount', 'aggregateConformancePackCompliance_nonCompliantRuleCount' - The number of noncompliant Config Rules.
--
-- 'totalRuleCount', 'aggregateConformancePackCompliance_totalRuleCount' - Total number of compliant rules, noncompliant rules, and the rules that
-- do not have any applicable resources to evaluate upon resulting in
-- insufficient data.
--
-- 'compliantRuleCount', 'aggregateConformancePackCompliance_compliantRuleCount' - The number of compliant Config Rules.
--
-- 'complianceType', 'aggregateConformancePackCompliance_complianceType' - The compliance status of the conformance pack.
newAggregateConformancePackCompliance ::
  AggregateConformancePackCompliance
newAggregateConformancePackCompliance =
  AggregateConformancePackCompliance'
    { nonCompliantRuleCount =
        Prelude.Nothing,
      totalRuleCount = Prelude.Nothing,
      compliantRuleCount = Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

-- | The number of noncompliant Config Rules.
aggregateConformancePackCompliance_nonCompliantRuleCount :: Lens.Lens' AggregateConformancePackCompliance (Prelude.Maybe Prelude.Int)
aggregateConformancePackCompliance_nonCompliantRuleCount = Lens.lens (\AggregateConformancePackCompliance' {nonCompliantRuleCount} -> nonCompliantRuleCount) (\s@AggregateConformancePackCompliance' {} a -> s {nonCompliantRuleCount = a} :: AggregateConformancePackCompliance)

-- | Total number of compliant rules, noncompliant rules, and the rules that
-- do not have any applicable resources to evaluate upon resulting in
-- insufficient data.
aggregateConformancePackCompliance_totalRuleCount :: Lens.Lens' AggregateConformancePackCompliance (Prelude.Maybe Prelude.Int)
aggregateConformancePackCompliance_totalRuleCount = Lens.lens (\AggregateConformancePackCompliance' {totalRuleCount} -> totalRuleCount) (\s@AggregateConformancePackCompliance' {} a -> s {totalRuleCount = a} :: AggregateConformancePackCompliance)

-- | The number of compliant Config Rules.
aggregateConformancePackCompliance_compliantRuleCount :: Lens.Lens' AggregateConformancePackCompliance (Prelude.Maybe Prelude.Int)
aggregateConformancePackCompliance_compliantRuleCount = Lens.lens (\AggregateConformancePackCompliance' {compliantRuleCount} -> compliantRuleCount) (\s@AggregateConformancePackCompliance' {} a -> s {compliantRuleCount = a} :: AggregateConformancePackCompliance)

-- | The compliance status of the conformance pack.
aggregateConformancePackCompliance_complianceType :: Lens.Lens' AggregateConformancePackCompliance (Prelude.Maybe ConformancePackComplianceType)
aggregateConformancePackCompliance_complianceType = Lens.lens (\AggregateConformancePackCompliance' {complianceType} -> complianceType) (\s@AggregateConformancePackCompliance' {} a -> s {complianceType = a} :: AggregateConformancePackCompliance)

instance
  Core.FromJSON
    AggregateConformancePackCompliance
  where
  parseJSON =
    Core.withObject
      "AggregateConformancePackCompliance"
      ( \x ->
          AggregateConformancePackCompliance'
            Prelude.<$> (x Core..:? "NonCompliantRuleCount")
            Prelude.<*> (x Core..:? "TotalRuleCount")
            Prelude.<*> (x Core..:? "CompliantRuleCount")
            Prelude.<*> (x Core..:? "ComplianceType")
      )

instance
  Prelude.Hashable
    AggregateConformancePackCompliance
  where
  hashWithSalt
    _salt
    AggregateConformancePackCompliance' {..} =
      _salt `Prelude.hashWithSalt` nonCompliantRuleCount
        `Prelude.hashWithSalt` totalRuleCount
        `Prelude.hashWithSalt` compliantRuleCount
        `Prelude.hashWithSalt` complianceType

instance
  Prelude.NFData
    AggregateConformancePackCompliance
  where
  rnf AggregateConformancePackCompliance' {..} =
    Prelude.rnf nonCompliantRuleCount
      `Prelude.seq` Prelude.rnf totalRuleCount
      `Prelude.seq` Prelude.rnf compliantRuleCount
      `Prelude.seq` Prelude.rnf complianceType

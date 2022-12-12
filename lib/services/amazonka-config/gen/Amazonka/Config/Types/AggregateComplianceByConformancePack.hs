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
-- Module      : Amazonka.Config.Types.AggregateComplianceByConformancePack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateComplianceByConformancePack where

import Amazonka.Config.Types.AggregateConformancePackCompliance
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides aggregate compliance of the conformance pack. Indicates whether
-- a conformance pack is compliant based on the name of the conformance
-- pack, account ID, and region.
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
-- /See:/ 'newAggregateComplianceByConformancePack' smart constructor.
data AggregateComplianceByConformancePack = AggregateComplianceByConformancePack'
  { -- | The 12-digit Amazon Web Services account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source Amazon Web Services Region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The compliance status of the conformance pack.
    compliance :: Prelude.Maybe AggregateConformancePackCompliance,
    -- | The name of the conformance pack.
    conformancePackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateComplianceByConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'aggregateComplianceByConformancePack_accountId' - The 12-digit Amazon Web Services account ID of the source account.
--
-- 'awsRegion', 'aggregateComplianceByConformancePack_awsRegion' - The source Amazon Web Services Region from where the data is aggregated.
--
-- 'compliance', 'aggregateComplianceByConformancePack_compliance' - The compliance status of the conformance pack.
--
-- 'conformancePackName', 'aggregateComplianceByConformancePack_conformancePackName' - The name of the conformance pack.
newAggregateComplianceByConformancePack ::
  AggregateComplianceByConformancePack
newAggregateComplianceByConformancePack =
  AggregateComplianceByConformancePack'
    { accountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      compliance = Prelude.Nothing,
      conformancePackName = Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account ID of the source account.
aggregateComplianceByConformancePack_accountId :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_accountId = Lens.lens (\AggregateComplianceByConformancePack' {accountId} -> accountId) (\s@AggregateComplianceByConformancePack' {} a -> s {accountId = a} :: AggregateComplianceByConformancePack)

-- | The source Amazon Web Services Region from where the data is aggregated.
aggregateComplianceByConformancePack_awsRegion :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_awsRegion = Lens.lens (\AggregateComplianceByConformancePack' {awsRegion} -> awsRegion) (\s@AggregateComplianceByConformancePack' {} a -> s {awsRegion = a} :: AggregateComplianceByConformancePack)

-- | The compliance status of the conformance pack.
aggregateComplianceByConformancePack_compliance :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe AggregateConformancePackCompliance)
aggregateComplianceByConformancePack_compliance = Lens.lens (\AggregateComplianceByConformancePack' {compliance} -> compliance) (\s@AggregateComplianceByConformancePack' {} a -> s {compliance = a} :: AggregateComplianceByConformancePack)

-- | The name of the conformance pack.
aggregateComplianceByConformancePack_conformancePackName :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_conformancePackName = Lens.lens (\AggregateComplianceByConformancePack' {conformancePackName} -> conformancePackName) (\s@AggregateComplianceByConformancePack' {} a -> s {conformancePackName = a} :: AggregateComplianceByConformancePack)

instance
  Data.FromJSON
    AggregateComplianceByConformancePack
  where
  parseJSON =
    Data.withObject
      "AggregateComplianceByConformancePack"
      ( \x ->
          AggregateComplianceByConformancePack'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "Compliance")
            Prelude.<*> (x Data..:? "ConformancePackName")
      )

instance
  Prelude.Hashable
    AggregateComplianceByConformancePack
  where
  hashWithSalt
    _salt
    AggregateComplianceByConformancePack' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` compliance
        `Prelude.hashWithSalt` conformancePackName

instance
  Prelude.NFData
    AggregateComplianceByConformancePack
  where
  rnf AggregateComplianceByConformancePack' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf compliance
      `Prelude.seq` Prelude.rnf conformancePackName

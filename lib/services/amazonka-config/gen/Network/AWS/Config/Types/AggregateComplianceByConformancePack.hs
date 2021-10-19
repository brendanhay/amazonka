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
-- Module      : Network.AWS.Config.Types.AggregateComplianceByConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceByConformancePack where

import Network.AWS.Config.Types.AggregateConformancePackCompliance
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The compliance status of the conformance pack.
    compliance :: Prelude.Maybe AggregateConformancePackCompliance,
    -- | The name of the conformance pack.
    conformancePackName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit Amazon Web Services account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source Amazon Web Services Region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
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
-- 'compliance', 'aggregateComplianceByConformancePack_compliance' - The compliance status of the conformance pack.
--
-- 'conformancePackName', 'aggregateComplianceByConformancePack_conformancePackName' - The name of the conformance pack.
--
-- 'accountId', 'aggregateComplianceByConformancePack_accountId' - The 12-digit Amazon Web Services account ID of the source account.
--
-- 'awsRegion', 'aggregateComplianceByConformancePack_awsRegion' - The source Amazon Web Services Region from where the data is aggregated.
newAggregateComplianceByConformancePack ::
  AggregateComplianceByConformancePack
newAggregateComplianceByConformancePack =
  AggregateComplianceByConformancePack'
    { compliance =
        Prelude.Nothing,
      conformancePackName = Prelude.Nothing,
      accountId = Prelude.Nothing,
      awsRegion = Prelude.Nothing
    }

-- | The compliance status of the conformance pack.
aggregateComplianceByConformancePack_compliance :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe AggregateConformancePackCompliance)
aggregateComplianceByConformancePack_compliance = Lens.lens (\AggregateComplianceByConformancePack' {compliance} -> compliance) (\s@AggregateComplianceByConformancePack' {} a -> s {compliance = a} :: AggregateComplianceByConformancePack)

-- | The name of the conformance pack.
aggregateComplianceByConformancePack_conformancePackName :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_conformancePackName = Lens.lens (\AggregateComplianceByConformancePack' {conformancePackName} -> conformancePackName) (\s@AggregateComplianceByConformancePack' {} a -> s {conformancePackName = a} :: AggregateComplianceByConformancePack)

-- | The 12-digit Amazon Web Services account ID of the source account.
aggregateComplianceByConformancePack_accountId :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_accountId = Lens.lens (\AggregateComplianceByConformancePack' {accountId} -> accountId) (\s@AggregateComplianceByConformancePack' {} a -> s {accountId = a} :: AggregateComplianceByConformancePack)

-- | The source Amazon Web Services Region from where the data is aggregated.
aggregateComplianceByConformancePack_awsRegion :: Lens.Lens' AggregateComplianceByConformancePack (Prelude.Maybe Prelude.Text)
aggregateComplianceByConformancePack_awsRegion = Lens.lens (\AggregateComplianceByConformancePack' {awsRegion} -> awsRegion) (\s@AggregateComplianceByConformancePack' {} a -> s {awsRegion = a} :: AggregateComplianceByConformancePack)

instance
  Core.FromJSON
    AggregateComplianceByConformancePack
  where
  parseJSON =
    Core.withObject
      "AggregateComplianceByConformancePack"
      ( \x ->
          AggregateComplianceByConformancePack'
            Prelude.<$> (x Core..:? "Compliance")
            Prelude.<*> (x Core..:? "ConformancePackName")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "AwsRegion")
      )

instance
  Prelude.Hashable
    AggregateComplianceByConformancePack

instance
  Prelude.NFData
    AggregateComplianceByConformancePack

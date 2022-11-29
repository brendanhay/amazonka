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
-- Module      : Amazonka.Config.Types.AggregateConformancePackComplianceCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackComplianceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The number of conformance packs that are compliant and noncompliant.
--
-- /See:/ 'newAggregateConformancePackComplianceCount' smart constructor.
data AggregateConformancePackComplianceCount = AggregateConformancePackComplianceCount'
  { -- | Number of compliant conformance packs.
    compliantConformancePackCount :: Prelude.Maybe Prelude.Int,
    -- | Number of noncompliant conformance packs.
    nonCompliantConformancePackCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateConformancePackComplianceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliantConformancePackCount', 'aggregateConformancePackComplianceCount_compliantConformancePackCount' - Number of compliant conformance packs.
--
-- 'nonCompliantConformancePackCount', 'aggregateConformancePackComplianceCount_nonCompliantConformancePackCount' - Number of noncompliant conformance packs.
newAggregateConformancePackComplianceCount ::
  AggregateConformancePackComplianceCount
newAggregateConformancePackComplianceCount =
  AggregateConformancePackComplianceCount'
    { compliantConformancePackCount =
        Prelude.Nothing,
      nonCompliantConformancePackCount =
        Prelude.Nothing
    }

-- | Number of compliant conformance packs.
aggregateConformancePackComplianceCount_compliantConformancePackCount :: Lens.Lens' AggregateConformancePackComplianceCount (Prelude.Maybe Prelude.Int)
aggregateConformancePackComplianceCount_compliantConformancePackCount = Lens.lens (\AggregateConformancePackComplianceCount' {compliantConformancePackCount} -> compliantConformancePackCount) (\s@AggregateConformancePackComplianceCount' {} a -> s {compliantConformancePackCount = a} :: AggregateConformancePackComplianceCount)

-- | Number of noncompliant conformance packs.
aggregateConformancePackComplianceCount_nonCompliantConformancePackCount :: Lens.Lens' AggregateConformancePackComplianceCount (Prelude.Maybe Prelude.Int)
aggregateConformancePackComplianceCount_nonCompliantConformancePackCount = Lens.lens (\AggregateConformancePackComplianceCount' {nonCompliantConformancePackCount} -> nonCompliantConformancePackCount) (\s@AggregateConformancePackComplianceCount' {} a -> s {nonCompliantConformancePackCount = a} :: AggregateConformancePackComplianceCount)

instance
  Core.FromJSON
    AggregateConformancePackComplianceCount
  where
  parseJSON =
    Core.withObject
      "AggregateConformancePackComplianceCount"
      ( \x ->
          AggregateConformancePackComplianceCount'
            Prelude.<$> (x Core..:? "CompliantConformancePackCount")
            Prelude.<*> (x Core..:? "NonCompliantConformancePackCount")
      )

instance
  Prelude.Hashable
    AggregateConformancePackComplianceCount
  where
  hashWithSalt
    _salt
    AggregateConformancePackComplianceCount' {..} =
      _salt
        `Prelude.hashWithSalt` compliantConformancePackCount
        `Prelude.hashWithSalt` nonCompliantConformancePackCount

instance
  Prelude.NFData
    AggregateConformancePackComplianceCount
  where
  rnf AggregateConformancePackComplianceCount' {..} =
    Prelude.rnf compliantConformancePackCount
      `Prelude.seq` Prelude.rnf nonCompliantConformancePackCount

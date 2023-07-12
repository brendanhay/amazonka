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
-- Module      : Amazonka.SageMaker.Types.DriftCheckModelDataQuality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DriftCheckModelDataQuality where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricsSource

-- | Represents the drift check data quality baselines that can be used when
-- the model monitor is set using the model package.
--
-- /See:/ 'newDriftCheckModelDataQuality' smart constructor.
data DriftCheckModelDataQuality = DriftCheckModelDataQuality'
  { -- | The drift check model data quality constraints.
    constraints :: Prelude.Maybe MetricsSource,
    -- | The drift check model data quality statistics.
    statistics :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DriftCheckModelDataQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'driftCheckModelDataQuality_constraints' - The drift check model data quality constraints.
--
-- 'statistics', 'driftCheckModelDataQuality_statistics' - The drift check model data quality statistics.
newDriftCheckModelDataQuality ::
  DriftCheckModelDataQuality
newDriftCheckModelDataQuality =
  DriftCheckModelDataQuality'
    { constraints =
        Prelude.Nothing,
      statistics = Prelude.Nothing
    }

-- | The drift check model data quality constraints.
driftCheckModelDataQuality_constraints :: Lens.Lens' DriftCheckModelDataQuality (Prelude.Maybe MetricsSource)
driftCheckModelDataQuality_constraints = Lens.lens (\DriftCheckModelDataQuality' {constraints} -> constraints) (\s@DriftCheckModelDataQuality' {} a -> s {constraints = a} :: DriftCheckModelDataQuality)

-- | The drift check model data quality statistics.
driftCheckModelDataQuality_statistics :: Lens.Lens' DriftCheckModelDataQuality (Prelude.Maybe MetricsSource)
driftCheckModelDataQuality_statistics = Lens.lens (\DriftCheckModelDataQuality' {statistics} -> statistics) (\s@DriftCheckModelDataQuality' {} a -> s {statistics = a} :: DriftCheckModelDataQuality)

instance Data.FromJSON DriftCheckModelDataQuality where
  parseJSON =
    Data.withObject
      "DriftCheckModelDataQuality"
      ( \x ->
          DriftCheckModelDataQuality'
            Prelude.<$> (x Data..:? "Constraints")
            Prelude.<*> (x Data..:? "Statistics")
      )

instance Prelude.Hashable DriftCheckModelDataQuality where
  hashWithSalt _salt DriftCheckModelDataQuality' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData DriftCheckModelDataQuality where
  rnf DriftCheckModelDataQuality' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf statistics

instance Data.ToJSON DriftCheckModelDataQuality where
  toJSON DriftCheckModelDataQuality' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Constraints" Data..=) Prelude.<$> constraints,
            ("Statistics" Data..=) Prelude.<$> statistics
          ]
      )

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
-- Module      : Amazonka.SageMaker.Types.ModelDataQuality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDataQuality where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricsSource

-- | Data quality constraints and statistics for a model.
--
-- /See:/ 'newModelDataQuality' smart constructor.
data ModelDataQuality = ModelDataQuality'
  { -- | Data quality constraints for a model.
    constraints :: Prelude.Maybe MetricsSource,
    -- | Data quality statistics for a model.
    statistics :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDataQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'modelDataQuality_constraints' - Data quality constraints for a model.
--
-- 'statistics', 'modelDataQuality_statistics' - Data quality statistics for a model.
newModelDataQuality ::
  ModelDataQuality
newModelDataQuality =
  ModelDataQuality'
    { constraints = Prelude.Nothing,
      statistics = Prelude.Nothing
    }

-- | Data quality constraints for a model.
modelDataQuality_constraints :: Lens.Lens' ModelDataQuality (Prelude.Maybe MetricsSource)
modelDataQuality_constraints = Lens.lens (\ModelDataQuality' {constraints} -> constraints) (\s@ModelDataQuality' {} a -> s {constraints = a} :: ModelDataQuality)

-- | Data quality statistics for a model.
modelDataQuality_statistics :: Lens.Lens' ModelDataQuality (Prelude.Maybe MetricsSource)
modelDataQuality_statistics = Lens.lens (\ModelDataQuality' {statistics} -> statistics) (\s@ModelDataQuality' {} a -> s {statistics = a} :: ModelDataQuality)

instance Data.FromJSON ModelDataQuality where
  parseJSON =
    Data.withObject
      "ModelDataQuality"
      ( \x ->
          ModelDataQuality'
            Prelude.<$> (x Data..:? "Constraints")
            Prelude.<*> (x Data..:? "Statistics")
      )

instance Prelude.Hashable ModelDataQuality where
  hashWithSalt _salt ModelDataQuality' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData ModelDataQuality where
  rnf ModelDataQuality' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf statistics

instance Data.ToJSON ModelDataQuality where
  toJSON ModelDataQuality' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Constraints" Data..=) Prelude.<$> constraints,
            ("Statistics" Data..=) Prelude.<$> statistics
          ]
      )

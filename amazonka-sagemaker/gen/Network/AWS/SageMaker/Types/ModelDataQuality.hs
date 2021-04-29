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
-- Module      : Network.AWS.SageMaker.Types.ModelDataQuality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelDataQuality where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetricsSource

-- | Data quality constraints and statistics for a model.
--
-- /See:/ 'newModelDataQuality' smart constructor.
data ModelDataQuality = ModelDataQuality'
  { -- | Data quality constraints for a model.
    constraints :: Prelude.Maybe MetricsSource,
    -- | Data quality statistics for a model.
    statistics :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ModelDataQuality where
  parseJSON =
    Prelude.withObject
      "ModelDataQuality"
      ( \x ->
          ModelDataQuality'
            Prelude.<$> (x Prelude..:? "Constraints")
            Prelude.<*> (x Prelude..:? "Statistics")
      )

instance Prelude.Hashable ModelDataQuality

instance Prelude.NFData ModelDataQuality

instance Prelude.ToJSON ModelDataQuality where
  toJSON ModelDataQuality' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Constraints" Prelude..=) Prelude.<$> constraints,
            ("Statistics" Prelude..=) Prelude.<$> statistics
          ]
      )

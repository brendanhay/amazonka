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
-- Module      : Network.AWS.SageMaker.Types.ModelQuality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelQuality where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetricsSource

-- | Model quality statistics and constraints.
--
-- /See:/ 'newModelQuality' smart constructor.
data ModelQuality = ModelQuality'
  { -- | Model quality constraints.
    constraints :: Prelude.Maybe MetricsSource,
    -- | Model quality statistics.
    statistics :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'modelQuality_constraints' - Model quality constraints.
--
-- 'statistics', 'modelQuality_statistics' - Model quality statistics.
newModelQuality ::
  ModelQuality
newModelQuality =
  ModelQuality'
    { constraints = Prelude.Nothing,
      statistics = Prelude.Nothing
    }

-- | Model quality constraints.
modelQuality_constraints :: Lens.Lens' ModelQuality (Prelude.Maybe MetricsSource)
modelQuality_constraints = Lens.lens (\ModelQuality' {constraints} -> constraints) (\s@ModelQuality' {} a -> s {constraints = a} :: ModelQuality)

-- | Model quality statistics.
modelQuality_statistics :: Lens.Lens' ModelQuality (Prelude.Maybe MetricsSource)
modelQuality_statistics = Lens.lens (\ModelQuality' {statistics} -> statistics) (\s@ModelQuality' {} a -> s {statistics = a} :: ModelQuality)

instance Prelude.FromJSON ModelQuality where
  parseJSON =
    Prelude.withObject
      "ModelQuality"
      ( \x ->
          ModelQuality'
            Prelude.<$> (x Prelude..:? "Constraints")
            Prelude.<*> (x Prelude..:? "Statistics")
      )

instance Prelude.Hashable ModelQuality

instance Prelude.NFData ModelQuality

instance Prelude.ToJSON ModelQuality where
  toJSON ModelQuality' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Constraints" Prelude..=) Prelude.<$> constraints,
            ("Statistics" Prelude..=) Prelude.<$> statistics
          ]
      )

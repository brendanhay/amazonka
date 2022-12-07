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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobResourceLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobResourceLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the maximum number of jobs that can run in parallel and the
-- maximum number of jobs that can run.
--
-- /See:/ 'newRecommendationJobResourceLimit' smart constructor.
data RecommendationJobResourceLimit = RecommendationJobResourceLimit'
  { -- | Defines the maximum number of load tests.
    maxNumberOfTests :: Prelude.Maybe Prelude.Natural,
    -- | Defines the maximum number of parallel load tests.
    maxParallelOfTests :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobResourceLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxNumberOfTests', 'recommendationJobResourceLimit_maxNumberOfTests' - Defines the maximum number of load tests.
--
-- 'maxParallelOfTests', 'recommendationJobResourceLimit_maxParallelOfTests' - Defines the maximum number of parallel load tests.
newRecommendationJobResourceLimit ::
  RecommendationJobResourceLimit
newRecommendationJobResourceLimit =
  RecommendationJobResourceLimit'
    { maxNumberOfTests =
        Prelude.Nothing,
      maxParallelOfTests = Prelude.Nothing
    }

-- | Defines the maximum number of load tests.
recommendationJobResourceLimit_maxNumberOfTests :: Lens.Lens' RecommendationJobResourceLimit (Prelude.Maybe Prelude.Natural)
recommendationJobResourceLimit_maxNumberOfTests = Lens.lens (\RecommendationJobResourceLimit' {maxNumberOfTests} -> maxNumberOfTests) (\s@RecommendationJobResourceLimit' {} a -> s {maxNumberOfTests = a} :: RecommendationJobResourceLimit)

-- | Defines the maximum number of parallel load tests.
recommendationJobResourceLimit_maxParallelOfTests :: Lens.Lens' RecommendationJobResourceLimit (Prelude.Maybe Prelude.Natural)
recommendationJobResourceLimit_maxParallelOfTests = Lens.lens (\RecommendationJobResourceLimit' {maxParallelOfTests} -> maxParallelOfTests) (\s@RecommendationJobResourceLimit' {} a -> s {maxParallelOfTests = a} :: RecommendationJobResourceLimit)

instance Data.FromJSON RecommendationJobResourceLimit where
  parseJSON =
    Data.withObject
      "RecommendationJobResourceLimit"
      ( \x ->
          RecommendationJobResourceLimit'
            Prelude.<$> (x Data..:? "MaxNumberOfTests")
            Prelude.<*> (x Data..:? "MaxParallelOfTests")
      )

instance
  Prelude.Hashable
    RecommendationJobResourceLimit
  where
  hashWithSalt
    _salt
    RecommendationJobResourceLimit' {..} =
      _salt `Prelude.hashWithSalt` maxNumberOfTests
        `Prelude.hashWithSalt` maxParallelOfTests

instance
  Prelude.NFData
    RecommendationJobResourceLimit
  where
  rnf RecommendationJobResourceLimit' {..} =
    Prelude.rnf maxNumberOfTests
      `Prelude.seq` Prelude.rnf maxParallelOfTests

instance Data.ToJSON RecommendationJobResourceLimit where
  toJSON RecommendationJobResourceLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxNumberOfTests" Data..=)
              Prelude.<$> maxNumberOfTests,
            ("MaxParallelOfTests" Data..=)
              Prelude.<$> maxParallelOfTests
          ]
      )

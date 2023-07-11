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
-- Module      : Amazonka.Personalize.Types.HPOResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.HPOResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the resource configuration for hyperparameter optimization
-- (HPO).
--
-- /See:/ 'newHPOResourceConfig' smart constructor.
data HPOResourceConfig = HPOResourceConfig'
  { -- | The maximum number of training jobs when you create a solution version.
    -- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
    maxNumberOfTrainingJobs :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of parallel training jobs when you create a solution
    -- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
    maxParallelTrainingJobs :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HPOResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxNumberOfTrainingJobs', 'hPOResourceConfig_maxNumberOfTrainingJobs' - The maximum number of training jobs when you create a solution version.
-- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
--
-- 'maxParallelTrainingJobs', 'hPOResourceConfig_maxParallelTrainingJobs' - The maximum number of parallel training jobs when you create a solution
-- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
newHPOResourceConfig ::
  HPOResourceConfig
newHPOResourceConfig =
  HPOResourceConfig'
    { maxNumberOfTrainingJobs =
        Prelude.Nothing,
      maxParallelTrainingJobs = Prelude.Nothing
    }

-- | The maximum number of training jobs when you create a solution version.
-- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
hPOResourceConfig_maxNumberOfTrainingJobs :: Lens.Lens' HPOResourceConfig (Prelude.Maybe Prelude.Text)
hPOResourceConfig_maxNumberOfTrainingJobs = Lens.lens (\HPOResourceConfig' {maxNumberOfTrainingJobs} -> maxNumberOfTrainingJobs) (\s@HPOResourceConfig' {} a -> s {maxNumberOfTrainingJobs = a} :: HPOResourceConfig)

-- | The maximum number of parallel training jobs when you create a solution
-- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
hPOResourceConfig_maxParallelTrainingJobs :: Lens.Lens' HPOResourceConfig (Prelude.Maybe Prelude.Text)
hPOResourceConfig_maxParallelTrainingJobs = Lens.lens (\HPOResourceConfig' {maxParallelTrainingJobs} -> maxParallelTrainingJobs) (\s@HPOResourceConfig' {} a -> s {maxParallelTrainingJobs = a} :: HPOResourceConfig)

instance Data.FromJSON HPOResourceConfig where
  parseJSON =
    Data.withObject
      "HPOResourceConfig"
      ( \x ->
          HPOResourceConfig'
            Prelude.<$> (x Data..:? "maxNumberOfTrainingJobs")
            Prelude.<*> (x Data..:? "maxParallelTrainingJobs")
      )

instance Prelude.Hashable HPOResourceConfig where
  hashWithSalt _salt HPOResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` maxNumberOfTrainingJobs
      `Prelude.hashWithSalt` maxParallelTrainingJobs

instance Prelude.NFData HPOResourceConfig where
  rnf HPOResourceConfig' {..} =
    Prelude.rnf maxNumberOfTrainingJobs
      `Prelude.seq` Prelude.rnf maxParallelTrainingJobs

instance Data.ToJSON HPOResourceConfig where
  toJSON HPOResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxNumberOfTrainingJobs" Data..=)
              Prelude.<$> maxNumberOfTrainingJobs,
            ("maxParallelTrainingJobs" Data..=)
              Prelude.<$> maxParallelTrainingJobs
          ]
      )

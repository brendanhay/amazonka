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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The maximum number of parallel training jobs when you create a solution
    -- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
    maxParallelTrainingJobs :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of training jobs when you create a solution version.
    -- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
    maxNumberOfTrainingJobs :: Prelude.Maybe Prelude.Text
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
-- 'maxParallelTrainingJobs', 'hPOResourceConfig_maxParallelTrainingJobs' - The maximum number of parallel training jobs when you create a solution
-- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
--
-- 'maxNumberOfTrainingJobs', 'hPOResourceConfig_maxNumberOfTrainingJobs' - The maximum number of training jobs when you create a solution version.
-- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
newHPOResourceConfig ::
  HPOResourceConfig
newHPOResourceConfig =
  HPOResourceConfig'
    { maxParallelTrainingJobs =
        Prelude.Nothing,
      maxNumberOfTrainingJobs = Prelude.Nothing
    }

-- | The maximum number of parallel training jobs when you create a solution
-- version. The maximum value for @maxParallelTrainingJobs@ is @10@.
hPOResourceConfig_maxParallelTrainingJobs :: Lens.Lens' HPOResourceConfig (Prelude.Maybe Prelude.Text)
hPOResourceConfig_maxParallelTrainingJobs = Lens.lens (\HPOResourceConfig' {maxParallelTrainingJobs} -> maxParallelTrainingJobs) (\s@HPOResourceConfig' {} a -> s {maxParallelTrainingJobs = a} :: HPOResourceConfig)

-- | The maximum number of training jobs when you create a solution version.
-- The maximum value for @maxNumberOfTrainingJobs@ is @40@.
hPOResourceConfig_maxNumberOfTrainingJobs :: Lens.Lens' HPOResourceConfig (Prelude.Maybe Prelude.Text)
hPOResourceConfig_maxNumberOfTrainingJobs = Lens.lens (\HPOResourceConfig' {maxNumberOfTrainingJobs} -> maxNumberOfTrainingJobs) (\s@HPOResourceConfig' {} a -> s {maxNumberOfTrainingJobs = a} :: HPOResourceConfig)

instance Data.FromJSON HPOResourceConfig where
  parseJSON =
    Data.withObject
      "HPOResourceConfig"
      ( \x ->
          HPOResourceConfig'
            Prelude.<$> (x Data..:? "maxParallelTrainingJobs")
            Prelude.<*> (x Data..:? "maxNumberOfTrainingJobs")
      )

instance Prelude.Hashable HPOResourceConfig where
  hashWithSalt _salt HPOResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` maxParallelTrainingJobs
      `Prelude.hashWithSalt` maxNumberOfTrainingJobs

instance Prelude.NFData HPOResourceConfig where
  rnf HPOResourceConfig' {..} =
    Prelude.rnf maxParallelTrainingJobs
      `Prelude.seq` Prelude.rnf maxNumberOfTrainingJobs

instance Data.ToJSON HPOResourceConfig where
  toJSON HPOResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxParallelTrainingJobs" Data..=)
              Prelude.<$> maxParallelTrainingJobs,
            ("maxNumberOfTrainingJobs" Data..=)
              Prelude.<$> maxNumberOfTrainingJobs
          ]
      )

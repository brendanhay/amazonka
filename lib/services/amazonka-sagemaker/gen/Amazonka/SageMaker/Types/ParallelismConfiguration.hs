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
-- Module      : Amazonka.SageMaker.Types.ParallelismConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParallelismConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration that controls the parallelism of the pipeline. By default,
-- the parallelism configuration specified applies to all executions of the
-- pipeline unless overridden.
--
-- /See:/ 'newParallelismConfiguration' smart constructor.
data ParallelismConfiguration = ParallelismConfiguration'
  { -- | The max number of steps that can be executed in parallel.
    maxParallelExecutionSteps :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelismConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxParallelExecutionSteps', 'parallelismConfiguration_maxParallelExecutionSteps' - The max number of steps that can be executed in parallel.
newParallelismConfiguration ::
  -- | 'maxParallelExecutionSteps'
  Prelude.Natural ->
  ParallelismConfiguration
newParallelismConfiguration
  pMaxParallelExecutionSteps_ =
    ParallelismConfiguration'
      { maxParallelExecutionSteps =
          pMaxParallelExecutionSteps_
      }

-- | The max number of steps that can be executed in parallel.
parallelismConfiguration_maxParallelExecutionSteps :: Lens.Lens' ParallelismConfiguration Prelude.Natural
parallelismConfiguration_maxParallelExecutionSteps = Lens.lens (\ParallelismConfiguration' {maxParallelExecutionSteps} -> maxParallelExecutionSteps) (\s@ParallelismConfiguration' {} a -> s {maxParallelExecutionSteps = a} :: ParallelismConfiguration)

instance Data.FromJSON ParallelismConfiguration where
  parseJSON =
    Data.withObject
      "ParallelismConfiguration"
      ( \x ->
          ParallelismConfiguration'
            Prelude.<$> (x Data..: "MaxParallelExecutionSteps")
      )

instance Prelude.Hashable ParallelismConfiguration where
  hashWithSalt _salt ParallelismConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` maxParallelExecutionSteps

instance Prelude.NFData ParallelismConfiguration where
  rnf ParallelismConfiguration' {..} =
    Prelude.rnf maxParallelExecutionSteps

instance Data.ToJSON ParallelismConfiguration where
  toJSON ParallelismConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MaxParallelExecutionSteps"
                  Data..= maxParallelExecutionSteps
              )
          ]
      )

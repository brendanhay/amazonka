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
-- Module      : Amazonka.CodePipeline.Types.ExecutorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ExecutorConfiguration where

import Amazonka.CodePipeline.Types.JobWorkerExecutorConfiguration
import Amazonka.CodePipeline.Types.LambdaExecutorConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action engine, or executor, related to the supported integration
-- model used to create and update the action type. The available executor
-- types are @Lambda@ and @JobWorker@.
--
-- /See:/ 'newExecutorConfiguration' smart constructor.
data ExecutorConfiguration = ExecutorConfiguration'
  { -- | Details about the @JobWorker@ executor of the action type.
    jobWorkerExecutorConfiguration :: Prelude.Maybe JobWorkerExecutorConfiguration,
    -- | Details about the @Lambda@ executor of the action type.
    lambdaExecutorConfiguration :: Prelude.Maybe LambdaExecutorConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobWorkerExecutorConfiguration', 'executorConfiguration_jobWorkerExecutorConfiguration' - Details about the @JobWorker@ executor of the action type.
--
-- 'lambdaExecutorConfiguration', 'executorConfiguration_lambdaExecutorConfiguration' - Details about the @Lambda@ executor of the action type.
newExecutorConfiguration ::
  ExecutorConfiguration
newExecutorConfiguration =
  ExecutorConfiguration'
    { jobWorkerExecutorConfiguration =
        Prelude.Nothing,
      lambdaExecutorConfiguration = Prelude.Nothing
    }

-- | Details about the @JobWorker@ executor of the action type.
executorConfiguration_jobWorkerExecutorConfiguration :: Lens.Lens' ExecutorConfiguration (Prelude.Maybe JobWorkerExecutorConfiguration)
executorConfiguration_jobWorkerExecutorConfiguration = Lens.lens (\ExecutorConfiguration' {jobWorkerExecutorConfiguration} -> jobWorkerExecutorConfiguration) (\s@ExecutorConfiguration' {} a -> s {jobWorkerExecutorConfiguration = a} :: ExecutorConfiguration)

-- | Details about the @Lambda@ executor of the action type.
executorConfiguration_lambdaExecutorConfiguration :: Lens.Lens' ExecutorConfiguration (Prelude.Maybe LambdaExecutorConfiguration)
executorConfiguration_lambdaExecutorConfiguration = Lens.lens (\ExecutorConfiguration' {lambdaExecutorConfiguration} -> lambdaExecutorConfiguration) (\s@ExecutorConfiguration' {} a -> s {lambdaExecutorConfiguration = a} :: ExecutorConfiguration)

instance Data.FromJSON ExecutorConfiguration where
  parseJSON =
    Data.withObject
      "ExecutorConfiguration"
      ( \x ->
          ExecutorConfiguration'
            Prelude.<$> (x Data..:? "jobWorkerExecutorConfiguration")
            Prelude.<*> (x Data..:? "lambdaExecutorConfiguration")
      )

instance Prelude.Hashable ExecutorConfiguration where
  hashWithSalt _salt ExecutorConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` jobWorkerExecutorConfiguration
      `Prelude.hashWithSalt` lambdaExecutorConfiguration

instance Prelude.NFData ExecutorConfiguration where
  rnf ExecutorConfiguration' {..} =
    Prelude.rnf jobWorkerExecutorConfiguration
      `Prelude.seq` Prelude.rnf lambdaExecutorConfiguration

instance Data.ToJSON ExecutorConfiguration where
  toJSON ExecutorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobWorkerExecutorConfiguration" Data..=)
              Prelude.<$> jobWorkerExecutorConfiguration,
            ("lambdaExecutorConfiguration" Data..=)
              Prelude.<$> lambdaExecutorConfiguration
          ]
      )

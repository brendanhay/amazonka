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
-- Module      : Network.AWS.CodePipeline.Types.ExecutorConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutorConfiguration where

import Network.AWS.CodePipeline.Types.JobWorkerExecutorConfiguration
import Network.AWS.CodePipeline.Types.LambdaExecutorConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The action engine, or executor, related to the supported integration
-- model used to create and update the action type. The available executor
-- types are @Lambda@ and @JobWorker@.
--
-- /See:/ 'newExecutorConfiguration' smart constructor.
data ExecutorConfiguration = ExecutorConfiguration'
  { -- | Details about the @Lambda@ executor of the action type.
    lambdaExecutorConfiguration :: Prelude.Maybe LambdaExecutorConfiguration,
    -- | Details about the @JobWorker@ executor of the action type.
    jobWorkerExecutorConfiguration :: Prelude.Maybe JobWorkerExecutorConfiguration
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
-- 'lambdaExecutorConfiguration', 'executorConfiguration_lambdaExecutorConfiguration' - Details about the @Lambda@ executor of the action type.
--
-- 'jobWorkerExecutorConfiguration', 'executorConfiguration_jobWorkerExecutorConfiguration' - Details about the @JobWorker@ executor of the action type.
newExecutorConfiguration ::
  ExecutorConfiguration
newExecutorConfiguration =
  ExecutorConfiguration'
    { lambdaExecutorConfiguration =
        Prelude.Nothing,
      jobWorkerExecutorConfiguration = Prelude.Nothing
    }

-- | Details about the @Lambda@ executor of the action type.
executorConfiguration_lambdaExecutorConfiguration :: Lens.Lens' ExecutorConfiguration (Prelude.Maybe LambdaExecutorConfiguration)
executorConfiguration_lambdaExecutorConfiguration = Lens.lens (\ExecutorConfiguration' {lambdaExecutorConfiguration} -> lambdaExecutorConfiguration) (\s@ExecutorConfiguration' {} a -> s {lambdaExecutorConfiguration = a} :: ExecutorConfiguration)

-- | Details about the @JobWorker@ executor of the action type.
executorConfiguration_jobWorkerExecutorConfiguration :: Lens.Lens' ExecutorConfiguration (Prelude.Maybe JobWorkerExecutorConfiguration)
executorConfiguration_jobWorkerExecutorConfiguration = Lens.lens (\ExecutorConfiguration' {jobWorkerExecutorConfiguration} -> jobWorkerExecutorConfiguration) (\s@ExecutorConfiguration' {} a -> s {jobWorkerExecutorConfiguration = a} :: ExecutorConfiguration)

instance Core.FromJSON ExecutorConfiguration where
  parseJSON =
    Core.withObject
      "ExecutorConfiguration"
      ( \x ->
          ExecutorConfiguration'
            Prelude.<$> (x Core..:? "lambdaExecutorConfiguration")
            Prelude.<*> (x Core..:? "jobWorkerExecutorConfiguration")
      )

instance Prelude.Hashable ExecutorConfiguration

instance Prelude.NFData ExecutorConfiguration

instance Core.ToJSON ExecutorConfiguration where
  toJSON ExecutorConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("lambdaExecutorConfiguration" Core..=)
              Prelude.<$> lambdaExecutorConfiguration,
            ("jobWorkerExecutorConfiguration" Core..=)
              Prelude.<$> jobWorkerExecutorConfiguration
          ]
      )

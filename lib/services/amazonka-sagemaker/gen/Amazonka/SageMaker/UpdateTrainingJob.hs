{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.UpdateTrainingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a model training job to request a new Debugger profiling
-- configuration or to change warm pool retention length.
module Amazonka.SageMaker.UpdateTrainingJob
  ( -- * Creating a Request
    UpdateTrainingJob (..),
    newUpdateTrainingJob,

    -- * Request Lenses
    updateTrainingJob_profilerConfig,
    updateTrainingJob_profilerRuleConfigurations,
    updateTrainingJob_resourceConfig,
    updateTrainingJob_trainingJobName,

    -- * Destructuring the Response
    UpdateTrainingJobResponse (..),
    newUpdateTrainingJobResponse,

    -- * Response Lenses
    updateTrainingJobResponse_httpStatus,
    updateTrainingJobResponse_trainingJobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateTrainingJob' smart constructor.
data UpdateTrainingJob = UpdateTrainingJob'
  { -- | Configuration information for Debugger system monitoring, framework
    -- profiling, and storage paths.
    profilerConfig :: Prelude.Maybe ProfilerConfigForUpdate,
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Prelude.Maybe [ProfilerRuleConfiguration],
    -- | The training job @ResourceConfig@ to update warm pool retention length.
    resourceConfig :: Prelude.Maybe ResourceConfigForUpdate,
    -- | The name of a training job to update the Debugger profiling
    -- configuration.
    trainingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilerConfig', 'updateTrainingJob_profilerConfig' - Configuration information for Debugger system monitoring, framework
-- profiling, and storage paths.
--
-- 'profilerRuleConfigurations', 'updateTrainingJob_profilerRuleConfigurations' - Configuration information for Debugger rules for profiling system and
-- framework metrics.
--
-- 'resourceConfig', 'updateTrainingJob_resourceConfig' - The training job @ResourceConfig@ to update warm pool retention length.
--
-- 'trainingJobName', 'updateTrainingJob_trainingJobName' - The name of a training job to update the Debugger profiling
-- configuration.
newUpdateTrainingJob ::
  -- | 'trainingJobName'
  Prelude.Text ->
  UpdateTrainingJob
newUpdateTrainingJob pTrainingJobName_ =
  UpdateTrainingJob'
    { profilerConfig =
        Prelude.Nothing,
      profilerRuleConfigurations = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      trainingJobName = pTrainingJobName_
    }

-- | Configuration information for Debugger system monitoring, framework
-- profiling, and storage paths.
updateTrainingJob_profilerConfig :: Lens.Lens' UpdateTrainingJob (Prelude.Maybe ProfilerConfigForUpdate)
updateTrainingJob_profilerConfig = Lens.lens (\UpdateTrainingJob' {profilerConfig} -> profilerConfig) (\s@UpdateTrainingJob' {} a -> s {profilerConfig = a} :: UpdateTrainingJob)

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
updateTrainingJob_profilerRuleConfigurations :: Lens.Lens' UpdateTrainingJob (Prelude.Maybe [ProfilerRuleConfiguration])
updateTrainingJob_profilerRuleConfigurations = Lens.lens (\UpdateTrainingJob' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@UpdateTrainingJob' {} a -> s {profilerRuleConfigurations = a} :: UpdateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The training job @ResourceConfig@ to update warm pool retention length.
updateTrainingJob_resourceConfig :: Lens.Lens' UpdateTrainingJob (Prelude.Maybe ResourceConfigForUpdate)
updateTrainingJob_resourceConfig = Lens.lens (\UpdateTrainingJob' {resourceConfig} -> resourceConfig) (\s@UpdateTrainingJob' {} a -> s {resourceConfig = a} :: UpdateTrainingJob)

-- | The name of a training job to update the Debugger profiling
-- configuration.
updateTrainingJob_trainingJobName :: Lens.Lens' UpdateTrainingJob Prelude.Text
updateTrainingJob_trainingJobName = Lens.lens (\UpdateTrainingJob' {trainingJobName} -> trainingJobName) (\s@UpdateTrainingJob' {} a -> s {trainingJobName = a} :: UpdateTrainingJob)

instance Core.AWSRequest UpdateTrainingJob where
  type
    AWSResponse UpdateTrainingJob =
      UpdateTrainingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrainingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TrainingJobArn")
      )

instance Prelude.Hashable UpdateTrainingJob where
  hashWithSalt _salt UpdateTrainingJob' {..} =
    _salt `Prelude.hashWithSalt` profilerConfig
      `Prelude.hashWithSalt` profilerRuleConfigurations
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` trainingJobName

instance Prelude.NFData UpdateTrainingJob where
  rnf UpdateTrainingJob' {..} =
    Prelude.rnf profilerConfig
      `Prelude.seq` Prelude.rnf profilerRuleConfigurations
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf trainingJobName

instance Core.ToHeaders UpdateTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateTrainingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTrainingJob where
  toJSON UpdateTrainingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProfilerConfig" Core..=)
              Prelude.<$> profilerConfig,
            ("ProfilerRuleConfigurations" Core..=)
              Prelude.<$> profilerRuleConfigurations,
            ("ResourceConfig" Core..=)
              Prelude.<$> resourceConfig,
            Prelude.Just
              ("TrainingJobName" Core..= trainingJobName)
          ]
      )

instance Core.ToPath UpdateTrainingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTrainingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrainingJobResponse' smart constructor.
data UpdateTrainingJobResponse = UpdateTrainingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrainingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTrainingJobResponse_httpStatus' - The response's http status code.
--
-- 'trainingJobArn', 'updateTrainingJobResponse_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
newUpdateTrainingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trainingJobArn'
  Prelude.Text ->
  UpdateTrainingJobResponse
newUpdateTrainingJobResponse
  pHttpStatus_
  pTrainingJobArn_ =
    UpdateTrainingJobResponse'
      { httpStatus =
          pHttpStatus_,
        trainingJobArn = pTrainingJobArn_
      }

-- | The response's http status code.
updateTrainingJobResponse_httpStatus :: Lens.Lens' UpdateTrainingJobResponse Prelude.Int
updateTrainingJobResponse_httpStatus = Lens.lens (\UpdateTrainingJobResponse' {httpStatus} -> httpStatus) (\s@UpdateTrainingJobResponse' {} a -> s {httpStatus = a} :: UpdateTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the training job.
updateTrainingJobResponse_trainingJobArn :: Lens.Lens' UpdateTrainingJobResponse Prelude.Text
updateTrainingJobResponse_trainingJobArn = Lens.lens (\UpdateTrainingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@UpdateTrainingJobResponse' {} a -> s {trainingJobArn = a} :: UpdateTrainingJobResponse)

instance Prelude.NFData UpdateTrainingJobResponse where
  rnf UpdateTrainingJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trainingJobArn

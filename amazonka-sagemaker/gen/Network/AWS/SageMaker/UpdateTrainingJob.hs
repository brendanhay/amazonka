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
-- Module      : Network.AWS.SageMaker.UpdateTrainingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a model training job to request a new Debugger profiling
-- configuration.
module Network.AWS.SageMaker.UpdateTrainingJob
  ( -- * Creating a Request
    UpdateTrainingJob (..),
    newUpdateTrainingJob,

    -- * Request Lenses
    updateTrainingJob_profilerConfig,
    updateTrainingJob_profilerRuleConfigurations,
    updateTrainingJob_trainingJobName,

    -- * Destructuring the Response
    UpdateTrainingJobResponse (..),
    newUpdateTrainingJobResponse,

    -- * Response Lenses
    updateTrainingJobResponse_httpStatus,
    updateTrainingJobResponse_trainingJobArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrainingJob' smart constructor.
data UpdateTrainingJob = UpdateTrainingJob'
  { -- | Configuration information for Debugger system monitoring, framework
    -- profiling, and storage paths.
    profilerConfig :: Core.Maybe ProfilerConfigForUpdate,
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Core.Maybe [ProfilerRuleConfiguration],
    -- | The name of a training job to update the Debugger profiling
    -- configuration.
    trainingJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'trainingJobName', 'updateTrainingJob_trainingJobName' - The name of a training job to update the Debugger profiling
-- configuration.
newUpdateTrainingJob ::
  -- | 'trainingJobName'
  Core.Text ->
  UpdateTrainingJob
newUpdateTrainingJob pTrainingJobName_ =
  UpdateTrainingJob'
    { profilerConfig = Core.Nothing,
      profilerRuleConfigurations = Core.Nothing,
      trainingJobName = pTrainingJobName_
    }

-- | Configuration information for Debugger system monitoring, framework
-- profiling, and storage paths.
updateTrainingJob_profilerConfig :: Lens.Lens' UpdateTrainingJob (Core.Maybe ProfilerConfigForUpdate)
updateTrainingJob_profilerConfig = Lens.lens (\UpdateTrainingJob' {profilerConfig} -> profilerConfig) (\s@UpdateTrainingJob' {} a -> s {profilerConfig = a} :: UpdateTrainingJob)

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
updateTrainingJob_profilerRuleConfigurations :: Lens.Lens' UpdateTrainingJob (Core.Maybe [ProfilerRuleConfiguration])
updateTrainingJob_profilerRuleConfigurations = Lens.lens (\UpdateTrainingJob' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@UpdateTrainingJob' {} a -> s {profilerRuleConfigurations = a} :: UpdateTrainingJob) Core.. Lens.mapping Lens._Coerce

-- | The name of a training job to update the Debugger profiling
-- configuration.
updateTrainingJob_trainingJobName :: Lens.Lens' UpdateTrainingJob Core.Text
updateTrainingJob_trainingJobName = Lens.lens (\UpdateTrainingJob' {trainingJobName} -> trainingJobName) (\s@UpdateTrainingJob' {} a -> s {trainingJobName = a} :: UpdateTrainingJob)

instance Core.AWSRequest UpdateTrainingJob where
  type
    AWSResponse UpdateTrainingJob =
      UpdateTrainingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrainingJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "TrainingJobArn")
      )

instance Core.Hashable UpdateTrainingJob

instance Core.NFData UpdateTrainingJob

instance Core.ToHeaders UpdateTrainingJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateTrainingJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTrainingJob where
  toJSON UpdateTrainingJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProfilerConfig" Core..=) Core.<$> profilerConfig,
            ("ProfilerRuleConfigurations" Core..=)
              Core.<$> profilerRuleConfigurations,
            Core.Just
              ("TrainingJobName" Core..= trainingJobName)
          ]
      )

instance Core.ToPath UpdateTrainingJob where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTrainingJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTrainingJobResponse' smart constructor.
data UpdateTrainingJobResponse = UpdateTrainingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'trainingJobArn'
  Core.Text ->
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
updateTrainingJobResponse_httpStatus :: Lens.Lens' UpdateTrainingJobResponse Core.Int
updateTrainingJobResponse_httpStatus = Lens.lens (\UpdateTrainingJobResponse' {httpStatus} -> httpStatus) (\s@UpdateTrainingJobResponse' {} a -> s {httpStatus = a} :: UpdateTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the training job.
updateTrainingJobResponse_trainingJobArn :: Lens.Lens' UpdateTrainingJobResponse Core.Text
updateTrainingJobResponse_trainingJobArn = Lens.lens (\UpdateTrainingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@UpdateTrainingJobResponse' {} a -> s {trainingJobArn = a} :: UpdateTrainingJobResponse)

instance Core.NFData UpdateTrainingJobResponse

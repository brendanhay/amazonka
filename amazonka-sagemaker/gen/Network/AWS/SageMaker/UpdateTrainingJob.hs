{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrainingJob' smart constructor.
data UpdateTrainingJob = UpdateTrainingJob'
  { -- | Configuration information for Debugger system monitoring, framework
    -- profiling, and storage paths.
    profilerConfig :: Prelude.Maybe ProfilerConfigForUpdate,
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Prelude.Maybe [ProfilerRuleConfiguration],
    -- | The name of a training job to update the Debugger profiling
    -- configuration.
    trainingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateTrainingJob
newUpdateTrainingJob pTrainingJobName_ =
  UpdateTrainingJob'
    { profilerConfig =
        Prelude.Nothing,
      profilerRuleConfigurations = Prelude.Nothing,
      trainingJobName = pTrainingJobName_
    }

-- | Configuration information for Debugger system monitoring, framework
-- profiling, and storage paths.
updateTrainingJob_profilerConfig :: Lens.Lens' UpdateTrainingJob (Prelude.Maybe ProfilerConfigForUpdate)
updateTrainingJob_profilerConfig = Lens.lens (\UpdateTrainingJob' {profilerConfig} -> profilerConfig) (\s@UpdateTrainingJob' {} a -> s {profilerConfig = a} :: UpdateTrainingJob)

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
updateTrainingJob_profilerRuleConfigurations :: Lens.Lens' UpdateTrainingJob (Prelude.Maybe [ProfilerRuleConfiguration])
updateTrainingJob_profilerRuleConfigurations = Lens.lens (\UpdateTrainingJob' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@UpdateTrainingJob' {} a -> s {profilerRuleConfigurations = a} :: UpdateTrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of a training job to update the Debugger profiling
-- configuration.
updateTrainingJob_trainingJobName :: Lens.Lens' UpdateTrainingJob Prelude.Text
updateTrainingJob_trainingJobName = Lens.lens (\UpdateTrainingJob' {trainingJobName} -> trainingJobName) (\s@UpdateTrainingJob' {} a -> s {trainingJobName = a} :: UpdateTrainingJob)

instance Prelude.AWSRequest UpdateTrainingJob where
  type Rs UpdateTrainingJob = UpdateTrainingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrainingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "TrainingJobArn")
      )

instance Prelude.Hashable UpdateTrainingJob

instance Prelude.NFData UpdateTrainingJob

instance Prelude.ToHeaders UpdateTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.UpdateTrainingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTrainingJob where
  toJSON UpdateTrainingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ProfilerConfig" Prelude..=)
              Prelude.<$> profilerConfig,
            ("ProfilerRuleConfigurations" Prelude..=)
              Prelude.<$> profilerRuleConfigurations,
            Prelude.Just
              ("TrainingJobName" Prelude..= trainingJobName)
          ]
      )

instance Prelude.ToPath UpdateTrainingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTrainingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrainingJobResponse' smart constructor.
data UpdateTrainingJobResponse = UpdateTrainingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateTrainingJobResponse

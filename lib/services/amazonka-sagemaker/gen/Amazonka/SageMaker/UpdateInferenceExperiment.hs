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
-- Module      : Amazonka.SageMaker.UpdateInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an inference experiment that you created. The status of the
-- inference experiment has to be either @Created@, @Running@. For more
-- information on the status of an inference experiment, see
-- DescribeInferenceExperimentResponse$Status.
module Amazonka.SageMaker.UpdateInferenceExperiment
  ( -- * Creating a Request
    UpdateInferenceExperiment (..),
    newUpdateInferenceExperiment,

    -- * Request Lenses
    updateInferenceExperiment_dataStorageConfig,
    updateInferenceExperiment_description,
    updateInferenceExperiment_modelVariants,
    updateInferenceExperiment_schedule,
    updateInferenceExperiment_shadowModeConfig,
    updateInferenceExperiment_name,

    -- * Destructuring the Response
    UpdateInferenceExperimentResponse (..),
    newUpdateInferenceExperimentResponse,

    -- * Response Lenses
    updateInferenceExperimentResponse_httpStatus,
    updateInferenceExperimentResponse_inferenceExperimentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateInferenceExperiment' smart constructor.
data UpdateInferenceExperiment = UpdateInferenceExperiment'
  { -- | The Amazon S3 location and configuration for storing inference request
    -- and response data.
    dataStorageConfig :: Prelude.Maybe InferenceExperimentDataStorageConfig,
    -- | The description of the inference experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of @ModelVariantConfig@ objects. There is one for each variant,
    -- whose infrastructure configuration you want to update.
    modelVariants :: Prelude.Maybe (Prelude.NonEmpty ModelVariantConfig),
    -- | The duration for which the inference experiment will run. If the status
    -- of the inference experiment is @Created@, then you can update both the
    -- start and end dates. If the status of the inference experiment is
    -- @Running@, then you can update only the end date.
    schedule :: Prelude.Maybe InferenceExperimentSchedule,
    -- | The configuration of @ShadowMode@ inference experiment type. Use this
    -- field to specify a production variant which takes all the inference
    -- requests, and a shadow variant to which Amazon SageMaker replicates a
    -- percentage of the inference requests. For the shadow variant also
    -- specify the percentage of requests that Amazon SageMaker replicates.
    shadowModeConfig :: Prelude.Maybe ShadowModeConfig,
    -- | The name of the inference experiment to be updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataStorageConfig', 'updateInferenceExperiment_dataStorageConfig' - The Amazon S3 location and configuration for storing inference request
-- and response data.
--
-- 'description', 'updateInferenceExperiment_description' - The description of the inference experiment.
--
-- 'modelVariants', 'updateInferenceExperiment_modelVariants' - An array of @ModelVariantConfig@ objects. There is one for each variant,
-- whose infrastructure configuration you want to update.
--
-- 'schedule', 'updateInferenceExperiment_schedule' - The duration for which the inference experiment will run. If the status
-- of the inference experiment is @Created@, then you can update both the
-- start and end dates. If the status of the inference experiment is
-- @Running@, then you can update only the end date.
--
-- 'shadowModeConfig', 'updateInferenceExperiment_shadowModeConfig' - The configuration of @ShadowMode@ inference experiment type. Use this
-- field to specify a production variant which takes all the inference
-- requests, and a shadow variant to which Amazon SageMaker replicates a
-- percentage of the inference requests. For the shadow variant also
-- specify the percentage of requests that Amazon SageMaker replicates.
--
-- 'name', 'updateInferenceExperiment_name' - The name of the inference experiment to be updated.
newUpdateInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  UpdateInferenceExperiment
newUpdateInferenceExperiment pName_ =
  UpdateInferenceExperiment'
    { dataStorageConfig =
        Prelude.Nothing,
      description = Prelude.Nothing,
      modelVariants = Prelude.Nothing,
      schedule = Prelude.Nothing,
      shadowModeConfig = Prelude.Nothing,
      name = pName_
    }

-- | The Amazon S3 location and configuration for storing inference request
-- and response data.
updateInferenceExperiment_dataStorageConfig :: Lens.Lens' UpdateInferenceExperiment (Prelude.Maybe InferenceExperimentDataStorageConfig)
updateInferenceExperiment_dataStorageConfig = Lens.lens (\UpdateInferenceExperiment' {dataStorageConfig} -> dataStorageConfig) (\s@UpdateInferenceExperiment' {} a -> s {dataStorageConfig = a} :: UpdateInferenceExperiment)

-- | The description of the inference experiment.
updateInferenceExperiment_description :: Lens.Lens' UpdateInferenceExperiment (Prelude.Maybe Prelude.Text)
updateInferenceExperiment_description = Lens.lens (\UpdateInferenceExperiment' {description} -> description) (\s@UpdateInferenceExperiment' {} a -> s {description = a} :: UpdateInferenceExperiment)

-- | An array of @ModelVariantConfig@ objects. There is one for each variant,
-- whose infrastructure configuration you want to update.
updateInferenceExperiment_modelVariants :: Lens.Lens' UpdateInferenceExperiment (Prelude.Maybe (Prelude.NonEmpty ModelVariantConfig))
updateInferenceExperiment_modelVariants = Lens.lens (\UpdateInferenceExperiment' {modelVariants} -> modelVariants) (\s@UpdateInferenceExperiment' {} a -> s {modelVariants = a} :: UpdateInferenceExperiment) Prelude.. Lens.mapping Lens.coerced

-- | The duration for which the inference experiment will run. If the status
-- of the inference experiment is @Created@, then you can update both the
-- start and end dates. If the status of the inference experiment is
-- @Running@, then you can update only the end date.
updateInferenceExperiment_schedule :: Lens.Lens' UpdateInferenceExperiment (Prelude.Maybe InferenceExperimentSchedule)
updateInferenceExperiment_schedule = Lens.lens (\UpdateInferenceExperiment' {schedule} -> schedule) (\s@UpdateInferenceExperiment' {} a -> s {schedule = a} :: UpdateInferenceExperiment)

-- | The configuration of @ShadowMode@ inference experiment type. Use this
-- field to specify a production variant which takes all the inference
-- requests, and a shadow variant to which Amazon SageMaker replicates a
-- percentage of the inference requests. For the shadow variant also
-- specify the percentage of requests that Amazon SageMaker replicates.
updateInferenceExperiment_shadowModeConfig :: Lens.Lens' UpdateInferenceExperiment (Prelude.Maybe ShadowModeConfig)
updateInferenceExperiment_shadowModeConfig = Lens.lens (\UpdateInferenceExperiment' {shadowModeConfig} -> shadowModeConfig) (\s@UpdateInferenceExperiment' {} a -> s {shadowModeConfig = a} :: UpdateInferenceExperiment)

-- | The name of the inference experiment to be updated.
updateInferenceExperiment_name :: Lens.Lens' UpdateInferenceExperiment Prelude.Text
updateInferenceExperiment_name = Lens.lens (\UpdateInferenceExperiment' {name} -> name) (\s@UpdateInferenceExperiment' {} a -> s {name = a} :: UpdateInferenceExperiment)

instance Core.AWSRequest UpdateInferenceExperiment where
  type
    AWSResponse UpdateInferenceExperiment =
      UpdateInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInferenceExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InferenceExperimentArn")
      )

instance Prelude.Hashable UpdateInferenceExperiment where
  hashWithSalt _salt UpdateInferenceExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` dataStorageConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modelVariants
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` shadowModeConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateInferenceExperiment where
  rnf UpdateInferenceExperiment' {..} =
    Prelude.rnf dataStorageConfig `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf modelVariants `Prelude.seq`
          Prelude.rnf schedule `Prelude.seq`
            Prelude.rnf shadowModeConfig `Prelude.seq`
              Prelude.rnf name

instance Data.ToHeaders UpdateInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInferenceExperiment where
  toJSON UpdateInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataStorageConfig" Data..=)
              Prelude.<$> dataStorageConfig,
            ("Description" Data..=) Prelude.<$> description,
            ("ModelVariants" Data..=) Prelude.<$> modelVariants,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("ShadowModeConfig" Data..=)
              Prelude.<$> shadowModeConfig,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInferenceExperimentResponse' smart constructor.
data UpdateInferenceExperimentResponse = UpdateInferenceExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the updated inference experiment.
    inferenceExperimentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'inferenceExperimentArn', 'updateInferenceExperimentResponse_inferenceExperimentArn' - The ARN of the updated inference experiment.
newUpdateInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inferenceExperimentArn'
  Prelude.Text ->
  UpdateInferenceExperimentResponse
newUpdateInferenceExperimentResponse
  pHttpStatus_
  pInferenceExperimentArn_ =
    UpdateInferenceExperimentResponse'
      { httpStatus =
          pHttpStatus_,
        inferenceExperimentArn =
          pInferenceExperimentArn_
      }

-- | The response's http status code.
updateInferenceExperimentResponse_httpStatus :: Lens.Lens' UpdateInferenceExperimentResponse Prelude.Int
updateInferenceExperimentResponse_httpStatus = Lens.lens (\UpdateInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@UpdateInferenceExperimentResponse' {} a -> s {httpStatus = a} :: UpdateInferenceExperimentResponse)

-- | The ARN of the updated inference experiment.
updateInferenceExperimentResponse_inferenceExperimentArn :: Lens.Lens' UpdateInferenceExperimentResponse Prelude.Text
updateInferenceExperimentResponse_inferenceExperimentArn = Lens.lens (\UpdateInferenceExperimentResponse' {inferenceExperimentArn} -> inferenceExperimentArn) (\s@UpdateInferenceExperimentResponse' {} a -> s {inferenceExperimentArn = a} :: UpdateInferenceExperimentResponse)

instance
  Prelude.NFData
    UpdateInferenceExperimentResponse
  where
  rnf UpdateInferenceExperimentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf inferenceExperimentArn

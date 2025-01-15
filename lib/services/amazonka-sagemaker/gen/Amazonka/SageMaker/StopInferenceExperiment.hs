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
-- Module      : Amazonka.SageMaker.StopInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an inference experiment.
module Amazonka.SageMaker.StopInferenceExperiment
  ( -- * Creating a Request
    StopInferenceExperiment (..),
    newStopInferenceExperiment,

    -- * Request Lenses
    stopInferenceExperiment_desiredModelVariants,
    stopInferenceExperiment_desiredState,
    stopInferenceExperiment_reason,
    stopInferenceExperiment_name,
    stopInferenceExperiment_modelVariantActions,

    -- * Destructuring the Response
    StopInferenceExperimentResponse (..),
    newStopInferenceExperimentResponse,

    -- * Response Lenses
    stopInferenceExperimentResponse_httpStatus,
    stopInferenceExperimentResponse_inferenceExperimentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopInferenceExperiment' smart constructor.
data StopInferenceExperiment = StopInferenceExperiment'
  { -- | An array of @ModelVariantConfig@ objects. There is one for each variant
    -- that you want to deploy after the inference experiment stops. Each
    -- @ModelVariantConfig@ describes the infrastructure configuration for
    -- deploying the corresponding variant.
    desiredModelVariants :: Prelude.Maybe (Prelude.NonEmpty ModelVariantConfig),
    -- | The desired state of the experiment after stopping. The possible states
    -- are the following:
    --
    -- -   @Completed@: The experiment completed successfully
    --
    -- -   @Cancelled@: The experiment was canceled
    desiredState :: Prelude.Maybe InferenceExperimentStopDesiredState,
    -- | The reason for stopping the experiment.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference experiment to stop.
    name :: Prelude.Text,
    -- | Array of key-value pairs, with names of variants mapped to actions. The
    -- possible actions are the following:
    --
    -- -   @Promote@ - Promote the shadow variant to a production variant
    --
    -- -   @Remove@ - Delete the variant
    --
    -- -   @Retain@ - Keep the variant as it is
    modelVariantActions :: Prelude.HashMap Prelude.Text ModelVariantAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredModelVariants', 'stopInferenceExperiment_desiredModelVariants' - An array of @ModelVariantConfig@ objects. There is one for each variant
-- that you want to deploy after the inference experiment stops. Each
-- @ModelVariantConfig@ describes the infrastructure configuration for
-- deploying the corresponding variant.
--
-- 'desiredState', 'stopInferenceExperiment_desiredState' - The desired state of the experiment after stopping. The possible states
-- are the following:
--
-- -   @Completed@: The experiment completed successfully
--
-- -   @Cancelled@: The experiment was canceled
--
-- 'reason', 'stopInferenceExperiment_reason' - The reason for stopping the experiment.
--
-- 'name', 'stopInferenceExperiment_name' - The name of the inference experiment to stop.
--
-- 'modelVariantActions', 'stopInferenceExperiment_modelVariantActions' - Array of key-value pairs, with names of variants mapped to actions. The
-- possible actions are the following:
--
-- -   @Promote@ - Promote the shadow variant to a production variant
--
-- -   @Remove@ - Delete the variant
--
-- -   @Retain@ - Keep the variant as it is
newStopInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  StopInferenceExperiment
newStopInferenceExperiment pName_ =
  StopInferenceExperiment'
    { desiredModelVariants =
        Prelude.Nothing,
      desiredState = Prelude.Nothing,
      reason = Prelude.Nothing,
      name = pName_,
      modelVariantActions = Prelude.mempty
    }

-- | An array of @ModelVariantConfig@ objects. There is one for each variant
-- that you want to deploy after the inference experiment stops. Each
-- @ModelVariantConfig@ describes the infrastructure configuration for
-- deploying the corresponding variant.
stopInferenceExperiment_desiredModelVariants :: Lens.Lens' StopInferenceExperiment (Prelude.Maybe (Prelude.NonEmpty ModelVariantConfig))
stopInferenceExperiment_desiredModelVariants = Lens.lens (\StopInferenceExperiment' {desiredModelVariants} -> desiredModelVariants) (\s@StopInferenceExperiment' {} a -> s {desiredModelVariants = a} :: StopInferenceExperiment) Prelude.. Lens.mapping Lens.coerced

-- | The desired state of the experiment after stopping. The possible states
-- are the following:
--
-- -   @Completed@: The experiment completed successfully
--
-- -   @Cancelled@: The experiment was canceled
stopInferenceExperiment_desiredState :: Lens.Lens' StopInferenceExperiment (Prelude.Maybe InferenceExperimentStopDesiredState)
stopInferenceExperiment_desiredState = Lens.lens (\StopInferenceExperiment' {desiredState} -> desiredState) (\s@StopInferenceExperiment' {} a -> s {desiredState = a} :: StopInferenceExperiment)

-- | The reason for stopping the experiment.
stopInferenceExperiment_reason :: Lens.Lens' StopInferenceExperiment (Prelude.Maybe Prelude.Text)
stopInferenceExperiment_reason = Lens.lens (\StopInferenceExperiment' {reason} -> reason) (\s@StopInferenceExperiment' {} a -> s {reason = a} :: StopInferenceExperiment)

-- | The name of the inference experiment to stop.
stopInferenceExperiment_name :: Lens.Lens' StopInferenceExperiment Prelude.Text
stopInferenceExperiment_name = Lens.lens (\StopInferenceExperiment' {name} -> name) (\s@StopInferenceExperiment' {} a -> s {name = a} :: StopInferenceExperiment)

-- | Array of key-value pairs, with names of variants mapped to actions. The
-- possible actions are the following:
--
-- -   @Promote@ - Promote the shadow variant to a production variant
--
-- -   @Remove@ - Delete the variant
--
-- -   @Retain@ - Keep the variant as it is
stopInferenceExperiment_modelVariantActions :: Lens.Lens' StopInferenceExperiment (Prelude.HashMap Prelude.Text ModelVariantAction)
stopInferenceExperiment_modelVariantActions = Lens.lens (\StopInferenceExperiment' {modelVariantActions} -> modelVariantActions) (\s@StopInferenceExperiment' {} a -> s {modelVariantActions = a} :: StopInferenceExperiment) Prelude.. Lens.coerced

instance Core.AWSRequest StopInferenceExperiment where
  type
    AWSResponse StopInferenceExperiment =
      StopInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopInferenceExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InferenceExperimentArn")
      )

instance Prelude.Hashable StopInferenceExperiment where
  hashWithSalt _salt StopInferenceExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` desiredModelVariants
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` modelVariantActions

instance Prelude.NFData StopInferenceExperiment where
  rnf StopInferenceExperiment' {..} =
    Prelude.rnf desiredModelVariants `Prelude.seq`
      Prelude.rnf desiredState `Prelude.seq`
        Prelude.rnf reason `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf modelVariantActions

instance Data.ToHeaders StopInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopInferenceExperiment where
  toJSON StopInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredModelVariants" Data..=)
              Prelude.<$> desiredModelVariants,
            ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("Reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ModelVariantActions" Data..= modelVariantActions)
          ]
      )

instance Data.ToPath StopInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery StopInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopInferenceExperimentResponse' smart constructor.
data StopInferenceExperimentResponse = StopInferenceExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the stopped inference experiment.
    inferenceExperimentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'inferenceExperimentArn', 'stopInferenceExperimentResponse_inferenceExperimentArn' - The ARN of the stopped inference experiment.
newStopInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inferenceExperimentArn'
  Prelude.Text ->
  StopInferenceExperimentResponse
newStopInferenceExperimentResponse
  pHttpStatus_
  pInferenceExperimentArn_ =
    StopInferenceExperimentResponse'
      { httpStatus =
          pHttpStatus_,
        inferenceExperimentArn =
          pInferenceExperimentArn_
      }

-- | The response's http status code.
stopInferenceExperimentResponse_httpStatus :: Lens.Lens' StopInferenceExperimentResponse Prelude.Int
stopInferenceExperimentResponse_httpStatus = Lens.lens (\StopInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@StopInferenceExperimentResponse' {} a -> s {httpStatus = a} :: StopInferenceExperimentResponse)

-- | The ARN of the stopped inference experiment.
stopInferenceExperimentResponse_inferenceExperimentArn :: Lens.Lens' StopInferenceExperimentResponse Prelude.Text
stopInferenceExperimentResponse_inferenceExperimentArn = Lens.lens (\StopInferenceExperimentResponse' {inferenceExperimentArn} -> inferenceExperimentArn) (\s@StopInferenceExperimentResponse' {} a -> s {inferenceExperimentArn = a} :: StopInferenceExperimentResponse)

instance
  Prelude.NFData
    StopInferenceExperimentResponse
  where
  rnf StopInferenceExperimentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf inferenceExperimentArn

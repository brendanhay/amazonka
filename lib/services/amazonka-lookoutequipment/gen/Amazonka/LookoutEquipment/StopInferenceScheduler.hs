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
-- Module      : Amazonka.LookoutEquipment.StopInferenceScheduler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an inference scheduler.
module Amazonka.LookoutEquipment.StopInferenceScheduler
  ( -- * Creating a Request
    StopInferenceScheduler (..),
    newStopInferenceScheduler,

    -- * Request Lenses
    stopInferenceScheduler_inferenceSchedulerName,

    -- * Destructuring the Response
    StopInferenceSchedulerResponse (..),
    newStopInferenceSchedulerResponse,

    -- * Response Lenses
    stopInferenceSchedulerResponse_inferenceSchedulerName,
    stopInferenceSchedulerResponse_status,
    stopInferenceSchedulerResponse_modelArn,
    stopInferenceSchedulerResponse_modelName,
    stopInferenceSchedulerResponse_inferenceSchedulerArn,
    stopInferenceSchedulerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopInferenceScheduler' smart constructor.
data StopInferenceScheduler = StopInferenceScheduler'
  { -- | The name of the inference scheduler to be stopped.
    inferenceSchedulerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceScheduler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerName', 'stopInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler to be stopped.
newStopInferenceScheduler ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  StopInferenceScheduler
newStopInferenceScheduler pInferenceSchedulerName_ =
  StopInferenceScheduler'
    { inferenceSchedulerName =
        pInferenceSchedulerName_
    }

-- | The name of the inference scheduler to be stopped.
stopInferenceScheduler_inferenceSchedulerName :: Lens.Lens' StopInferenceScheduler Prelude.Text
stopInferenceScheduler_inferenceSchedulerName = Lens.lens (\StopInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@StopInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: StopInferenceScheduler)

instance Core.AWSRequest StopInferenceScheduler where
  type
    AWSResponse StopInferenceScheduler =
      StopInferenceSchedulerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopInferenceSchedulerResponse'
            Prelude.<$> (x Core..?> "InferenceSchedulerName")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ModelArn")
            Prelude.<*> (x Core..?> "ModelName")
            Prelude.<*> (x Core..?> "InferenceSchedulerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopInferenceScheduler where
  hashWithSalt _salt StopInferenceScheduler' {..} =
    _salt `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData StopInferenceScheduler where
  rnf StopInferenceScheduler' {..} =
    Prelude.rnf inferenceSchedulerName

instance Core.ToHeaders StopInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.StopInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopInferenceScheduler where
  toJSON StopInferenceScheduler' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "InferenceSchedulerName"
                  Core..= inferenceSchedulerName
              )
          ]
      )

instance Core.ToPath StopInferenceScheduler where
  toPath = Prelude.const "/"

instance Core.ToQuery StopInferenceScheduler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopInferenceSchedulerResponse' smart constructor.
data StopInferenceSchedulerResponse = StopInferenceSchedulerResponse'
  { -- | The name of the inference scheduler being stopped.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the inference scheduler.
    status :: Prelude.Maybe InferenceSchedulerStatus,
    -- | The Amazon Resource Name (ARN) of the ML model used by the inference
    -- scheduler being stopped.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ML model used by the inference scheduler being stopped.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the inference schedule being stopped.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceSchedulerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerName', 'stopInferenceSchedulerResponse_inferenceSchedulerName' - The name of the inference scheduler being stopped.
--
-- 'status', 'stopInferenceSchedulerResponse_status' - Indicates the status of the inference scheduler.
--
-- 'modelArn', 'stopInferenceSchedulerResponse_modelArn' - The Amazon Resource Name (ARN) of the ML model used by the inference
-- scheduler being stopped.
--
-- 'modelName', 'stopInferenceSchedulerResponse_modelName' - The name of the ML model used by the inference scheduler being stopped.
--
-- 'inferenceSchedulerArn', 'stopInferenceSchedulerResponse_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference schedule being stopped.
--
-- 'httpStatus', 'stopInferenceSchedulerResponse_httpStatus' - The response's http status code.
newStopInferenceSchedulerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopInferenceSchedulerResponse
newStopInferenceSchedulerResponse pHttpStatus_ =
  StopInferenceSchedulerResponse'
    { inferenceSchedulerName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the inference scheduler being stopped.
stopInferenceSchedulerResponse_inferenceSchedulerName :: Lens.Lens' StopInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
stopInferenceSchedulerResponse_inferenceSchedulerName = Lens.lens (\StopInferenceSchedulerResponse' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@StopInferenceSchedulerResponse' {} a -> s {inferenceSchedulerName = a} :: StopInferenceSchedulerResponse)

-- | Indicates the status of the inference scheduler.
stopInferenceSchedulerResponse_status :: Lens.Lens' StopInferenceSchedulerResponse (Prelude.Maybe InferenceSchedulerStatus)
stopInferenceSchedulerResponse_status = Lens.lens (\StopInferenceSchedulerResponse' {status} -> status) (\s@StopInferenceSchedulerResponse' {} a -> s {status = a} :: StopInferenceSchedulerResponse)

-- | The Amazon Resource Name (ARN) of the ML model used by the inference
-- scheduler being stopped.
stopInferenceSchedulerResponse_modelArn :: Lens.Lens' StopInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
stopInferenceSchedulerResponse_modelArn = Lens.lens (\StopInferenceSchedulerResponse' {modelArn} -> modelArn) (\s@StopInferenceSchedulerResponse' {} a -> s {modelArn = a} :: StopInferenceSchedulerResponse)

-- | The name of the ML model used by the inference scheduler being stopped.
stopInferenceSchedulerResponse_modelName :: Lens.Lens' StopInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
stopInferenceSchedulerResponse_modelName = Lens.lens (\StopInferenceSchedulerResponse' {modelName} -> modelName) (\s@StopInferenceSchedulerResponse' {} a -> s {modelName = a} :: StopInferenceSchedulerResponse)

-- | The Amazon Resource Name (ARN) of the inference schedule being stopped.
stopInferenceSchedulerResponse_inferenceSchedulerArn :: Lens.Lens' StopInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
stopInferenceSchedulerResponse_inferenceSchedulerArn = Lens.lens (\StopInferenceSchedulerResponse' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@StopInferenceSchedulerResponse' {} a -> s {inferenceSchedulerArn = a} :: StopInferenceSchedulerResponse)

-- | The response's http status code.
stopInferenceSchedulerResponse_httpStatus :: Lens.Lens' StopInferenceSchedulerResponse Prelude.Int
stopInferenceSchedulerResponse_httpStatus = Lens.lens (\StopInferenceSchedulerResponse' {httpStatus} -> httpStatus) (\s@StopInferenceSchedulerResponse' {} a -> s {httpStatus = a} :: StopInferenceSchedulerResponse)

instance
  Prelude.NFData
    StopInferenceSchedulerResponse
  where
  rnf StopInferenceSchedulerResponse' {..} =
    Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.LookoutEquipment.StartInferenceScheduler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an inference scheduler.
module Amazonka.LookoutEquipment.StartInferenceScheduler
  ( -- * Creating a Request
    StartInferenceScheduler (..),
    newStartInferenceScheduler,

    -- * Request Lenses
    startInferenceScheduler_inferenceSchedulerName,

    -- * Destructuring the Response
    StartInferenceSchedulerResponse (..),
    newStartInferenceSchedulerResponse,

    -- * Response Lenses
    startInferenceSchedulerResponse_inferenceSchedulerName,
    startInferenceSchedulerResponse_status,
    startInferenceSchedulerResponse_modelArn,
    startInferenceSchedulerResponse_modelName,
    startInferenceSchedulerResponse_inferenceSchedulerArn,
    startInferenceSchedulerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartInferenceScheduler' smart constructor.
data StartInferenceScheduler = StartInferenceScheduler'
  { -- | The name of the inference scheduler to be started.
    inferenceSchedulerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInferenceScheduler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerName', 'startInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler to be started.
newStartInferenceScheduler ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  StartInferenceScheduler
newStartInferenceScheduler pInferenceSchedulerName_ =
  StartInferenceScheduler'
    { inferenceSchedulerName =
        pInferenceSchedulerName_
    }

-- | The name of the inference scheduler to be started.
startInferenceScheduler_inferenceSchedulerName :: Lens.Lens' StartInferenceScheduler Prelude.Text
startInferenceScheduler_inferenceSchedulerName = Lens.lens (\StartInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@StartInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: StartInferenceScheduler)

instance Core.AWSRequest StartInferenceScheduler where
  type
    AWSResponse StartInferenceScheduler =
      StartInferenceSchedulerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartInferenceSchedulerResponse'
            Prelude.<$> (x Core..?> "InferenceSchedulerName")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ModelArn")
            Prelude.<*> (x Core..?> "ModelName")
            Prelude.<*> (x Core..?> "InferenceSchedulerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInferenceScheduler where
  hashWithSalt _salt StartInferenceScheduler' {..} =
    _salt `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData StartInferenceScheduler where
  rnf StartInferenceScheduler' {..} =
    Prelude.rnf inferenceSchedulerName

instance Core.ToHeaders StartInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.StartInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartInferenceScheduler where
  toJSON StartInferenceScheduler' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "InferenceSchedulerName"
                  Core..= inferenceSchedulerName
              )
          ]
      )

instance Core.ToPath StartInferenceScheduler where
  toPath = Prelude.const "/"

instance Core.ToQuery StartInferenceScheduler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartInferenceSchedulerResponse' smart constructor.
data StartInferenceSchedulerResponse = StartInferenceSchedulerResponse'
  { -- | The name of the inference scheduler being started.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the inference scheduler.
    status :: Prelude.Maybe InferenceSchedulerStatus,
    -- | The Amazon Resource Name (ARN) of the ML model being used by the
    -- inference scheduler.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ML model being used by the inference scheduler.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the inference scheduler being started.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInferenceSchedulerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerName', 'startInferenceSchedulerResponse_inferenceSchedulerName' - The name of the inference scheduler being started.
--
-- 'status', 'startInferenceSchedulerResponse_status' - Indicates the status of the inference scheduler.
--
-- 'modelArn', 'startInferenceSchedulerResponse_modelArn' - The Amazon Resource Name (ARN) of the ML model being used by the
-- inference scheduler.
--
-- 'modelName', 'startInferenceSchedulerResponse_modelName' - The name of the ML model being used by the inference scheduler.
--
-- 'inferenceSchedulerArn', 'startInferenceSchedulerResponse_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler being started.
--
-- 'httpStatus', 'startInferenceSchedulerResponse_httpStatus' - The response's http status code.
newStartInferenceSchedulerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInferenceSchedulerResponse
newStartInferenceSchedulerResponse pHttpStatus_ =
  StartInferenceSchedulerResponse'
    { inferenceSchedulerName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the inference scheduler being started.
startInferenceSchedulerResponse_inferenceSchedulerName :: Lens.Lens' StartInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
startInferenceSchedulerResponse_inferenceSchedulerName = Lens.lens (\StartInferenceSchedulerResponse' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@StartInferenceSchedulerResponse' {} a -> s {inferenceSchedulerName = a} :: StartInferenceSchedulerResponse)

-- | Indicates the status of the inference scheduler.
startInferenceSchedulerResponse_status :: Lens.Lens' StartInferenceSchedulerResponse (Prelude.Maybe InferenceSchedulerStatus)
startInferenceSchedulerResponse_status = Lens.lens (\StartInferenceSchedulerResponse' {status} -> status) (\s@StartInferenceSchedulerResponse' {} a -> s {status = a} :: StartInferenceSchedulerResponse)

-- | The Amazon Resource Name (ARN) of the ML model being used by the
-- inference scheduler.
startInferenceSchedulerResponse_modelArn :: Lens.Lens' StartInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
startInferenceSchedulerResponse_modelArn = Lens.lens (\StartInferenceSchedulerResponse' {modelArn} -> modelArn) (\s@StartInferenceSchedulerResponse' {} a -> s {modelArn = a} :: StartInferenceSchedulerResponse)

-- | The name of the ML model being used by the inference scheduler.
startInferenceSchedulerResponse_modelName :: Lens.Lens' StartInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
startInferenceSchedulerResponse_modelName = Lens.lens (\StartInferenceSchedulerResponse' {modelName} -> modelName) (\s@StartInferenceSchedulerResponse' {} a -> s {modelName = a} :: StartInferenceSchedulerResponse)

-- | The Amazon Resource Name (ARN) of the inference scheduler being started.
startInferenceSchedulerResponse_inferenceSchedulerArn :: Lens.Lens' StartInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
startInferenceSchedulerResponse_inferenceSchedulerArn = Lens.lens (\StartInferenceSchedulerResponse' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@StartInferenceSchedulerResponse' {} a -> s {inferenceSchedulerArn = a} :: StartInferenceSchedulerResponse)

-- | The response's http status code.
startInferenceSchedulerResponse_httpStatus :: Lens.Lens' StartInferenceSchedulerResponse Prelude.Int
startInferenceSchedulerResponse_httpStatus = Lens.lens (\StartInferenceSchedulerResponse' {httpStatus} -> httpStatus) (\s@StartInferenceSchedulerResponse' {} a -> s {httpStatus = a} :: StartInferenceSchedulerResponse)

instance
  Prelude.NFData
    StartInferenceSchedulerResponse
  where
  rnf StartInferenceSchedulerResponse' {..} =
    Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf httpStatus

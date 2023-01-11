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
-- Module      : Amazonka.SageMaker.StartInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an inference experiment.
module Amazonka.SageMaker.StartInferenceExperiment
  ( -- * Creating a Request
    StartInferenceExperiment (..),
    newStartInferenceExperiment,

    -- * Request Lenses
    startInferenceExperiment_name,

    -- * Destructuring the Response
    StartInferenceExperimentResponse (..),
    newStartInferenceExperimentResponse,

    -- * Response Lenses
    startInferenceExperimentResponse_httpStatus,
    startInferenceExperimentResponse_inferenceExperimentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStartInferenceExperiment' smart constructor.
data StartInferenceExperiment = StartInferenceExperiment'
  { -- | The name of the inference experiment to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startInferenceExperiment_name' - The name of the inference experiment to start.
newStartInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  StartInferenceExperiment
newStartInferenceExperiment pName_ =
  StartInferenceExperiment' {name = pName_}

-- | The name of the inference experiment to start.
startInferenceExperiment_name :: Lens.Lens' StartInferenceExperiment Prelude.Text
startInferenceExperiment_name = Lens.lens (\StartInferenceExperiment' {name} -> name) (\s@StartInferenceExperiment' {} a -> s {name = a} :: StartInferenceExperiment)

instance Core.AWSRequest StartInferenceExperiment where
  type
    AWSResponse StartInferenceExperiment =
      StartInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartInferenceExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InferenceExperimentArn")
      )

instance Prelude.Hashable StartInferenceExperiment where
  hashWithSalt _salt StartInferenceExperiment' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StartInferenceExperiment where
  rnf StartInferenceExperiment' {..} = Prelude.rnf name

instance Data.ToHeaders StartInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StartInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartInferenceExperiment where
  toJSON StartInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StartInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery StartInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartInferenceExperimentResponse' smart constructor.
data StartInferenceExperimentResponse = StartInferenceExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the started inference experiment to start.
    inferenceExperimentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'inferenceExperimentArn', 'startInferenceExperimentResponse_inferenceExperimentArn' - The ARN of the started inference experiment to start.
newStartInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inferenceExperimentArn'
  Prelude.Text ->
  StartInferenceExperimentResponse
newStartInferenceExperimentResponse
  pHttpStatus_
  pInferenceExperimentArn_ =
    StartInferenceExperimentResponse'
      { httpStatus =
          pHttpStatus_,
        inferenceExperimentArn =
          pInferenceExperimentArn_
      }

-- | The response's http status code.
startInferenceExperimentResponse_httpStatus :: Lens.Lens' StartInferenceExperimentResponse Prelude.Int
startInferenceExperimentResponse_httpStatus = Lens.lens (\StartInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@StartInferenceExperimentResponse' {} a -> s {httpStatus = a} :: StartInferenceExperimentResponse)

-- | The ARN of the started inference experiment to start.
startInferenceExperimentResponse_inferenceExperimentArn :: Lens.Lens' StartInferenceExperimentResponse Prelude.Text
startInferenceExperimentResponse_inferenceExperimentArn = Lens.lens (\StartInferenceExperimentResponse' {inferenceExperimentArn} -> inferenceExperimentArn) (\s@StartInferenceExperimentResponse' {} a -> s {inferenceExperimentArn = a} :: StartInferenceExperimentResponse)

instance
  Prelude.NFData
    StartInferenceExperimentResponse
  where
  rnf StartInferenceExperimentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf inferenceExperimentArn

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
-- Module      : Amazonka.SageMaker.DeleteInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inference experiment.
--
-- This operation does not delete your endpoint, variants, or any
-- underlying resources. This operation only deletes the metadata of your
-- experiment.
module Amazonka.SageMaker.DeleteInferenceExperiment
  ( -- * Creating a Request
    DeleteInferenceExperiment (..),
    newDeleteInferenceExperiment,

    -- * Request Lenses
    deleteInferenceExperiment_name,

    -- * Destructuring the Response
    DeleteInferenceExperimentResponse (..),
    newDeleteInferenceExperimentResponse,

    -- * Response Lenses
    deleteInferenceExperimentResponse_httpStatus,
    deleteInferenceExperimentResponse_inferenceExperimentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteInferenceExperiment' smart constructor.
data DeleteInferenceExperiment = DeleteInferenceExperiment'
  { -- | The name of the inference experiment you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteInferenceExperiment_name' - The name of the inference experiment you want to delete.
newDeleteInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  DeleteInferenceExperiment
newDeleteInferenceExperiment pName_ =
  DeleteInferenceExperiment' {name = pName_}

-- | The name of the inference experiment you want to delete.
deleteInferenceExperiment_name :: Lens.Lens' DeleteInferenceExperiment Prelude.Text
deleteInferenceExperiment_name = Lens.lens (\DeleteInferenceExperiment' {name} -> name) (\s@DeleteInferenceExperiment' {} a -> s {name = a} :: DeleteInferenceExperiment)

instance Core.AWSRequest DeleteInferenceExperiment where
  type
    AWSResponse DeleteInferenceExperiment =
      DeleteInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInferenceExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InferenceExperimentArn")
      )

instance Prelude.Hashable DeleteInferenceExperiment where
  hashWithSalt _salt DeleteInferenceExperiment' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteInferenceExperiment where
  rnf DeleteInferenceExperiment' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInferenceExperiment where
  toJSON DeleteInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInferenceExperimentResponse' smart constructor.
data DeleteInferenceExperimentResponse = DeleteInferenceExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the deleted inference experiment.
    inferenceExperimentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'inferenceExperimentArn', 'deleteInferenceExperimentResponse_inferenceExperimentArn' - The ARN of the deleted inference experiment.
newDeleteInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inferenceExperimentArn'
  Prelude.Text ->
  DeleteInferenceExperimentResponse
newDeleteInferenceExperimentResponse
  pHttpStatus_
  pInferenceExperimentArn_ =
    DeleteInferenceExperimentResponse'
      { httpStatus =
          pHttpStatus_,
        inferenceExperimentArn =
          pInferenceExperimentArn_
      }

-- | The response's http status code.
deleteInferenceExperimentResponse_httpStatus :: Lens.Lens' DeleteInferenceExperimentResponse Prelude.Int
deleteInferenceExperimentResponse_httpStatus = Lens.lens (\DeleteInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@DeleteInferenceExperimentResponse' {} a -> s {httpStatus = a} :: DeleteInferenceExperimentResponse)

-- | The ARN of the deleted inference experiment.
deleteInferenceExperimentResponse_inferenceExperimentArn :: Lens.Lens' DeleteInferenceExperimentResponse Prelude.Text
deleteInferenceExperimentResponse_inferenceExperimentArn = Lens.lens (\DeleteInferenceExperimentResponse' {inferenceExperimentArn} -> inferenceExperimentArn) (\s@DeleteInferenceExperimentResponse' {} a -> s {inferenceExperimentArn = a} :: DeleteInferenceExperimentResponse)

instance
  Prelude.NFData
    DeleteInferenceExperimentResponse
  where
  rnf DeleteInferenceExperimentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf inferenceExperimentArn

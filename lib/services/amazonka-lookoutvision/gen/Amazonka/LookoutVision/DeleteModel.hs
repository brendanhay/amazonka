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
-- Module      : Amazonka.LookoutVision.DeleteModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lookout for Vision model. You can\'t delete a running
-- model. To stop a running model, use the StopModel operation.
--
-- It might take a few seconds to delete a model. To determine if a model
-- has been deleted, call ListModels and check if the version of the model
-- (@ModelVersion@) is in the @Models@ array.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DeleteModel@ operation.
module Amazonka.LookoutVision.DeleteModel
  ( -- * Creating a Request
    DeleteModel (..),
    newDeleteModel,

    -- * Request Lenses
    deleteModel_clientToken,
    deleteModel_projectName,
    deleteModel_modelVersion,

    -- * Destructuring the Response
    DeleteModelResponse (..),
    newDeleteModelResponse,

    -- * Response Lenses
    deleteModelResponse_modelArn,
    deleteModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | ClientToken is an idempotency token that ensures a call to @DeleteModel@
    -- completes only once. You choose the value to pass. For example, an issue
    -- might prevent you from getting a response from @DeleteModel@. In this
    -- case, safely retry your call to @DeleteModel@ by using the same
    -- @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for ClientToken, the AWS SDK you are using
    -- inserts a value for you. This prevents retries after a network error
    -- from making multiple model deletion requests. You\'ll need to provide
    -- your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @DeleteModel@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that contains the model that you want to delete.
    projectName :: Prelude.Text,
    -- | The version of the model that you want to delete.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteModel_clientToken' - ClientToken is an idempotency token that ensures a call to @DeleteModel@
-- completes only once. You choose the value to pass. For example, an issue
-- might prevent you from getting a response from @DeleteModel@. In this
-- case, safely retry your call to @DeleteModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for ClientToken, the AWS SDK you are using
-- inserts a value for you. This prevents retries after a network error
-- from making multiple model deletion requests. You\'ll need to provide
-- your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteModel@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'deleteModel_projectName' - The name of the project that contains the model that you want to delete.
--
-- 'modelVersion', 'deleteModel_modelVersion' - The version of the model that you want to delete.
newDeleteModel ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  DeleteModel
newDeleteModel pProjectName_ pModelVersion_ =
  DeleteModel'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_,
      modelVersion = pModelVersion_
    }

-- | ClientToken is an idempotency token that ensures a call to @DeleteModel@
-- completes only once. You choose the value to pass. For example, an issue
-- might prevent you from getting a response from @DeleteModel@. In this
-- case, safely retry your call to @DeleteModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for ClientToken, the AWS SDK you are using
-- inserts a value for you. This prevents retries after a network error
-- from making multiple model deletion requests. You\'ll need to provide
-- your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteModel@. An idempotency token is active for 8 hours.
deleteModel_clientToken :: Lens.Lens' DeleteModel (Prelude.Maybe Prelude.Text)
deleteModel_clientToken = Lens.lens (\DeleteModel' {clientToken} -> clientToken) (\s@DeleteModel' {} a -> s {clientToken = a} :: DeleteModel)

-- | The name of the project that contains the model that you want to delete.
deleteModel_projectName :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_projectName = Lens.lens (\DeleteModel' {projectName} -> projectName) (\s@DeleteModel' {} a -> s {projectName = a} :: DeleteModel)

-- | The version of the model that you want to delete.
deleteModel_modelVersion :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_modelVersion = Lens.lens (\DeleteModel' {modelVersion} -> modelVersion) (\s@DeleteModel' {} a -> s {modelVersion = a} :: DeleteModel)

instance Core.AWSRequest DeleteModel where
  type AWSResponse DeleteModel = DeleteModelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteModelResponse'
            Prelude.<$> (x Data..?> "ModelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteModel where
  hashWithSalt _salt DeleteModel' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData DeleteModel where
  rnf DeleteModel' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf modelVersion

instance Data.ToHeaders DeleteModel where
  toHeaders DeleteModel' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteModel where
  toPath DeleteModel' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/models/",
        Data.toBS modelVersion
      ]

instance Data.ToQuery DeleteModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  { -- | The Amazon Resource Name (ARN) of the model that was deleted.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelArn', 'deleteModelResponse_modelArn' - The Amazon Resource Name (ARN) of the model that was deleted.
--
-- 'httpStatus', 'deleteModelResponse_httpStatus' - The response's http status code.
newDeleteModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteModelResponse
newDeleteModelResponse pHttpStatus_ =
  DeleteModelResponse'
    { modelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the model that was deleted.
deleteModelResponse_modelArn :: Lens.Lens' DeleteModelResponse (Prelude.Maybe Prelude.Text)
deleteModelResponse_modelArn = Lens.lens (\DeleteModelResponse' {modelArn} -> modelArn) (\s@DeleteModelResponse' {} a -> s {modelArn = a} :: DeleteModelResponse)

-- | The response's http status code.
deleteModelResponse_httpStatus :: Lens.Lens' DeleteModelResponse Prelude.Int
deleteModelResponse_httpStatus = Lens.lens (\DeleteModelResponse' {httpStatus} -> httpStatus) (\s@DeleteModelResponse' {} a -> s {httpStatus = a} :: DeleteModelResponse)

instance Prelude.NFData DeleteModelResponse where
  rnf DeleteModelResponse' {..} =
    Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf httpStatus

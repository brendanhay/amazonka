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
-- Module      : Network.AWS.ApiGatewayV2.UpdateModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Model.
module Network.AWS.ApiGatewayV2.UpdateModel
  ( -- * Creating a Request
    UpdateModel (..),
    newUpdateModel,

    -- * Request Lenses
    updateModel_schema,
    updateModel_name,
    updateModel_description,
    updateModel_contentType,
    updateModel_modelId,
    updateModel_apiId,

    -- * Destructuring the Response
    UpdateModelResponse (..),
    newUpdateModelResponse,

    -- * Response Lenses
    updateModelResponse_modelId,
    updateModelResponse_schema,
    updateModelResponse_name,
    updateModelResponse_description,
    updateModelResponse_contentType,
    updateModelResponse_httpStatus,
  )
where

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a Model.
--
-- /See:/ 'newUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The name of the model.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'updateModel_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'name', 'updateModel_name' - The name of the model.
--
-- 'description', 'updateModel_description' - The description of the model.
--
-- 'contentType', 'updateModel_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'modelId', 'updateModel_modelId' - The model ID.
--
-- 'apiId', 'updateModel_apiId' - The API identifier.
newUpdateModel ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  UpdateModel
newUpdateModel pModelId_ pApiId_ =
  UpdateModel'
    { schema = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      contentType = Prelude.Nothing,
      modelId = pModelId_,
      apiId = pApiId_
    }

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
updateModel_schema :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_schema = Lens.lens (\UpdateModel' {schema} -> schema) (\s@UpdateModel' {} a -> s {schema = a} :: UpdateModel)

-- | The name of the model.
updateModel_name :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_name = Lens.lens (\UpdateModel' {name} -> name) (\s@UpdateModel' {} a -> s {name = a} :: UpdateModel)

-- | The description of the model.
updateModel_description :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_description = Lens.lens (\UpdateModel' {description} -> description) (\s@UpdateModel' {} a -> s {description = a} :: UpdateModel)

-- | The content-type for the model, for example, \"application\/json\".
updateModel_contentType :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_contentType = Lens.lens (\UpdateModel' {contentType} -> contentType) (\s@UpdateModel' {} a -> s {contentType = a} :: UpdateModel)

-- | The model ID.
updateModel_modelId :: Lens.Lens' UpdateModel Prelude.Text
updateModel_modelId = Lens.lens (\UpdateModel' {modelId} -> modelId) (\s@UpdateModel' {} a -> s {modelId = a} :: UpdateModel)

-- | The API identifier.
updateModel_apiId :: Lens.Lens' UpdateModel Prelude.Text
updateModel_apiId = Lens.lens (\UpdateModel' {apiId} -> apiId) (\s@UpdateModel' {} a -> s {apiId = a} :: UpdateModel)

instance Core.AWSRequest UpdateModel where
  type AWSResponse UpdateModel = UpdateModelResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelResponse'
            Prelude.<$> (x Core..?> "modelId")
            Prelude.<*> (x Core..?> "schema")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "contentType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateModel

instance Prelude.NFData UpdateModel

instance Core.ToHeaders UpdateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateModel where
  toJSON UpdateModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("schema" Core..=) Prelude.<$> schema,
            ("name" Core..=) Prelude.<$> name,
            ("description" Core..=) Prelude.<$> description,
            ("contentType" Core..=) Prelude.<$> contentType
          ]
      )

instance Core.ToPath UpdateModel where
  toPath UpdateModel' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/models/",
        Core.toBS modelId
      ]

instance Core.ToQuery UpdateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelResponse' smart constructor.
data UpdateModelResponse = UpdateModelResponse'
  { -- | The model identifier.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelId', 'updateModelResponse_modelId' - The model identifier.
--
-- 'schema', 'updateModelResponse_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'name', 'updateModelResponse_name' - The name of the model. Must be alphanumeric.
--
-- 'description', 'updateModelResponse_description' - The description of the model.
--
-- 'contentType', 'updateModelResponse_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'httpStatus', 'updateModelResponse_httpStatus' - The response's http status code.
newUpdateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateModelResponse
newUpdateModelResponse pHttpStatus_ =
  UpdateModelResponse'
    { modelId = Prelude.Nothing,
      schema = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The model identifier.
updateModelResponse_modelId :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_modelId = Lens.lens (\UpdateModelResponse' {modelId} -> modelId) (\s@UpdateModelResponse' {} a -> s {modelId = a} :: UpdateModelResponse)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
updateModelResponse_schema :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_schema = Lens.lens (\UpdateModelResponse' {schema} -> schema) (\s@UpdateModelResponse' {} a -> s {schema = a} :: UpdateModelResponse)

-- | The name of the model. Must be alphanumeric.
updateModelResponse_name :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_name = Lens.lens (\UpdateModelResponse' {name} -> name) (\s@UpdateModelResponse' {} a -> s {name = a} :: UpdateModelResponse)

-- | The description of the model.
updateModelResponse_description :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_description = Lens.lens (\UpdateModelResponse' {description} -> description) (\s@UpdateModelResponse' {} a -> s {description = a} :: UpdateModelResponse)

-- | The content-type for the model, for example, \"application\/json\".
updateModelResponse_contentType :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_contentType = Lens.lens (\UpdateModelResponse' {contentType} -> contentType) (\s@UpdateModelResponse' {} a -> s {contentType = a} :: UpdateModelResponse)

-- | The response's http status code.
updateModelResponse_httpStatus :: Lens.Lens' UpdateModelResponse Prelude.Int
updateModelResponse_httpStatus = Lens.lens (\UpdateModelResponse' {httpStatus} -> httpStatus) (\s@UpdateModelResponse' {} a -> s {httpStatus = a} :: UpdateModelResponse)

instance Prelude.NFData UpdateModelResponse

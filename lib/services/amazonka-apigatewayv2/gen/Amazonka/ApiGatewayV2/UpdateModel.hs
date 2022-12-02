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
-- Module      : Amazonka.ApiGatewayV2.UpdateModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Model.
module Amazonka.ApiGatewayV2.UpdateModel
  ( -- * Creating a Request
    UpdateModel (..),
    newUpdateModel,

    -- * Request Lenses
    updateModel_name,
    updateModel_description,
    updateModel_schema,
    updateModel_contentType,
    updateModel_modelId,
    updateModel_apiId,

    -- * Destructuring the Response
    UpdateModelResponse (..),
    newUpdateModelResponse,

    -- * Response Lenses
    updateModelResponse_name,
    updateModelResponse_description,
    updateModelResponse_schema,
    updateModelResponse_modelId,
    updateModelResponse_contentType,
    updateModelResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a Model.
--
-- /See:/ 'newUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { -- | The name of the model.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
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
-- 'name', 'updateModel_name' - The name of the model.
--
-- 'description', 'updateModel_description' - The description of the model.
--
-- 'schema', 'updateModel_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
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
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      schema = Prelude.Nothing,
      contentType = Prelude.Nothing,
      modelId = pModelId_,
      apiId = pApiId_
    }

-- | The name of the model.
updateModel_name :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_name = Lens.lens (\UpdateModel' {name} -> name) (\s@UpdateModel' {} a -> s {name = a} :: UpdateModel)

-- | The description of the model.
updateModel_description :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_description = Lens.lens (\UpdateModel' {description} -> description) (\s@UpdateModel' {} a -> s {description = a} :: UpdateModel)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
updateModel_schema :: Lens.Lens' UpdateModel (Prelude.Maybe Prelude.Text)
updateModel_schema = Lens.lens (\UpdateModel' {schema} -> schema) (\s@UpdateModel' {} a -> s {schema = a} :: UpdateModel)

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
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelResponse'
            Prelude.<$> (x Data..?> "name")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "schema")
            Prelude.<*> (x Data..?> "modelId")
            Prelude.<*> (x Data..?> "contentType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateModel where
  hashWithSalt _salt UpdateModel' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData UpdateModel where
  rnf UpdateModel' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders UpdateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModel where
  toJSON UpdateModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("description" Data..=) Prelude.<$> description,
            ("schema" Data..=) Prelude.<$> schema,
            ("contentType" Data..=) Prelude.<$> contentType
          ]
      )

instance Data.ToPath UpdateModel where
  toPath UpdateModel' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/models/",
        Data.toBS modelId
      ]

instance Data.ToQuery UpdateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelResponse' smart constructor.
data UpdateModelResponse = UpdateModelResponse'
  { -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The model identifier.
    modelId :: Prelude.Maybe Prelude.Text,
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
-- 'name', 'updateModelResponse_name' - The name of the model. Must be alphanumeric.
--
-- 'description', 'updateModelResponse_description' - The description of the model.
--
-- 'schema', 'updateModelResponse_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'modelId', 'updateModelResponse_modelId' - The model identifier.
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
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      schema = Prelude.Nothing,
      modelId = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the model. Must be alphanumeric.
updateModelResponse_name :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_name = Lens.lens (\UpdateModelResponse' {name} -> name) (\s@UpdateModelResponse' {} a -> s {name = a} :: UpdateModelResponse)

-- | The description of the model.
updateModelResponse_description :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_description = Lens.lens (\UpdateModelResponse' {description} -> description) (\s@UpdateModelResponse' {} a -> s {description = a} :: UpdateModelResponse)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
updateModelResponse_schema :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_schema = Lens.lens (\UpdateModelResponse' {schema} -> schema) (\s@UpdateModelResponse' {} a -> s {schema = a} :: UpdateModelResponse)

-- | The model identifier.
updateModelResponse_modelId :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_modelId = Lens.lens (\UpdateModelResponse' {modelId} -> modelId) (\s@UpdateModelResponse' {} a -> s {modelId = a} :: UpdateModelResponse)

-- | The content-type for the model, for example, \"application\/json\".
updateModelResponse_contentType :: Lens.Lens' UpdateModelResponse (Prelude.Maybe Prelude.Text)
updateModelResponse_contentType = Lens.lens (\UpdateModelResponse' {contentType} -> contentType) (\s@UpdateModelResponse' {} a -> s {contentType = a} :: UpdateModelResponse)

-- | The response's http status code.
updateModelResponse_httpStatus :: Lens.Lens' UpdateModelResponse Prelude.Int
updateModelResponse_httpStatus = Lens.lens (\UpdateModelResponse' {httpStatus} -> httpStatus) (\s@UpdateModelResponse' {} a -> s {httpStatus = a} :: UpdateModelResponse)

instance Prelude.NFData UpdateModelResponse where
  rnf UpdateModelResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus

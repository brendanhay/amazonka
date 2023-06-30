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
-- Module      : Amazonka.ApiGatewayV2.CreateModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Model for an API.
module Amazonka.ApiGatewayV2.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_contentType,
    createModel_description,
    createModel_apiId,
    createModel_schema,
    createModel_name,

    -- * Destructuring the Response
    CreateModelResponse (..),
    newCreateModelResponse,

    -- * Response Lenses
    createModelResponse_contentType,
    createModelResponse_description,
    createModelResponse_modelId,
    createModelResponse_name,
    createModelResponse_schema,
    createModelResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new Model.
--
-- /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Text,
    -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'createModel_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'description', 'createModel_description' - The description of the model.
--
-- 'apiId', 'createModel_apiId' - The API identifier.
--
-- 'schema', 'createModel_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'name', 'createModel_name' - The name of the model. Must be alphanumeric.
newCreateModel ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'schema'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateModel
newCreateModel pApiId_ pSchema_ pName_ =
  CreateModel'
    { contentType = Prelude.Nothing,
      description = Prelude.Nothing,
      apiId = pApiId_,
      schema = pSchema_,
      name = pName_
    }

-- | The content-type for the model, for example, \"application\/json\".
createModel_contentType :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_contentType = Lens.lens (\CreateModel' {contentType} -> contentType) (\s@CreateModel' {} a -> s {contentType = a} :: CreateModel)

-- | The description of the model.
createModel_description :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_description = Lens.lens (\CreateModel' {description} -> description) (\s@CreateModel' {} a -> s {description = a} :: CreateModel)

-- | The API identifier.
createModel_apiId :: Lens.Lens' CreateModel Prelude.Text
createModel_apiId = Lens.lens (\CreateModel' {apiId} -> apiId) (\s@CreateModel' {} a -> s {apiId = a} :: CreateModel)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
createModel_schema :: Lens.Lens' CreateModel Prelude.Text
createModel_schema = Lens.lens (\CreateModel' {schema} -> schema) (\s@CreateModel' {} a -> s {schema = a} :: CreateModel)

-- | The name of the model. Must be alphanumeric.
createModel_name :: Lens.Lens' CreateModel Prelude.Text
createModel_name = Lens.lens (\CreateModel' {name} -> name) (\s@CreateModel' {} a -> s {name = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = CreateModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelResponse'
            Prelude.<$> (x Data..?> "contentType")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "modelId")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "schema")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateModel where
  hashWithSalt _salt CreateModel' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateModel where
  rnf CreateModel' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentType" Data..=) Prelude.<$> contentType,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("schema" Data..= schema),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateModel where
  toPath CreateModel' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/models"]

instance Data.ToQuery CreateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The model identifier.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'createModelResponse_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'description', 'createModelResponse_description' - The description of the model.
--
-- 'modelId', 'createModelResponse_modelId' - The model identifier.
--
-- 'name', 'createModelResponse_name' - The name of the model. Must be alphanumeric.
--
-- 'schema', 'createModelResponse_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'httpStatus', 'createModelResponse_httpStatus' - The response's http status code.
newCreateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateModelResponse
newCreateModelResponse pHttpStatus_ =
  CreateModelResponse'
    { contentType = Prelude.Nothing,
      description = Prelude.Nothing,
      modelId = Prelude.Nothing,
      name = Prelude.Nothing,
      schema = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content-type for the model, for example, \"application\/json\".
createModelResponse_contentType :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_contentType = Lens.lens (\CreateModelResponse' {contentType} -> contentType) (\s@CreateModelResponse' {} a -> s {contentType = a} :: CreateModelResponse)

-- | The description of the model.
createModelResponse_description :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_description = Lens.lens (\CreateModelResponse' {description} -> description) (\s@CreateModelResponse' {} a -> s {description = a} :: CreateModelResponse)

-- | The model identifier.
createModelResponse_modelId :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_modelId = Lens.lens (\CreateModelResponse' {modelId} -> modelId) (\s@CreateModelResponse' {} a -> s {modelId = a} :: CreateModelResponse)

-- | The name of the model. Must be alphanumeric.
createModelResponse_name :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_name = Lens.lens (\CreateModelResponse' {name} -> name) (\s@CreateModelResponse' {} a -> s {name = a} :: CreateModelResponse)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
createModelResponse_schema :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_schema = Lens.lens (\CreateModelResponse' {schema} -> schema) (\s@CreateModelResponse' {} a -> s {schema = a} :: CreateModelResponse)

-- | The response's http status code.
createModelResponse_httpStatus :: Lens.Lens' CreateModelResponse Prelude.Int
createModelResponse_httpStatus = Lens.lens (\CreateModelResponse' {httpStatus} -> httpStatus) (\s@CreateModelResponse' {} a -> s {httpStatus = a} :: CreateModelResponse)

instance Prelude.NFData CreateModelResponse where
  rnf CreateModelResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf httpStatus

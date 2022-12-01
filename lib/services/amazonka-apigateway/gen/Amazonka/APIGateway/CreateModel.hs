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
-- Module      : Amazonka.APIGateway.CreateModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new Model resource to an existing RestApi resource.
module Amazonka.APIGateway.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_description,
    createModel_schema,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,

    -- * Destructuring the Response
    Model (..),
    newModel,

    -- * Response Lenses
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to add a new Model to an existing RestApi resource.
--
-- /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For @application\/json@ models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The RestApi identifier under which the Model will be created.
    restApiId :: Prelude.Text,
    -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Text,
    -- | The content-type for the model.
    contentType :: Prelude.Text
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
-- 'description', 'createModel_description' - The description of the model.
--
-- 'schema', 'createModel_schema' - The schema for the model. For @application\/json@ models, this should be
-- JSON schema draft 4 model.
--
-- 'restApiId', 'createModel_restApiId' - The RestApi identifier under which the Model will be created.
--
-- 'name', 'createModel_name' - The name of the model. Must be alphanumeric.
--
-- 'contentType', 'createModel_contentType' - The content-type for the model.
newCreateModel ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'contentType'
  Prelude.Text ->
  CreateModel
newCreateModel pRestApiId_ pName_ pContentType_ =
  CreateModel'
    { description = Prelude.Nothing,
      schema = Prelude.Nothing,
      restApiId = pRestApiId_,
      name = pName_,
      contentType = pContentType_
    }

-- | The description of the model.
createModel_description :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_description = Lens.lens (\CreateModel' {description} -> description) (\s@CreateModel' {} a -> s {description = a} :: CreateModel)

-- | The schema for the model. For @application\/json@ models, this should be
-- JSON schema draft 4 model.
createModel_schema :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_schema = Lens.lens (\CreateModel' {schema} -> schema) (\s@CreateModel' {} a -> s {schema = a} :: CreateModel)

-- | The RestApi identifier under which the Model will be created.
createModel_restApiId :: Lens.Lens' CreateModel Prelude.Text
createModel_restApiId = Lens.lens (\CreateModel' {restApiId} -> restApiId) (\s@CreateModel' {} a -> s {restApiId = a} :: CreateModel)

-- | The name of the model. Must be alphanumeric.
createModel_name :: Lens.Lens' CreateModel Prelude.Text
createModel_name = Lens.lens (\CreateModel' {name} -> name) (\s@CreateModel' {} a -> s {name = a} :: CreateModel)

-- | The content-type for the model.
createModel_contentType :: Lens.Lens' CreateModel Prelude.Text
createModel_contentType = Lens.lens (\CreateModel' {contentType} -> contentType) (\s@CreateModel' {} a -> s {contentType = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = Model
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateModel where
  hashWithSalt _salt CreateModel' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` contentType

instance Prelude.NFData CreateModel where
  rnf CreateModel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf contentType

instance Core.ToHeaders CreateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("schema" Core..=) Prelude.<$> schema,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("contentType" Core..= contentType)
          ]
      )

instance Core.ToPath CreateModel where
  toPath CreateModel' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/models"]

instance Core.ToQuery CreateModel where
  toQuery = Prelude.const Prelude.mempty

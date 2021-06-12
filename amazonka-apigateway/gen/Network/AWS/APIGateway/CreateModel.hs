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
-- Module      : Network.AWS.APIGateway.CreateModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new Model resource to an existing RestApi resource.
module Network.AWS.APIGateway.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_schema,
    createModel_description,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,

    -- * Destructuring the Response
    Model (..),
    newModel,

    -- * Response Lenses
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add a new Model to an existing RestApi resource.
--
-- /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | The schema for the model. For @application\/json@ models, this should be
    -- <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4>
    -- model.
    schema :: Core.Maybe Core.Text,
    -- | The description of the model.
    description :: Core.Maybe Core.Text,
    -- | [Required] The RestApi identifier under which the Model will be created.
    restApiId :: Core.Text,
    -- | [Required] The name of the model. Must be alphanumeric.
    name :: Core.Text,
    -- | [Required] The content-type for the model.
    contentType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'createModel_schema' - The schema for the model. For @application\/json@ models, this should be
-- <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4>
-- model.
--
-- 'description', 'createModel_description' - The description of the model.
--
-- 'restApiId', 'createModel_restApiId' - [Required] The RestApi identifier under which the Model will be created.
--
-- 'name', 'createModel_name' - [Required] The name of the model. Must be alphanumeric.
--
-- 'contentType', 'createModel_contentType' - [Required] The content-type for the model.
newCreateModel ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'contentType'
  Core.Text ->
  CreateModel
newCreateModel pRestApiId_ pName_ pContentType_ =
  CreateModel'
    { schema = Core.Nothing,
      description = Core.Nothing,
      restApiId = pRestApiId_,
      name = pName_,
      contentType = pContentType_
    }

-- | The schema for the model. For @application\/json@ models, this should be
-- <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4>
-- model.
createModel_schema :: Lens.Lens' CreateModel (Core.Maybe Core.Text)
createModel_schema = Lens.lens (\CreateModel' {schema} -> schema) (\s@CreateModel' {} a -> s {schema = a} :: CreateModel)

-- | The description of the model.
createModel_description :: Lens.Lens' CreateModel (Core.Maybe Core.Text)
createModel_description = Lens.lens (\CreateModel' {description} -> description) (\s@CreateModel' {} a -> s {description = a} :: CreateModel)

-- | [Required] The RestApi identifier under which the Model will be created.
createModel_restApiId :: Lens.Lens' CreateModel Core.Text
createModel_restApiId = Lens.lens (\CreateModel' {restApiId} -> restApiId) (\s@CreateModel' {} a -> s {restApiId = a} :: CreateModel)

-- | [Required] The name of the model. Must be alphanumeric.
createModel_name :: Lens.Lens' CreateModel Core.Text
createModel_name = Lens.lens (\CreateModel' {name} -> name) (\s@CreateModel' {} a -> s {name = a} :: CreateModel)

-- | [Required] The content-type for the model.
createModel_contentType :: Lens.Lens' CreateModel Core.Text
createModel_contentType = Lens.lens (\CreateModel' {contentType} -> contentType) (\s@CreateModel' {} a -> s {contentType = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = Model
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateModel

instance Core.NFData CreateModel

instance Core.ToHeaders CreateModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("schema" Core..=) Core.<$> schema,
            ("description" Core..=) Core.<$> description,
            Core.Just ("name" Core..= name),
            Core.Just ("contentType" Core..= contentType)
          ]
      )

instance Core.ToPath CreateModel where
  toPath CreateModel' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/models"]

instance Core.ToQuery CreateModel where
  toQuery = Core.const Core.mempty

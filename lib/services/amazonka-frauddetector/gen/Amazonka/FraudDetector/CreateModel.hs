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
-- Module      : Amazonka.FraudDetector.CreateModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model using the specified model type.
module Amazonka.FraudDetector.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_tags,
    createModel_description,
    createModel_modelId,
    createModel_modelType,
    createModel_eventTypeName,

    -- * Destructuring the Response
    CreateModelResponse (..),
    newCreateModelResponse,

    -- * Response Lenses
    createModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The model description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum,
    -- | The name of the event type.
    eventTypeName :: Prelude.Text
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
-- 'tags', 'createModel_tags' - A collection of key and value pairs.
--
-- 'description', 'createModel_description' - The model description.
--
-- 'modelId', 'createModel_modelId' - The model ID.
--
-- 'modelType', 'createModel_modelType' - The model type.
--
-- 'eventTypeName', 'createModel_eventTypeName' - The name of the event type.
newCreateModel ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'eventTypeName'
  Prelude.Text ->
  CreateModel
newCreateModel pModelId_ pModelType_ pEventTypeName_ =
  CreateModel'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      modelId = pModelId_,
      modelType = pModelType_,
      eventTypeName = pEventTypeName_
    }

-- | A collection of key and value pairs.
createModel_tags :: Lens.Lens' CreateModel (Prelude.Maybe [Tag])
createModel_tags = Lens.lens (\CreateModel' {tags} -> tags) (\s@CreateModel' {} a -> s {tags = a} :: CreateModel) Prelude.. Lens.mapping Lens.coerced

-- | The model description.
createModel_description :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_description = Lens.lens (\CreateModel' {description} -> description) (\s@CreateModel' {} a -> s {description = a} :: CreateModel)

-- | The model ID.
createModel_modelId :: Lens.Lens' CreateModel Prelude.Text
createModel_modelId = Lens.lens (\CreateModel' {modelId} -> modelId) (\s@CreateModel' {} a -> s {modelId = a} :: CreateModel)

-- | The model type.
createModel_modelType :: Lens.Lens' CreateModel ModelTypeEnum
createModel_modelType = Lens.lens (\CreateModel' {modelType} -> modelType) (\s@CreateModel' {} a -> s {modelType = a} :: CreateModel)

-- | The name of the event type.
createModel_eventTypeName :: Lens.Lens' CreateModel Prelude.Text
createModel_eventTypeName = Lens.lens (\CreateModel' {eventTypeName} -> eventTypeName) (\s@CreateModel' {} a -> s {eventTypeName = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = CreateModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateModel where
  hashWithSalt _salt CreateModel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` eventTypeName

instance Prelude.NFData CreateModel where
  rnf CreateModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf eventTypeName

instance Data.ToHeaders CreateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CreateModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("eventTypeName" Data..= eventTypeName)
          ]
      )

instance Data.ToPath CreateModel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'createModelResponse_httpStatus' - The response's http status code.
newCreateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateModelResponse
newCreateModelResponse pHttpStatus_ =
  CreateModelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createModelResponse_httpStatus :: Lens.Lens' CreateModelResponse Prelude.Int
createModelResponse_httpStatus = Lens.lens (\CreateModelResponse' {httpStatus} -> httpStatus) (\s@CreateModelResponse' {} a -> s {httpStatus = a} :: CreateModelResponse)

instance Prelude.NFData CreateModelResponse where
  rnf CreateModelResponse' {..} = Prelude.rnf httpStatus

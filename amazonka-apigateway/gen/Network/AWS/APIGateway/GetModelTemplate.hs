{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.GetModelTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a sample mapping template that can be used to transform a
-- payload into the structure of a model.
module Network.AWS.APIGateway.GetModelTemplate
  ( -- * Creating a Request
    GetModelTemplate (..),
    newGetModelTemplate,

    -- * Request Lenses
    getModelTemplate_restApiId,
    getModelTemplate_modelName,

    -- * Destructuring the Response
    GetModelTemplateResponse (..),
    newGetModelTemplateResponse,

    -- * Response Lenses
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to generate a sample mapping template used to transform the
-- payload.
--
-- /See:/ 'newGetModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the model for which to generate a template.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetModelTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getModelTemplate_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'modelName', 'getModelTemplate_modelName' - [Required] The name of the model for which to generate a template.
newGetModelTemplate ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  GetModelTemplate
newGetModelTemplate pRestApiId_ pModelName_ =
  GetModelTemplate'
    { restApiId = pRestApiId_,
      modelName = pModelName_
    }

-- | [Required] The string identifier of the associated RestApi.
getModelTemplate_restApiId :: Lens.Lens' GetModelTemplate Prelude.Text
getModelTemplate_restApiId = Lens.lens (\GetModelTemplate' {restApiId} -> restApiId) (\s@GetModelTemplate' {} a -> s {restApiId = a} :: GetModelTemplate)

-- | [Required] The name of the model for which to generate a template.
getModelTemplate_modelName :: Lens.Lens' GetModelTemplate Prelude.Text
getModelTemplate_modelName = Lens.lens (\GetModelTemplate' {modelName} -> modelName) (\s@GetModelTemplate' {} a -> s {modelName = a} :: GetModelTemplate)

instance Prelude.AWSRequest GetModelTemplate where
  type Rs GetModelTemplate = GetModelTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelTemplateResponse'
            Prelude.<$> (x Prelude..?> "value")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModelTemplate

instance Prelude.NFData GetModelTemplate

instance Prelude.ToHeaders GetModelTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetModelTemplate where
  toPath GetModelTemplate' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/models/",
        Prelude.toBS modelName,
        "/default_template"
      ]

instance Prelude.ToQuery GetModelTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | Represents a mapping template used to transform a payload.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings Mapping Templates>
--
-- /See:/ 'newGetModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { -- | The Apache
    -- <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)>
    -- template content used for the template resource.
    value :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetModelTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'getModelTemplateResponse_value' - The Apache
-- <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)>
-- template content used for the template resource.
--
-- 'httpStatus', 'getModelTemplateResponse_httpStatus' - The response's http status code.
newGetModelTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetModelTemplateResponse
newGetModelTemplateResponse pHttpStatus_ =
  GetModelTemplateResponse'
    { value = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Apache
-- <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)>
-- template content used for the template resource.
getModelTemplateResponse_value :: Lens.Lens' GetModelTemplateResponse (Prelude.Maybe Prelude.Text)
getModelTemplateResponse_value = Lens.lens (\GetModelTemplateResponse' {value} -> value) (\s@GetModelTemplateResponse' {} a -> s {value = a} :: GetModelTemplateResponse)

-- | The response's http status code.
getModelTemplateResponse_httpStatus :: Lens.Lens' GetModelTemplateResponse Prelude.Int
getModelTemplateResponse_httpStatus = Lens.lens (\GetModelTemplateResponse' {httpStatus} -> httpStatus) (\s@GetModelTemplateResponse' {} a -> s {httpStatus = a} :: GetModelTemplateResponse)

instance Prelude.NFData GetModelTemplateResponse

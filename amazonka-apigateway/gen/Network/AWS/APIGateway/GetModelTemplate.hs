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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to generate a sample mapping template used to transform the
-- payload.
--
-- /See:/ 'newGetModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The name of the model for which to generate a template.
    modelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'modelName'
  Core.Text ->
  GetModelTemplate
newGetModelTemplate pRestApiId_ pModelName_ =
  GetModelTemplate'
    { restApiId = pRestApiId_,
      modelName = pModelName_
    }

-- | [Required] The string identifier of the associated RestApi.
getModelTemplate_restApiId :: Lens.Lens' GetModelTemplate Core.Text
getModelTemplate_restApiId = Lens.lens (\GetModelTemplate' {restApiId} -> restApiId) (\s@GetModelTemplate' {} a -> s {restApiId = a} :: GetModelTemplate)

-- | [Required] The name of the model for which to generate a template.
getModelTemplate_modelName :: Lens.Lens' GetModelTemplate Core.Text
getModelTemplate_modelName = Lens.lens (\GetModelTemplate' {modelName} -> modelName) (\s@GetModelTemplate' {} a -> s {modelName = a} :: GetModelTemplate)

instance Core.AWSRequest GetModelTemplate where
  type
    AWSResponse GetModelTemplate =
      GetModelTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelTemplateResponse'
            Core.<$> (x Core..?> "value")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetModelTemplate

instance Core.NFData GetModelTemplate

instance Core.ToHeaders GetModelTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetModelTemplate where
  toPath GetModelTemplate' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/models/",
        Core.toBS modelName,
        "/default_template"
      ]

instance Core.ToQuery GetModelTemplate where
  toQuery = Core.const Core.mempty

-- | Represents a mapping template used to transform a payload.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings Mapping Templates>
--
-- /See:/ 'newGetModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { -- | The Apache
    -- <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)>
    -- template content used for the template resource.
    value :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetModelTemplateResponse
newGetModelTemplateResponse pHttpStatus_ =
  GetModelTemplateResponse'
    { value = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Apache
-- <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)>
-- template content used for the template resource.
getModelTemplateResponse_value :: Lens.Lens' GetModelTemplateResponse (Core.Maybe Core.Text)
getModelTemplateResponse_value = Lens.lens (\GetModelTemplateResponse' {value} -> value) (\s@GetModelTemplateResponse' {} a -> s {value = a} :: GetModelTemplateResponse)

-- | The response's http status code.
getModelTemplateResponse_httpStatus :: Lens.Lens' GetModelTemplateResponse Core.Int
getModelTemplateResponse_httpStatus = Lens.lens (\GetModelTemplateResponse' {httpStatus} -> httpStatus) (\s@GetModelTemplateResponse' {} a -> s {httpStatus = a} :: GetModelTemplateResponse)

instance Core.NFData GetModelTemplateResponse

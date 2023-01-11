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
-- Module      : Amazonka.APIGateway.GetModelTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a sample mapping template that can be used to transform a
-- payload into the structure of a model.
module Amazonka.APIGateway.GetModelTemplate
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to generate a sample mapping template used to transform the
-- payload.
--
-- /See:/ 'newGetModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the model for which to generate a template.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getModelTemplate_restApiId' - The string identifier of the associated RestApi.
--
-- 'modelName', 'getModelTemplate_modelName' - The name of the model for which to generate a template.
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

-- | The string identifier of the associated RestApi.
getModelTemplate_restApiId :: Lens.Lens' GetModelTemplate Prelude.Text
getModelTemplate_restApiId = Lens.lens (\GetModelTemplate' {restApiId} -> restApiId) (\s@GetModelTemplate' {} a -> s {restApiId = a} :: GetModelTemplate)

-- | The name of the model for which to generate a template.
getModelTemplate_modelName :: Lens.Lens' GetModelTemplate Prelude.Text
getModelTemplate_modelName = Lens.lens (\GetModelTemplate' {modelName} -> modelName) (\s@GetModelTemplate' {} a -> s {modelName = a} :: GetModelTemplate)

instance Core.AWSRequest GetModelTemplate where
  type
    AWSResponse GetModelTemplate =
      GetModelTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelTemplateResponse'
            Prelude.<$> (x Data..?> "value")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModelTemplate where
  hashWithSalt _salt GetModelTemplate' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData GetModelTemplate where
  rnf GetModelTemplate' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf modelName

instance Data.ToHeaders GetModelTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetModelTemplate where
  toPath GetModelTemplate' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/models/",
        Data.toBS modelName,
        "/default_template"
      ]

instance Data.ToQuery GetModelTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | Represents a mapping template used to transform a payload.
--
-- /See:/ 'newGetModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { -- | The Apache Velocity Template Language (VTL) template content used for
    -- the template resource.
    value :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'getModelTemplateResponse_value' - The Apache Velocity Template Language (VTL) template content used for
-- the template resource.
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

-- | The Apache Velocity Template Language (VTL) template content used for
-- the template resource.
getModelTemplateResponse_value :: Lens.Lens' GetModelTemplateResponse (Prelude.Maybe Prelude.Text)
getModelTemplateResponse_value = Lens.lens (\GetModelTemplateResponse' {value} -> value) (\s@GetModelTemplateResponse' {} a -> s {value = a} :: GetModelTemplateResponse)

-- | The response's http status code.
getModelTemplateResponse_httpStatus :: Lens.Lens' GetModelTemplateResponse Prelude.Int
getModelTemplateResponse_httpStatus = Lens.lens (\GetModelTemplateResponse' {httpStatus} -> httpStatus) (\s@GetModelTemplateResponse' {} a -> s {httpStatus = a} :: GetModelTemplateResponse)

instance Prelude.NFData GetModelTemplateResponse where
  rnf GetModelTemplateResponse' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.APIGateway.GetExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a deployed version of a RestApi in a specified format.
module Amazonka.APIGateway.GetExport
  ( -- * Creating a Request
    GetExport (..),
    newGetExport,

    -- * Request Lenses
    getExport_accepts,
    getExport_parameters,
    getExport_restApiId,
    getExport_stageName,
    getExport_exportType,

    -- * Destructuring the Response
    GetExportResponse (..),
    newGetExportResponse,

    -- * Response Lenses
    getExportResponse_body,
    getExportResponse_contentDisposition,
    getExportResponse_contentType,
    getExportResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request a new export of a RestApi for a particular Stage.
--
-- /See:/ 'newGetExport' smart constructor.
data GetExport = GetExport'
  { -- | The content-type of the export, for example @application\/json@.
    -- Currently @application\/json@ and @application\/yaml@ are supported for
    -- @exportType@ of@oas30@ and @swagger@. This should be specified in the
    -- @Accept@ header for direct API requests.
    accepts :: Prelude.Maybe Prelude.Text,
    -- | A key-value map of query string parameters that specify properties of
    -- the export, depending on the requested @exportType@. For @exportType@
    -- @oas30@ and @swagger@, any combination of the following parameters are
    -- supported: @extensions=\'integrations\'@ or @extensions=\'apigateway\'@
    -- will export the API with x-amazon-apigateway-integration extensions.
    -- @extensions=\'authorizers\'@ will export the API with
    -- x-amazon-apigateway-authorizer extensions. @postman@ will export the API
    -- with Postman extensions, allowing for import to the Postman tool
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the Stage that will be exported.
    stageName :: Prelude.Text,
    -- | The type of export. Acceptable values are \'oas30\' for OpenAPI 3.0.x
    -- and \'swagger\' for Swagger\/OpenAPI 2.0.
    exportType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accepts', 'getExport_accepts' - The content-type of the export, for example @application\/json@.
-- Currently @application\/json@ and @application\/yaml@ are supported for
-- @exportType@ of@oas30@ and @swagger@. This should be specified in the
-- @Accept@ header for direct API requests.
--
-- 'parameters', 'getExport_parameters' - A key-value map of query string parameters that specify properties of
-- the export, depending on the requested @exportType@. For @exportType@
-- @oas30@ and @swagger@, any combination of the following parameters are
-- supported: @extensions=\'integrations\'@ or @extensions=\'apigateway\'@
-- will export the API with x-amazon-apigateway-integration extensions.
-- @extensions=\'authorizers\'@ will export the API with
-- x-amazon-apigateway-authorizer extensions. @postman@ will export the API
-- with Postman extensions, allowing for import to the Postman tool
--
-- 'restApiId', 'getExport_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'getExport_stageName' - The name of the Stage that will be exported.
--
-- 'exportType', 'getExport_exportType' - The type of export. Acceptable values are \'oas30\' for OpenAPI 3.0.x
-- and \'swagger\' for Swagger\/OpenAPI 2.0.
newGetExport ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'exportType'
  Prelude.Text ->
  GetExport
newGetExport pRestApiId_ pStageName_ pExportType_ =
  GetExport'
    { accepts = Prelude.Nothing,
      parameters = Prelude.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      exportType = pExportType_
    }

-- | The content-type of the export, for example @application\/json@.
-- Currently @application\/json@ and @application\/yaml@ are supported for
-- @exportType@ of@oas30@ and @swagger@. This should be specified in the
-- @Accept@ header for direct API requests.
getExport_accepts :: Lens.Lens' GetExport (Prelude.Maybe Prelude.Text)
getExport_accepts = Lens.lens (\GetExport' {accepts} -> accepts) (\s@GetExport' {} a -> s {accepts = a} :: GetExport)

-- | A key-value map of query string parameters that specify properties of
-- the export, depending on the requested @exportType@. For @exportType@
-- @oas30@ and @swagger@, any combination of the following parameters are
-- supported: @extensions=\'integrations\'@ or @extensions=\'apigateway\'@
-- will export the API with x-amazon-apigateway-integration extensions.
-- @extensions=\'authorizers\'@ will export the API with
-- x-amazon-apigateway-authorizer extensions. @postman@ will export the API
-- with Postman extensions, allowing for import to the Postman tool
getExport_parameters :: Lens.Lens' GetExport (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getExport_parameters = Lens.lens (\GetExport' {parameters} -> parameters) (\s@GetExport' {} a -> s {parameters = a} :: GetExport) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
getExport_restApiId :: Lens.Lens' GetExport Prelude.Text
getExport_restApiId = Lens.lens (\GetExport' {restApiId} -> restApiId) (\s@GetExport' {} a -> s {restApiId = a} :: GetExport)

-- | The name of the Stage that will be exported.
getExport_stageName :: Lens.Lens' GetExport Prelude.Text
getExport_stageName = Lens.lens (\GetExport' {stageName} -> stageName) (\s@GetExport' {} a -> s {stageName = a} :: GetExport)

-- | The type of export. Acceptable values are \'oas30\' for OpenAPI 3.0.x
-- and \'swagger\' for Swagger\/OpenAPI 2.0.
getExport_exportType :: Lens.Lens' GetExport Prelude.Text
getExport_exportType = Lens.lens (\GetExport' {exportType} -> exportType) (\s@GetExport' {} a -> s {exportType = a} :: GetExport)

instance Core.AWSRequest GetExport where
  type AWSResponse GetExport = GetExportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetExportResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Data..#? "Content-Disposition")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExport where
  hashWithSalt _salt GetExport' {..} =
    _salt `Prelude.hashWithSalt` accepts
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` exportType

instance Prelude.NFData GetExport where
  rnf GetExport' {..} =
    Prelude.rnf accepts
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf exportType

instance Data.ToHeaders GetExport where
  toHeaders GetExport' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accepts,
        "Accept"
          Data.=# ("application/json" :: Prelude.ByteString)
      ]

instance Data.ToPath GetExport where
  toPath GetExport' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/stages/",
        Data.toBS stageName,
        "/exports/",
        Data.toBS exportType
      ]

instance Data.ToQuery GetExport where
  toQuery GetExport' {..} =
    Prelude.mconcat
      [ "parameters"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            )
      ]

-- | The binary blob response to GetExport, which contains the generated SDK.
--
-- /See:/ 'newGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The binary blob response to GetExport, which contains the export.
    body :: Prelude.Maybe Prelude.ByteString,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The content-type header value in the HTTP response. This will correspond
    -- to a valid \'accept\' type in the request.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'getExportResponse_body' - The binary blob response to GetExport, which contains the export.
--
-- 'contentDisposition', 'getExportResponse_contentDisposition' - The content-disposition header value in the HTTP response.
--
-- 'contentType', 'getExportResponse_contentType' - The content-type header value in the HTTP response. This will correspond
-- to a valid \'accept\' type in the request.
--
-- 'httpStatus', 'getExportResponse_httpStatus' - The response's http status code.
newGetExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExportResponse
newGetExportResponse pHttpStatus_ =
  GetExportResponse'
    { body = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The binary blob response to GetExport, which contains the export.
getExportResponse_body :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.ByteString)
getExportResponse_body = Lens.lens (\GetExportResponse' {body} -> body) (\s@GetExportResponse' {} a -> s {body = a} :: GetExportResponse)

-- | The content-disposition header value in the HTTP response.
getExportResponse_contentDisposition :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_contentDisposition = Lens.lens (\GetExportResponse' {contentDisposition} -> contentDisposition) (\s@GetExportResponse' {} a -> s {contentDisposition = a} :: GetExportResponse)

-- | The content-type header value in the HTTP response. This will correspond
-- to a valid \'accept\' type in the request.
getExportResponse_contentType :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_contentType = Lens.lens (\GetExportResponse' {contentType} -> contentType) (\s@GetExportResponse' {} a -> s {contentType = a} :: GetExportResponse)

-- | The response's http status code.
getExportResponse_httpStatus :: Lens.Lens' GetExportResponse Prelude.Int
getExportResponse_httpStatus = Lens.lens (\GetExportResponse' {httpStatus} -> httpStatus) (\s@GetExportResponse' {} a -> s {httpStatus = a} :: GetExportResponse)

instance Prelude.NFData GetExportResponse where
  rnf GetExportResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf contentDisposition
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus

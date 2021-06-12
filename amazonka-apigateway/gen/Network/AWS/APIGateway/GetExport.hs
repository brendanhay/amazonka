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
-- Module      : Network.AWS.APIGateway.GetExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a deployed version of a RestApi in a specified format.
module Network.AWS.APIGateway.GetExport
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
    getExportResponse_contentType,
    getExportResponse_contentDisposition,
    getExportResponse_body,
    getExportResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request a new export of a RestApi for a particular Stage.
--
-- /See:/ 'newGetExport' smart constructor.
data GetExport = GetExport'
  { -- | The content-type of the export, for example @application\/json@.
    -- Currently @application\/json@ and @application\/yaml@ are supported for
    -- @exportType@ of@oas30@ and @swagger@. This should be specified in the
    -- @Accept@ header for direct API requests.
    accepts :: Core.Maybe Core.Text,
    -- | A key-value map of query string parameters that specify properties of
    -- the export, depending on the requested @exportType@. For @exportType@
    -- @oas30@ and @swagger@, any combination of the following parameters are
    -- supported: @extensions=\'integrations\'@ or @extensions=\'apigateway\'@
    -- will export the API with x-amazon-apigateway-integration extensions.
    -- @extensions=\'authorizers\'@ will export the API with
    -- x-amazon-apigateway-authorizer extensions. @postman@ will export the API
    -- with Postman extensions, allowing for import to the Postman tool
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The name of the Stage that will be exported.
    stageName :: Core.Text,
    -- | [Required] The type of export. Acceptable values are \'oas30\' for
    -- OpenAPI 3.0.x and \'swagger\' for Swagger\/OpenAPI 2.0.
    exportType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'restApiId', 'getExport_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'stageName', 'getExport_stageName' - [Required] The name of the Stage that will be exported.
--
-- 'exportType', 'getExport_exportType' - [Required] The type of export. Acceptable values are \'oas30\' for
-- OpenAPI 3.0.x and \'swagger\' for Swagger\/OpenAPI 2.0.
newGetExport ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'stageName'
  Core.Text ->
  -- | 'exportType'
  Core.Text ->
  GetExport
newGetExport pRestApiId_ pStageName_ pExportType_ =
  GetExport'
    { accepts = Core.Nothing,
      parameters = Core.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      exportType = pExportType_
    }

-- | The content-type of the export, for example @application\/json@.
-- Currently @application\/json@ and @application\/yaml@ are supported for
-- @exportType@ of@oas30@ and @swagger@. This should be specified in the
-- @Accept@ header for direct API requests.
getExport_accepts :: Lens.Lens' GetExport (Core.Maybe Core.Text)
getExport_accepts = Lens.lens (\GetExport' {accepts} -> accepts) (\s@GetExport' {} a -> s {accepts = a} :: GetExport)

-- | A key-value map of query string parameters that specify properties of
-- the export, depending on the requested @exportType@. For @exportType@
-- @oas30@ and @swagger@, any combination of the following parameters are
-- supported: @extensions=\'integrations\'@ or @extensions=\'apigateway\'@
-- will export the API with x-amazon-apigateway-integration extensions.
-- @extensions=\'authorizers\'@ will export the API with
-- x-amazon-apigateway-authorizer extensions. @postman@ will export the API
-- with Postman extensions, allowing for import to the Postman tool
getExport_parameters :: Lens.Lens' GetExport (Core.Maybe (Core.HashMap Core.Text Core.Text))
getExport_parameters = Lens.lens (\GetExport' {parameters} -> parameters) (\s@GetExport' {} a -> s {parameters = a} :: GetExport) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
getExport_restApiId :: Lens.Lens' GetExport Core.Text
getExport_restApiId = Lens.lens (\GetExport' {restApiId} -> restApiId) (\s@GetExport' {} a -> s {restApiId = a} :: GetExport)

-- | [Required] The name of the Stage that will be exported.
getExport_stageName :: Lens.Lens' GetExport Core.Text
getExport_stageName = Lens.lens (\GetExport' {stageName} -> stageName) (\s@GetExport' {} a -> s {stageName = a} :: GetExport)

-- | [Required] The type of export. Acceptable values are \'oas30\' for
-- OpenAPI 3.0.x and \'swagger\' for Swagger\/OpenAPI 2.0.
getExport_exportType :: Lens.Lens' GetExport Core.Text
getExport_exportType = Lens.lens (\GetExport' {exportType} -> exportType) (\s@GetExport' {} a -> s {exportType = a} :: GetExport)

instance Core.AWSRequest GetExport where
  type AWSResponse GetExport = GetExportResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetExportResponse'
            Core.<$> (h Core..#? "Content-Type")
            Core.<*> (h Core..#? "Content-Disposition")
            Core.<*> (Core.pure (Core.Just x))
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetExport

instance Core.NFData GetExport

instance Core.ToHeaders GetExport where
  toHeaders GetExport' {..} =
    Core.mconcat
      [ "Accept" Core.=# accepts,
        "Accept"
          Core.=# ("application/json" :: Core.ByteString)
      ]

instance Core.ToPath GetExport where
  toPath GetExport' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/stages/",
        Core.toBS stageName,
        "/exports/",
        Core.toBS exportType
      ]

instance Core.ToQuery GetExport where
  toQuery GetExport' {..} =
    Core.mconcat
      [ "parameters"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Core.<$> parameters
            )
      ]

-- | The binary blob response to GetExport, which contains the generated SDK.
--
-- /See:/ 'newGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The content-type header value in the HTTP response. This will correspond
    -- to a valid \'accept\' type in the request.
    contentType :: Core.Maybe Core.Text,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Core.Maybe Core.Text,
    -- | The binary blob response to GetExport, which contains the export.
    body :: Core.Maybe Core.ByteString,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getExportResponse_contentType' - The content-type header value in the HTTP response. This will correspond
-- to a valid \'accept\' type in the request.
--
-- 'contentDisposition', 'getExportResponse_contentDisposition' - The content-disposition header value in the HTTP response.
--
-- 'body', 'getExportResponse_body' - The binary blob response to GetExport, which contains the export.
--
-- 'httpStatus', 'getExportResponse_httpStatus' - The response's http status code.
newGetExportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetExportResponse
newGetExportResponse pHttpStatus_ =
  GetExportResponse'
    { contentType = Core.Nothing,
      contentDisposition = Core.Nothing,
      body = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content-type header value in the HTTP response. This will correspond
-- to a valid \'accept\' type in the request.
getExportResponse_contentType :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
getExportResponse_contentType = Lens.lens (\GetExportResponse' {contentType} -> contentType) (\s@GetExportResponse' {} a -> s {contentType = a} :: GetExportResponse)

-- | The content-disposition header value in the HTTP response.
getExportResponse_contentDisposition :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
getExportResponse_contentDisposition = Lens.lens (\GetExportResponse' {contentDisposition} -> contentDisposition) (\s@GetExportResponse' {} a -> s {contentDisposition = a} :: GetExportResponse)

-- | The binary blob response to GetExport, which contains the export.
getExportResponse_body :: Lens.Lens' GetExportResponse (Core.Maybe Core.ByteString)
getExportResponse_body = Lens.lens (\GetExportResponse' {body} -> body) (\s@GetExportResponse' {} a -> s {body = a} :: GetExportResponse)

-- | The response's http status code.
getExportResponse_httpStatus :: Lens.Lens' GetExportResponse Core.Int
getExportResponse_httpStatus = Lens.lens (\GetExportResponse' {httpStatus} -> httpStatus) (\s@GetExportResponse' {} a -> s {httpStatus = a} :: GetExportResponse)

instance Core.NFData GetExportResponse

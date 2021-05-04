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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the Stage that will be exported.
    stageName :: Prelude.Text,
    -- | [Required] The type of export. Acceptable values are \'oas30\' for
    -- OpenAPI 3.0.x and \'swagger\' for Swagger\/OpenAPI 2.0.
    exportType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getExport_parameters = Lens.lens (\GetExport' {parameters} -> parameters) (\s@GetExport' {} a -> s {parameters = a} :: GetExport) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The string identifier of the associated RestApi.
getExport_restApiId :: Lens.Lens' GetExport Prelude.Text
getExport_restApiId = Lens.lens (\GetExport' {restApiId} -> restApiId) (\s@GetExport' {} a -> s {restApiId = a} :: GetExport)

-- | [Required] The name of the Stage that will be exported.
getExport_stageName :: Lens.Lens' GetExport Prelude.Text
getExport_stageName = Lens.lens (\GetExport' {stageName} -> stageName) (\s@GetExport' {} a -> s {stageName = a} :: GetExport)

-- | [Required] The type of export. Acceptable values are \'oas30\' for
-- OpenAPI 3.0.x and \'swagger\' for Swagger\/OpenAPI 2.0.
getExport_exportType :: Lens.Lens' GetExport Prelude.Text
getExport_exportType = Lens.lens (\GetExport' {exportType} -> exportType) (\s@GetExport' {} a -> s {exportType = a} :: GetExport)

instance Prelude.AWSRequest GetExport where
  type Rs GetExport = GetExportResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetExportResponse'
            Prelude.<$> (h Prelude..#? "Content-Type")
            Prelude.<*> (h Prelude..#? "Content-Disposition")
            Prelude.<*> (Prelude.pure (Prelude.Just x))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExport

instance Prelude.NFData GetExport

instance Prelude.ToHeaders GetExport where
  toHeaders GetExport' {..} =
    Prelude.mconcat
      [ "Accept" Prelude.=# accepts,
        "Accept"
          Prelude.=# ("application/json" :: Prelude.ByteString)
      ]

instance Prelude.ToPath GetExport where
  toPath GetExport' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/stages/",
        Prelude.toBS stageName,
        "/exports/",
        Prelude.toBS exportType
      ]

instance Prelude.ToQuery GetExport where
  toQuery GetExport' {..} =
    Prelude.mconcat
      [ "parameters"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            )
      ]

-- | The binary blob response to GetExport, which contains the generated SDK.
--
-- /See:/ 'newGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The content-type header value in the HTTP response. This will correspond
    -- to a valid \'accept\' type in the request.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The binary blob response to GetExport, which contains the export.
    body :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetExportResponse
newGetExportResponse pHttpStatus_ =
  GetExportResponse'
    { contentType = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      body = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content-type header value in the HTTP response. This will correspond
-- to a valid \'accept\' type in the request.
getExportResponse_contentType :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_contentType = Lens.lens (\GetExportResponse' {contentType} -> contentType) (\s@GetExportResponse' {} a -> s {contentType = a} :: GetExportResponse)

-- | The content-disposition header value in the HTTP response.
getExportResponse_contentDisposition :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_contentDisposition = Lens.lens (\GetExportResponse' {contentDisposition} -> contentDisposition) (\s@GetExportResponse' {} a -> s {contentDisposition = a} :: GetExportResponse)

-- | The binary blob response to GetExport, which contains the export.
getExportResponse_body :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.ByteString)
getExportResponse_body = Lens.lens (\GetExportResponse' {body} -> body) (\s@GetExportResponse' {} a -> s {body = a} :: GetExportResponse)

-- | The response's http status code.
getExportResponse_httpStatus :: Lens.Lens' GetExportResponse Prelude.Int
getExportResponse_httpStatus = Lens.lens (\GetExportResponse' {httpStatus} -> httpStatus) (\s@GetExportResponse' {} a -> s {httpStatus = a} :: GetExportResponse)

instance Prelude.NFData GetExportResponse

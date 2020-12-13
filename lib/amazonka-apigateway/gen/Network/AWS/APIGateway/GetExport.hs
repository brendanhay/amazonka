{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a deployed version of a 'RestApi' in a specified format.
module Network.AWS.APIGateway.GetExport
  ( -- * Creating a request
    GetExport (..),
    mkGetExport,

    -- ** Request lenses
    geExportType,
    geParameters,
    geRestAPIId,
    geAccepts,
    geStageName,

    -- * Destructuring the response
    GetExportResponse (..),
    mkGetExportResponse,

    -- ** Response lenses
    gersBody,
    gersContentDisposition,
    gersContentType,
    gersResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request a new export of a 'RestApi' for a particular 'Stage' .
--
-- /See:/ 'mkGetExport' smart constructor.
data GetExport = GetExport'
  { -- | [Required] The type of export. Acceptable values are 'oas30' for OpenAPI 3.0.x and 'swagger' for Swagger/OpenAPI 2.0.
    exportType :: Lude.Text,
    -- | A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @oas30@ and @swagger@ , any combination of the following parameters are supported: @extensions='integrations'@ or @extensions='apigateway'@ will export the API with x-amazon-apigateway-integration extensions. @extensions='authorizers'@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of@oas30@ and @swagger@ . This should be specified in the @Accept@ header for direct API requests.
    accepts :: Lude.Maybe Lude.Text,
    -- | [Required] The name of the 'Stage' that will be exported.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExport' with the minimum fields required to make a request.
--
-- * 'exportType' - [Required] The type of export. Acceptable values are 'oas30' for OpenAPI 3.0.x and 'swagger' for Swagger/OpenAPI 2.0.
-- * 'parameters' - A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @oas30@ and @swagger@ , any combination of the following parameters are supported: @extensions='integrations'@ or @extensions='apigateway'@ will export the API with x-amazon-apigateway-integration extensions. @extensions='authorizers'@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'accepts' - The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of@oas30@ and @swagger@ . This should be specified in the @Accept@ header for direct API requests.
-- * 'stageName' - [Required] The name of the 'Stage' that will be exported.
mkGetExport ::
  -- | 'exportType'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  GetExport
mkGetExport pExportType_ pRestAPIId_ pStageName_ =
  GetExport'
    { exportType = pExportType_,
      parameters = Lude.Nothing,
      restAPIId = pRestAPIId_,
      accepts = Lude.Nothing,
      stageName = pStageName_
    }

-- | [Required] The type of export. Acceptable values are 'oas30' for OpenAPI 3.0.x and 'swagger' for Swagger/OpenAPI 2.0.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geExportType :: Lens.Lens' GetExport Lude.Text
geExportType = Lens.lens (exportType :: GetExport -> Lude.Text) (\s a -> s {exportType = a} :: GetExport)
{-# DEPRECATED geExportType "Use generic-lens or generic-optics with 'exportType' instead." #-}

-- | A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @oas30@ and @swagger@ , any combination of the following parameters are supported: @extensions='integrations'@ or @extensions='apigateway'@ will export the API with x-amazon-apigateway-integration extensions. @extensions='authorizers'@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geParameters :: Lens.Lens' GetExport (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
geParameters = Lens.lens (parameters :: GetExport -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: GetExport)
{-# DEPRECATED geParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geRestAPIId :: Lens.Lens' GetExport Lude.Text
geRestAPIId = Lens.lens (restAPIId :: GetExport -> Lude.Text) (\s a -> s {restAPIId = a} :: GetExport)
{-# DEPRECATED geRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of@oas30@ and @swagger@ . This should be specified in the @Accept@ header for direct API requests.
--
-- /Note:/ Consider using 'accepts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geAccepts :: Lens.Lens' GetExport (Lude.Maybe Lude.Text)
geAccepts = Lens.lens (accepts :: GetExport -> Lude.Maybe Lude.Text) (\s a -> s {accepts = a} :: GetExport)
{-# DEPRECATED geAccepts "Use generic-lens or generic-optics with 'accepts' instead." #-}

-- | [Required] The name of the 'Stage' that will be exported.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geStageName :: Lens.Lens' GetExport Lude.Text
geStageName = Lens.lens (stageName :: GetExport -> Lude.Text) (\s a -> s {stageName = a} :: GetExport)
{-# DEPRECATED geStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest GetExport where
  type Rs GetExport = GetExportResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveBytes
      ( \s h x ->
          GetExportResponse'
            Lude.<$> (Lude.pure (Lude.Just x))
            Lude.<*> (h Lude..#? "Content-Disposition")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExport where
  toHeaders GetExport' {..} =
    Lude.mconcat
      [ "Accept" Lude.=# accepts,
        "Accept" Lude.=# ("application/json" :: Lude.ByteString)
      ]

instance Lude.ToPath GetExport where
  toPath GetExport' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName,
        "/exports/",
        Lude.toBS exportType
      ]

instance Lude.ToQuery GetExport where
  toQuery GetExport' {..} =
    Lude.mconcat
      [ "parameters"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> parameters)
      ]

-- | The binary blob response to 'GetExport' , which contains the generated SDK.
--
-- /See:/ 'mkGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The binary blob response to 'GetExport' , which contains the export.
    body :: Lude.Maybe Lude.ByteString,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Lude.Maybe Lude.Text,
    -- | The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportResponse' with the minimum fields required to make a request.
--
-- * 'body' - The binary blob response to 'GetExport' , which contains the export.
-- * 'contentDisposition' - The content-disposition header value in the HTTP response.
-- * 'contentType' - The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
-- * 'responseStatus' - The response status code.
mkGetExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetExportResponse
mkGetExportResponse pResponseStatus_ =
  GetExportResponse'
    { body = Lude.Nothing,
      contentDisposition = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The binary blob response to 'GetExport' , which contains the export.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersBody :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.ByteString)
gersBody = Lens.lens (body :: GetExportResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {body = a} :: GetExportResponse)
{-# DEPRECATED gersBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The content-disposition header value in the HTTP response.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersContentDisposition :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersContentDisposition = Lens.lens (contentDisposition :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: GetExportResponse)
{-# DEPRECATED gersContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersContentType :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersContentType = Lens.lens (contentType :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetExportResponse)
{-# DEPRECATED gersContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResponseStatus :: Lens.Lens' GetExportResponse Lude.Int
gersResponseStatus = Lens.lens (responseStatus :: GetExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportResponse)
{-# DEPRECATED gersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

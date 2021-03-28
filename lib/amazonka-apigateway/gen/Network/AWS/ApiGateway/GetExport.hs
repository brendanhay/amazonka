{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a deployed version of a 'RestApi' in a specified format.
module Network.AWS.ApiGateway.GetExport
    (
    -- * Creating a request
      GetExport (..)
    , mkGetExport
    -- ** Request lenses
    , geRestApiId
    , geStageName
    , geExportType
    , geAccepts
    , geParameters

    -- * Destructuring the response
    , GetExportResponse (..)
    , mkGetExportResponse
    -- ** Response lenses
    , gerrsBody
    , gerrsContentDisposition
    , gerrsContentType
    , gerrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request a new export of a 'RestApi' for a particular 'Stage' .
--
-- /See:/ 'mkGetExport' smart constructor.
data GetExport = GetExport'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the 'Stage' that will be exported.
  , exportType :: Core.Text
    -- ^ [Required] The type of export. Acceptable values are 'oas30' for OpenAPI 3.0.x and 'swagger' for Swagger/OpenAPI 2.0.
  , accepts :: Core.Maybe Core.Text
    -- ^ The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of@oas30@ and @swagger@ . This should be specified in the @Accept@ header for direct API requests.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @oas30@ and @swagger@ , any combination of the following parameters are supported: @extensions='integrations'@ or @extensions='apigateway'@ will export the API with x-amazon-apigateway-integration extensions. @extensions='authorizers'@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExport' value with any optional fields omitted.
mkGetExport
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> Core.Text -- ^ 'exportType'
    -> GetExport
mkGetExport restApiId stageName exportType
  = GetExport'{restApiId, stageName, exportType,
               accepts = Core.Nothing, parameters = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geRestApiId :: Lens.Lens' GetExport Core.Text
geRestApiId = Lens.field @"restApiId"
{-# INLINEABLE geRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the 'Stage' that will be exported.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geStageName :: Lens.Lens' GetExport Core.Text
geStageName = Lens.field @"stageName"
{-# INLINEABLE geStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | [Required] The type of export. Acceptable values are 'oas30' for OpenAPI 3.0.x and 'swagger' for Swagger/OpenAPI 2.0.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geExportType :: Lens.Lens' GetExport Core.Text
geExportType = Lens.field @"exportType"
{-# INLINEABLE geExportType #-}
{-# DEPRECATED exportType "Use generic-lens or generic-optics with 'exportType' instead"  #-}

-- | The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of@oas30@ and @swagger@ . This should be specified in the @Accept@ header for direct API requests.
--
-- /Note:/ Consider using 'accepts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geAccepts :: Lens.Lens' GetExport (Core.Maybe Core.Text)
geAccepts = Lens.field @"accepts"
{-# INLINEABLE geAccepts #-}
{-# DEPRECATED accepts "Use generic-lens or generic-optics with 'accepts' instead"  #-}

-- | A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @oas30@ and @swagger@ , any combination of the following parameters are supported: @extensions='integrations'@ or @extensions='apigateway'@ will export the API with x-amazon-apigateway-integration extensions. @extensions='authorizers'@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geParameters :: Lens.Lens' GetExport (Core.Maybe (Core.HashMap Core.Text Core.Text))
geParameters = Lens.field @"parameters"
{-# INLINEABLE geParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery GetExport where
        toQuery GetExport{..}
          = Core.toQueryPair "parameters"
              (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                 parameters)

instance Core.ToHeaders GetExport where
        toHeaders GetExport{..}
          = Core.toHeaders "Accept" accepts Core.<>
              Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetExport where
        type Rs GetExport = GetExportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName
                             Core.<> "/exports/"
                             Core.<> Core.toText exportType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 GetExportResponse' Core.<$>
                   (Core.pure x) Core.<*>
                     Core.parseHeaderMaybe "Content-Disposition" h
                     Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The binary blob response to 'GetExport' , which contains the generated SDK.
--
-- /See:/ 'mkGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { body :: Core.Maybe Core.ByteString
    -- ^ The binary blob response to 'GetExport' , which contains the export.
  , contentDisposition :: Core.Maybe Core.Text
    -- ^ The content-disposition header value in the HTTP response.
  , contentType :: Core.Maybe Core.Text
    -- ^ The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportResponse' value with any optional fields omitted.
mkGetExportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetExportResponse
mkGetExportResponse responseStatus
  = GetExportResponse'{body = Core.Nothing,
                       contentDisposition = Core.Nothing, contentType = Core.Nothing,
                       responseStatus}

-- | The binary blob response to 'GetExport' , which contains the export.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsBody :: Lens.Lens' GetExportResponse (Core.Maybe Core.ByteString)
gerrsBody = Lens.field @"body"
{-# INLINEABLE gerrsBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The content-disposition header value in the HTTP response.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsContentDisposition :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
gerrsContentDisposition = Lens.field @"contentDisposition"
{-# INLINEABLE gerrsContentDisposition #-}
{-# DEPRECATED contentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead"  #-}

-- | The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsContentType :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
gerrsContentType = Lens.field @"contentType"
{-# INLINEABLE gerrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetExportResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

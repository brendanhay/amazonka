{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Network.AWS.ApiGateway.PutIntegrationResponse
    (
    -- * Creating a request
      PutIntegrationResponse (..)
    , mkPutIntegrationResponse
    -- ** Request lenses
    , pirRestApiId
    , pirResourceId
    , pirHttpMethod
    , pirStatusCode
    , pirContentHandling
    , pirResponseParameters
    , pirResponseTemplates
    , pirSelectionPattern

     -- * Destructuring the response
    , Types.IntegrationResponse (..)
    , Types.mkIntegrationResponse
    -- ** Response lenses
    , Types.irContentHandling
    , Types.irResponseParameters
    , Types.irResponseTemplates
    , Types.irSelectionPattern
    , Types.irStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a put integration response request.
--
-- /See:/ 'mkPutIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a put integration response request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a put integration response request's HTTP method.
  , statusCode :: Types.StatusCode
    -- ^ [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
  , contentHandling :: Core.Maybe Types.ContentHandlingStrategy
    -- ^ Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
  , responseParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
  , responseTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Specifies a put integration response's templates.
  , selectionPattern :: Core.Maybe Core.Text
    -- ^ Specifies the selection pattern of a put integration response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutIntegrationResponse' value with any optional fields omitted.
mkPutIntegrationResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> PutIntegrationResponse
mkPutIntegrationResponse restApiId resourceId httpMethod statusCode
  = PutIntegrationResponse'{restApiId, resourceId, httpMethod,
                            statusCode, contentHandling = Core.Nothing,
                            responseParameters = Core.Nothing,
                            responseTemplates = Core.Nothing, selectionPattern = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRestApiId :: Lens.Lens' PutIntegrationResponse Core.Text
pirRestApiId = Lens.field @"restApiId"
{-# INLINEABLE pirRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a put integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirResourceId :: Lens.Lens' PutIntegrationResponse Core.Text
pirResourceId = Lens.field @"resourceId"
{-# INLINEABLE pirResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a put integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirHttpMethod :: Lens.Lens' PutIntegrationResponse Core.Text
pirHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE pirHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirStatusCode :: Lens.Lens' PutIntegrationResponse Types.StatusCode
pirStatusCode = Lens.field @"statusCode"
{-# INLINEABLE pirStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- /Note:/ Consider using 'contentHandling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirContentHandling :: Lens.Lens' PutIntegrationResponse (Core.Maybe Types.ContentHandlingStrategy)
pirContentHandling = Lens.field @"contentHandling"
{-# INLINEABLE pirContentHandling #-}
{-# DEPRECATED contentHandling "Use generic-lens or generic-optics with 'contentHandling' instead"  #-}

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirResponseParameters :: Lens.Lens' PutIntegrationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pirResponseParameters = Lens.field @"responseParameters"
{-# INLINEABLE pirResponseParameters #-}
{-# DEPRECATED responseParameters "Use generic-lens or generic-optics with 'responseParameters' instead"  #-}

-- | Specifies a put integration response's templates.
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirResponseTemplates :: Lens.Lens' PutIntegrationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pirResponseTemplates = Lens.field @"responseTemplates"
{-# INLINEABLE pirResponseTemplates #-}
{-# DEPRECATED responseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead"  #-}

-- | Specifies the selection pattern of a put integration response.
--
-- /Note:/ Consider using 'selectionPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirSelectionPattern :: Lens.Lens' PutIntegrationResponse (Core.Maybe Core.Text)
pirSelectionPattern = Lens.field @"selectionPattern"
{-# INLINEABLE pirSelectionPattern #-}
{-# DEPRECATED selectionPattern "Use generic-lens or generic-optics with 'selectionPattern' instead"  #-}

instance Core.ToQuery PutIntegrationResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutIntegrationResponse where
        toHeaders PutIntegrationResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON PutIntegrationResponse where
        toJSON PutIntegrationResponse{..}
          = Core.object
              (Core.catMaybes
                 [("contentHandling" Core..=) Core.<$> contentHandling,
                  ("responseParameters" Core..=) Core.<$> responseParameters,
                  ("responseTemplates" Core..=) Core.<$> responseTemplates,
                  ("selectionPattern" Core..=) Core.<$> selectionPattern])

instance Core.AWSRequest PutIntegrationResponse where
        type Rs PutIntegrationResponse = Types.IntegrationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

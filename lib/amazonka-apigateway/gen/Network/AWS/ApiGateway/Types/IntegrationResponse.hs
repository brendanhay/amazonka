{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.IntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.IntegrationResponse
  ( IntegrationResponse (..)
  -- * Smart constructor
  , mkIntegrationResponse
  -- * Lenses
  , irContentHandling
  , irResponseParameters
  , irResponseTemplates
  , irSelectionPattern
  , irStatusCode
  ) where

import qualified Network.AWS.ApiGateway.Types.ContentHandlingStrategy as Types
import qualified Network.AWS.ApiGateway.Types.StatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an integration response. The status code must map to an existing 'MethodResponse' , and parameters and templates can be used to transform the back-end response.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API> 
--
-- /See:/ 'mkIntegrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
  { contentHandling :: Core.Maybe Types.ContentHandlingStrategy
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
    -- ^ A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
  , responseTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
  , selectionPattern :: Core.Maybe Core.Text
    -- ^ Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
  , statusCode :: Core.Maybe Types.StatusCode
    -- ^ Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntegrationResponse' value with any optional fields omitted.
mkIntegrationResponse
    :: IntegrationResponse
mkIntegrationResponse
  = IntegrationResponse'{contentHandling = Core.Nothing,
                         responseParameters = Core.Nothing,
                         responseTemplates = Core.Nothing, selectionPattern = Core.Nothing,
                         statusCode = Core.Nothing}

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
irContentHandling :: Lens.Lens' IntegrationResponse (Core.Maybe Types.ContentHandlingStrategy)
irContentHandling = Lens.field @"contentHandling"
{-# INLINEABLE irContentHandling #-}
{-# DEPRECATED contentHandling "Use generic-lens or generic-optics with 'contentHandling' instead"  #-}

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irResponseParameters :: Lens.Lens' IntegrationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
irResponseParameters = Lens.field @"responseParameters"
{-# INLINEABLE irResponseParameters #-}
{-# DEPRECATED responseParameters "Use generic-lens or generic-optics with 'responseParameters' instead"  #-}

-- | Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irResponseTemplates :: Lens.Lens' IntegrationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
irResponseTemplates = Lens.field @"responseTemplates"
{-# INLINEABLE irResponseTemplates #-}
{-# DEPRECATED responseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead"  #-}

-- | Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
--
-- /Note:/ Consider using 'selectionPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irSelectionPattern :: Lens.Lens' IntegrationResponse (Core.Maybe Core.Text)
irSelectionPattern = Lens.field @"selectionPattern"
{-# INLINEABLE irSelectionPattern #-}
{-# DEPRECATED selectionPattern "Use generic-lens or generic-optics with 'selectionPattern' instead"  #-}

-- | Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatusCode :: Lens.Lens' IntegrationResponse (Core.Maybe Types.StatusCode)
irStatusCode = Lens.field @"statusCode"
{-# INLINEABLE irStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.FromJSON IntegrationResponse where
        parseJSON
          = Core.withObject "IntegrationResponse" Core.$
              \ x ->
                IntegrationResponse' Core.<$>
                  (x Core..:? "contentHandling") Core.<*>
                    x Core..:? "responseParameters"
                    Core.<*> x Core..:? "responseTemplates"
                    Core.<*> x Core..:? "selectionPattern"
                    Core.<*> x Core..:? "statusCode"

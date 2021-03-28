{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetGatewayResponses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'GatewayResponses' collection on the given 'RestApi' . If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default 'GatewayResponses' collection for the supported response types.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetGatewayResponses
    (
    -- * Creating a request
      GetGatewayResponses (..)
    , mkGetGatewayResponses
    -- ** Request lenses
    , ggrRestApiId
    , ggrLimit
    , ggrPosition

    -- * Destructuring the response
    , GetGatewayResponsesResponse (..)
    , mkGetGatewayResponsesResponse
    -- ** Response lenses
    , ggrrrsItems
    , ggrrrsPosition
    , ggrrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the 'GatewayResponses' collection on the given 'RestApi' . If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default 'GatewayResponses' collection for the supported response types.
--
-- /See:/ 'mkGetGatewayResponses' smart constructor.
data GetGatewayResponses = GetGatewayResponses'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500. The 'GatewayResponses' collection does not support pagination and the limit does not apply here.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set. The 'GatewayResponse' collection does not support pagination and the position does not apply here.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayResponses' value with any optional fields omitted.
mkGetGatewayResponses
    :: Core.Text -- ^ 'restApiId'
    -> GetGatewayResponses
mkGetGatewayResponses restApiId
  = GetGatewayResponses'{restApiId, limit = Core.Nothing,
                         position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrRestApiId :: Lens.Lens' GetGatewayResponses Core.Text
ggrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE ggrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500. The 'GatewayResponses' collection does not support pagination and the limit does not apply here.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrLimit :: Lens.Lens' GetGatewayResponses (Core.Maybe Core.Int)
ggrLimit = Lens.field @"limit"
{-# INLINEABLE ggrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set. The 'GatewayResponse' collection does not support pagination and the position does not apply here.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrPosition :: Lens.Lens' GetGatewayResponses (Core.Maybe Core.Text)
ggrPosition = Lens.field @"position"
{-# INLINEABLE ggrPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetGatewayResponses where
        toQuery GetGatewayResponses{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetGatewayResponses where
        toHeaders GetGatewayResponses{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetGatewayResponses where
        type Rs GetGatewayResponses = GetGatewayResponsesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/gatewayresponses",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGatewayResponsesResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetGatewayResponses where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | The collection of the 'GatewayResponse' instances of a 'RestApi' as a @responseType@ -to-'GatewayResponse' object map of key-value pairs. As such, pagination is not supported for querying this collection.
--
-- For more information about valid gateway response types, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html Gateway Response Types Supported by API Gateway> __Example: Get the collection of gateway responses of an API__ 
-- __Request__ 
-- This example request shows how to retrieve the 'GatewayResponses' collection from an API.
-- @@GET /restapis/o81lxisefl/gatewayresponses HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T220604Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=59b42fe54a76a5de8adf2c67baa6d39206f8e9ad49a1d77ccc6a5da3103a398a Cache-Control: no-cache Postman-Token: 5637af27-dc29-fc5c-9dfe-0645d52cb515 @ @ 
-- __Response__ 
-- The successful operation returns the @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "first": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "gatewayresponse:by-type": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "item": [ { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } ] }, "_embedded": { "item": [ { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_FAILURE", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "RESOURCE_NOT_FOUND", "statusCode": "404" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "REQUEST_TOO_LARGE", "statusCode": "413" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "THROTTLED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNSUPPORTED_MEDIA_TYPE", "statusCode": "415" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_5XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_4XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_PARAMETERS", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_BODY", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "EXPIRED_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "ACCESS_DENIED", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_API_KEY", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNAUTHORIZED", "statusCode": "401" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "API_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "QUOTA_EXCEEDED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_TIMEOUT", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_SIGNATURE", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_FAILURE", "statusCode": "500" } ] } }@ @ 
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html Customize Gateway Responses> 
--
-- /See:/ 'mkGetGatewayResponsesResponse' smart constructor.
data GetGatewayResponsesResponse = GetGatewayResponsesResponse'
  { items :: Core.Maybe [Types.GatewayResponse]
    -- ^ Returns the entire collection, because of no pagination support.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayResponsesResponse' value with any optional fields omitted.
mkGetGatewayResponsesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGatewayResponsesResponse
mkGetGatewayResponsesResponse responseStatus
  = GetGatewayResponsesResponse'{items = Core.Nothing,
                                 position = Core.Nothing, responseStatus}

-- | Returns the entire collection, because of no pagination support.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrrsItems :: Lens.Lens' GetGatewayResponsesResponse (Core.Maybe [Types.GatewayResponse])
ggrrrsItems = Lens.field @"items"
{-# INLINEABLE ggrrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrrsPosition :: Lens.Lens' GetGatewayResponsesResponse (Core.Maybe Core.Text)
ggrrrsPosition = Lens.field @"position"
{-# INLINEABLE ggrrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrrsResponseStatus :: Lens.Lens' GetGatewayResponsesResponse Core.Int
ggrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

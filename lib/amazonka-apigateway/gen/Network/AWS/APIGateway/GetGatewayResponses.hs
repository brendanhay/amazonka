{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetGatewayResponses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'GatewayResponses' collection on the given 'RestApi' . If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default 'GatewayResponses' collection for the supported response types.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetGatewayResponses
  ( -- * Creating a request
    GetGatewayResponses (..),
    mkGetGatewayResponses,

    -- ** Request lenses
    ggrLimit,
    ggrRestAPIId,
    ggrPosition,

    -- * Destructuring the response
    GetGatewayResponsesResponse (..),
    mkGetGatewayResponsesResponse,

    -- ** Response lenses
    ggrrsItems,
    ggrrsPosition,
    ggrrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the 'GatewayResponses' collection on the given 'RestApi' . If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default 'GatewayResponses' collection for the supported response types.
--
-- /See:/ 'mkGetGatewayResponses' smart constructor.
data GetGatewayResponses = GetGatewayResponses'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500. The 'GatewayResponses' collection does not support pagination and the limit does not apply here.
    limit :: Lude.Maybe Lude.Int,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The current pagination position in the paged result set. The 'GatewayResponse' collection does not support pagination and the position does not apply here.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayResponses' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500. The 'GatewayResponses' collection does not support pagination and the limit does not apply here.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'position' - The current pagination position in the paged result set. The 'GatewayResponse' collection does not support pagination and the position does not apply here.
mkGetGatewayResponses ::
  -- | 'restAPIId'
  Lude.Text ->
  GetGatewayResponses
mkGetGatewayResponses pRestAPIId_ =
  GetGatewayResponses'
    { limit = Lude.Nothing,
      restAPIId = pRestAPIId_,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500. The 'GatewayResponses' collection does not support pagination and the limit does not apply here.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrLimit :: Lens.Lens' GetGatewayResponses (Lude.Maybe Lude.Int)
ggrLimit = Lens.lens (limit :: GetGatewayResponses -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetGatewayResponses)
{-# DEPRECATED ggrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrRestAPIId :: Lens.Lens' GetGatewayResponses Lude.Text
ggrRestAPIId = Lens.lens (restAPIId :: GetGatewayResponses -> Lude.Text) (\s a -> s {restAPIId = a} :: GetGatewayResponses)
{-# DEPRECATED ggrRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The current pagination position in the paged result set. The 'GatewayResponse' collection does not support pagination and the position does not apply here.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrPosition :: Lens.Lens' GetGatewayResponses (Lude.Maybe Lude.Text)
ggrPosition = Lens.lens (position :: GetGatewayResponses -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetGatewayResponses)
{-# DEPRECATED ggrPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetGatewayResponses where
  page rq rs
    | Page.stop (rs Lens.^. ggrrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. ggrrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ggrPosition Lens..~ rs Lens.^. ggrrsPosition

instance Lude.AWSRequest GetGatewayResponses where
  type Rs GetGatewayResponses = GetGatewayResponsesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGatewayResponsesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGatewayResponses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetGatewayResponses where
  toPath GetGatewayResponses' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/gatewayresponses"]

instance Lude.ToQuery GetGatewayResponses where
  toQuery GetGatewayResponses' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

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
  { -- | Returns the entire collection, because of no pagination support.
    items :: Lude.Maybe [GatewayResponse],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayResponsesResponse' with the minimum fields required to make a request.
--
-- * 'items' - Returns the entire collection, because of no pagination support.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetGatewayResponsesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGatewayResponsesResponse
mkGetGatewayResponsesResponse pResponseStatus_ =
  GetGatewayResponsesResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the entire collection, because of no pagination support.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsItems :: Lens.Lens' GetGatewayResponsesResponse (Lude.Maybe [GatewayResponse])
ggrrsItems = Lens.lens (items :: GetGatewayResponsesResponse -> Lude.Maybe [GatewayResponse]) (\s a -> s {items = a} :: GetGatewayResponsesResponse)
{-# DEPRECATED ggrrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsPosition :: Lens.Lens' GetGatewayResponsesResponse (Lude.Maybe Lude.Text)
ggrrsPosition = Lens.lens (position :: GetGatewayResponsesResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetGatewayResponsesResponse)
{-# DEPRECATED ggrrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGatewayResponsesResponse Lude.Int
ggrrsResponseStatus = Lens.lens (responseStatus :: GetGatewayResponsesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGatewayResponsesResponse)
{-# DEPRECATED ggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

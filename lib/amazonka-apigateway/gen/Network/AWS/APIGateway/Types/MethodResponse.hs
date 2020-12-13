{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodResponse
  ( MethodResponse (..),

    -- * Smart constructor
    mkMethodResponse,

    -- * Lenses
    mResponseModels,
    mStatusCode,
    mResponseParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a method response of a given HTTP status code returned to the client. The method response is passed from the back end through the associated integration response that can be transformed using a mapping template.
--
--
-- __Example: A __MethodResponse__ instance of an API__
-- __Request__
-- The example request retrieves a __MethodResponse__ of the 200 status code.
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T222952Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns @200 OK@ status and a payload as follows:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" }@ @
-- 'Method' , 'IntegrationResponse' , 'Integration' <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'mkMethodResponse' smart constructor.
data MethodResponse = MethodResponse'
  { -- | Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
    responseModels :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The method response's status code.
    statusCode :: Lude.Maybe Lude.Text,
    -- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
    responseParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MethodResponse' with the minimum fields required to make a request.
--
-- * 'responseModels' - Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
-- * 'statusCode' - The method response's status code.
-- * 'responseParameters' - A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
mkMethodResponse ::
  MethodResponse
mkMethodResponse =
  MethodResponse'
    { responseModels = Lude.Nothing,
      statusCode = Lude.Nothing,
      responseParameters = Lude.Nothing
    }

-- | Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
--
-- /Note:/ Consider using 'responseModels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mResponseModels :: Lens.Lens' MethodResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mResponseModels = Lens.lens (responseModels :: MethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseModels = a} :: MethodResponse)
{-# DEPRECATED mResponseModels "Use generic-lens or generic-optics with 'responseModels' instead." #-}

-- | The method response's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mStatusCode :: Lens.Lens' MethodResponse (Lude.Maybe Lude.Text)
mStatusCode = Lens.lens (statusCode :: MethodResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: MethodResponse)
{-# DEPRECATED mStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mResponseParameters :: Lens.Lens' MethodResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool)))
mResponseParameters = Lens.lens (responseParameters :: MethodResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool))) (\s a -> s {responseParameters = a} :: MethodResponse)
{-# DEPRECATED mResponseParameters "Use generic-lens or generic-optics with 'responseParameters' instead." #-}

instance Lude.FromJSON MethodResponse where
  parseJSON =
    Lude.withObject
      "MethodResponse"
      ( \x ->
          MethodResponse'
            Lude.<$> (x Lude..:? "responseModels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statusCode")
            Lude.<*> (x Lude..:? "responseParameters" Lude..!= Lude.mempty)
      )

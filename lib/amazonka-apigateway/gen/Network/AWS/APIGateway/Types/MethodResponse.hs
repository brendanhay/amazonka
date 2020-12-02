{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a method response of a given HTTP status code returned to the client. The method response is passed from the back end through the associated integration response that can be transformed using a mapping template.
--
--
--
--
-- __Example: A __MethodResponse__ instance of an API__
-- __Request__
-- The example request retrieves a __MethodResponse__ of the 200 status code.
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T222952Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns @200 OK@ status and a payload as follows:
--
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" }@ @
--
-- 'Method' , 'IntegrationResponse' , 'Integration' <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'methodResponse' smart constructor.
data MethodResponse = MethodResponse'
  { _mResponseModels ::
      !(Maybe (Map Text (Text))),
    _mStatusCode :: !(Maybe Text),
    _mResponseParameters :: !(Maybe (Map Text (Bool)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mResponseModels' - Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
--
-- * 'mStatusCode' - The method response's status code.
--
-- * 'mResponseParameters' - A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
methodResponse ::
  MethodResponse
methodResponse =
  MethodResponse'
    { _mResponseModels = Nothing,
      _mStatusCode = Nothing,
      _mResponseParameters = Nothing
    }

-- | Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
mResponseModels :: Lens' MethodResponse (HashMap Text (Text))
mResponseModels = lens _mResponseModels (\s a -> s {_mResponseModels = a}) . _Default . _Map

-- | The method response's status code.
mStatusCode :: Lens' MethodResponse (Maybe Text)
mStatusCode = lens _mStatusCode (\s a -> s {_mStatusCode = a})

-- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
mResponseParameters :: Lens' MethodResponse (HashMap Text (Bool))
mResponseParameters = lens _mResponseParameters (\s a -> s {_mResponseParameters = a}) . _Default . _Map

instance FromJSON MethodResponse where
  parseJSON =
    withObject
      "MethodResponse"
      ( \x ->
          MethodResponse'
            <$> (x .:? "responseModels" .!= mempty)
            <*> (x .:? "statusCode")
            <*> (x .:? "responseParameters" .!= mempty)
      )

instance Hashable MethodResponse

instance NFData MethodResponse

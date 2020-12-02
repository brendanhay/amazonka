{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.IntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.IntegrationResponse where

import Network.AWS.APIGateway.Types.ContentHandlingStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an integration response. The status code must map to an existing 'MethodResponse' , and parameters and templates can be used to transform the back-end response.
--
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'integrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
  { _intContentHandling ::
      !(Maybe ContentHandlingStrategy),
    _intResponseTemplates :: !(Maybe (Map Text (Text))),
    _intSelectionPattern :: !(Maybe Text),
    _intStatusCode :: !(Maybe Text),
    _intResponseParameters ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'intContentHandling' - Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- * 'intResponseTemplates' - Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'intSelectionPattern' - Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
--
-- * 'intStatusCode' - Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- * 'intResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
integrationResponse ::
  IntegrationResponse
integrationResponse =
  IntegrationResponse'
    { _intContentHandling = Nothing,
      _intResponseTemplates = Nothing,
      _intSelectionPattern = Nothing,
      _intStatusCode = Nothing,
      _intResponseParameters = Nothing
    }

-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
intContentHandling :: Lens' IntegrationResponse (Maybe ContentHandlingStrategy)
intContentHandling = lens _intContentHandling (\s a -> s {_intContentHandling = a})

-- | Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
intResponseTemplates :: Lens' IntegrationResponse (HashMap Text (Text))
intResponseTemplates = lens _intResponseTemplates (\s a -> s {_intResponseTemplates = a}) . _Default . _Map

-- | Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
intSelectionPattern :: Lens' IntegrationResponse (Maybe Text)
intSelectionPattern = lens _intSelectionPattern (\s a -> s {_intSelectionPattern = a})

-- | Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
intStatusCode :: Lens' IntegrationResponse (Maybe Text)
intStatusCode = lens _intStatusCode (\s a -> s {_intStatusCode = a})

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
intResponseParameters :: Lens' IntegrationResponse (HashMap Text (Text))
intResponseParameters = lens _intResponseParameters (\s a -> s {_intResponseParameters = a}) . _Default . _Map

instance FromJSON IntegrationResponse where
  parseJSON =
    withObject
      "IntegrationResponse"
      ( \x ->
          IntegrationResponse'
            <$> (x .:? "contentHandling")
            <*> (x .:? "responseTemplates" .!= mempty)
            <*> (x .:? "selectionPattern")
            <*> (x .:? "statusCode")
            <*> (x .:? "responseParameters" .!= mempty)
      )

instance Hashable IntegrationResponse

instance NFData IntegrationResponse

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
--
--
module Network.AWS.APIGateway.PutIntegrationResponse
    (
    -- * Creating a Request
      putIntegrationResponse
    , PutIntegrationResponse
    -- * Request Lenses
    , piContentHandling
    , piResponseTemplates
    , piSelectionPattern
    , piResponseParameters
    , piRestAPIId
    , piResourceId
    , piHttpMethod
    , piStatusCode

    -- * Destructuring the Response
    , integrationResponse
    , IntegrationResponse
    -- * Response Lenses
    , intContentHandling
    , intResponseTemplates
    , intSelectionPattern
    , intStatusCode
    , intResponseParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a put integration response request.
--
--
--
-- /See:/ 'putIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { _piContentHandling    :: !(Maybe ContentHandlingStrategy)
  , _piResponseTemplates  :: !(Maybe (Map Text Text))
  , _piSelectionPattern   :: !(Maybe Text)
  , _piResponseParameters :: !(Maybe (Map Text Text))
  , _piRestAPIId          :: !Text
  , _piResourceId         :: !Text
  , _piHttpMethod         :: !Text
  , _piStatusCode         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piContentHandling' - Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- * 'piResponseTemplates' - Specifies a put integration response's templates.
--
-- * 'piSelectionPattern' - Specifies the selection pattern of a put integration response.
--
-- * 'piResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
--
-- * 'piRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'piResourceId' - [Required] Specifies a put integration response request's resource identifier.
--
-- * 'piHttpMethod' - [Required] Specifies a put integration response request's HTTP method.
--
-- * 'piStatusCode' - [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
putIntegrationResponse
    :: Text -- ^ 'piRestAPIId'
    -> Text -- ^ 'piResourceId'
    -> Text -- ^ 'piHttpMethod'
    -> Text -- ^ 'piStatusCode'
    -> PutIntegrationResponse
putIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
  PutIntegrationResponse'
    { _piContentHandling = Nothing
    , _piResponseTemplates = Nothing
    , _piSelectionPattern = Nothing
    , _piResponseParameters = Nothing
    , _piRestAPIId = pRestAPIId_
    , _piResourceId = pResourceId_
    , _piHttpMethod = pHttpMethod_
    , _piStatusCode = pStatusCode_
    }


-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
piContentHandling :: Lens' PutIntegrationResponse (Maybe ContentHandlingStrategy)
piContentHandling = lens _piContentHandling (\ s a -> s{_piContentHandling = a})

-- | Specifies a put integration response's templates.
piResponseTemplates :: Lens' PutIntegrationResponse (HashMap Text Text)
piResponseTemplates = lens _piResponseTemplates (\ s a -> s{_piResponseTemplates = a}) . _Default . _Map

-- | Specifies the selection pattern of a put integration response.
piSelectionPattern :: Lens' PutIntegrationResponse (Maybe Text)
piSelectionPattern = lens _piSelectionPattern (\ s a -> s{_piSelectionPattern = a})

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ must be a valid and unique response header name and @JSON-expression@ a valid JSON expression without the @> @ prefix.
piResponseParameters :: Lens' PutIntegrationResponse (HashMap Text Text)
piResponseParameters = lens _piResponseParameters (\ s a -> s{_piResponseParameters = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
piRestAPIId :: Lens' PutIntegrationResponse Text
piRestAPIId = lens _piRestAPIId (\ s a -> s{_piRestAPIId = a})

-- | [Required] Specifies a put integration response request's resource identifier.
piResourceId :: Lens' PutIntegrationResponse Text
piResourceId = lens _piResourceId (\ s a -> s{_piResourceId = a})

-- | [Required] Specifies a put integration response request's HTTP method.
piHttpMethod :: Lens' PutIntegrationResponse Text
piHttpMethod = lens _piHttpMethod (\ s a -> s{_piHttpMethod = a})

-- | [Required] Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
piStatusCode :: Lens' PutIntegrationResponse Text
piStatusCode = lens _piStatusCode (\ s a -> s{_piStatusCode = a})

instance AWSRequest PutIntegrationResponse where
        type Rs PutIntegrationResponse = IntegrationResponse
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutIntegrationResponse where

instance NFData PutIntegrationResponse where

instance ToHeaders PutIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutIntegrationResponse where
        toJSON PutIntegrationResponse'{..}
          = object
              (catMaybes
                 [("contentHandling" .=) <$> _piContentHandling,
                  ("responseTemplates" .=) <$> _piResponseTemplates,
                  ("selectionPattern" .=) <$> _piSelectionPattern,
                  ("responseParameters" .=) <$> _piResponseParameters])

instance ToPath PutIntegrationResponse where
        toPath PutIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _piRestAPIId, "/resources/",
               toBS _piResourceId, "/methods/", toBS _piHttpMethod,
               "/integration/responses/", toBS _piStatusCode]

instance ToQuery PutIntegrationResponse where
        toQuery = const mempty

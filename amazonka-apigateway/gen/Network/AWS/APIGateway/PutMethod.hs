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
-- Module      : Network.AWS.APIGateway.PutMethod
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a method to an existing 'Resource' resource.
--
--
module Network.AWS.APIGateway.PutMethod
    (
    -- * Creating a Request
      putMethod
    , PutMethod
    -- * Request Lenses
    , putRequestValidatorId
    , putRequestModels
    , putRequestParameters
    , putAuthorizerId
    , putOperationName
    , putApiKeyRequired
    , putRestAPIId
    , putResourceId
    , putHttpMethod
    , putAuthorizationType

    -- * Destructuring the Response
    , method
    , Method
    -- * Response Lenses
    , mMethodResponses
    , mHttpMethod
    , mRequestValidatorId
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mOperationName
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to add a method to an existing 'Resource' resource.
--
--
--
-- /See:/ 'putMethod' smart constructor.
data PutMethod = PutMethod'
    { _putRequestValidatorId :: !(Maybe Text)
    , _putRequestModels      :: !(Maybe (Map Text Text))
    , _putRequestParameters  :: !(Maybe (Map Text Bool))
    , _putAuthorizerId       :: !(Maybe Text)
    , _putOperationName      :: !(Maybe Text)
    , _putApiKeyRequired     :: !(Maybe Bool)
    , _putRestAPIId          :: !Text
    , _putResourceId         :: !Text
    , _putHttpMethod         :: !Text
    , _putAuthorizationType  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'putRequestValidatorId' - The identifier of a 'RequestValidator' for validating the method request.
--
-- * 'putRequestModels' - Specifies the 'Model' resources used for the request's content type. Request models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
--
-- * 'putRequestParameters' - A key-value map defining required or optional method request parameters that can be accepted by Amazon API Gateway. A key defines a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or body-mapping templates.
--
-- * 'putAuthorizerId' - Specifies the identifier of an 'Authorizer' to use on this Method, if the type is CUSTOM.
--
-- * 'putOperationName' - A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in <http://petstore-demo-endpoint.execute-api.com/petstore/pets PetStore> example.
--
-- * 'putApiKeyRequired' - Specifies whether the method required a valid 'ApiKey' .
--
-- * 'putRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'putResourceId' - The 'Resource' identifier for the new 'Method' resource.
--
-- * 'putHttpMethod' - Specifies the method request's HTTP method type.
--
-- * 'putAuthorizationType' - The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
putMethod
    :: Text -- ^ 'putRestAPIId'
    -> Text -- ^ 'putResourceId'
    -> Text -- ^ 'putHttpMethod'
    -> Text -- ^ 'putAuthorizationType'
    -> PutMethod
putMethod pRestAPIId_ pResourceId_ pHttpMethod_ pAuthorizationType_ =
    PutMethod'
    { _putRequestValidatorId = Nothing
    , _putRequestModels = Nothing
    , _putRequestParameters = Nothing
    , _putAuthorizerId = Nothing
    , _putOperationName = Nothing
    , _putApiKeyRequired = Nothing
    , _putRestAPIId = pRestAPIId_
    , _putResourceId = pResourceId_
    , _putHttpMethod = pHttpMethod_
    , _putAuthorizationType = pAuthorizationType_
    }

-- | The identifier of a 'RequestValidator' for validating the method request.
putRequestValidatorId :: Lens' PutMethod (Maybe Text)
putRequestValidatorId = lens _putRequestValidatorId (\ s a -> s{_putRequestValidatorId = a});

-- | Specifies the 'Model' resources used for the request's content type. Request models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
putRequestModels :: Lens' PutMethod (HashMap Text Text)
putRequestModels = lens _putRequestModels (\ s a -> s{_putRequestModels = a}) . _Default . _Map;

-- | A key-value map defining required or optional method request parameters that can be accepted by Amazon API Gateway. A key defines a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or body-mapping templates.
putRequestParameters :: Lens' PutMethod (HashMap Text Bool)
putRequestParameters = lens _putRequestParameters (\ s a -> s{_putRequestParameters = a}) . _Default . _Map;

-- | Specifies the identifier of an 'Authorizer' to use on this Method, if the type is CUSTOM.
putAuthorizerId :: Lens' PutMethod (Maybe Text)
putAuthorizerId = lens _putAuthorizerId (\ s a -> s{_putAuthorizerId = a});

-- | A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in <http://petstore-demo-endpoint.execute-api.com/petstore/pets PetStore> example.
putOperationName :: Lens' PutMethod (Maybe Text)
putOperationName = lens _putOperationName (\ s a -> s{_putOperationName = a});

-- | Specifies whether the method required a valid 'ApiKey' .
putApiKeyRequired :: Lens' PutMethod (Maybe Bool)
putApiKeyRequired = lens _putApiKeyRequired (\ s a -> s{_putApiKeyRequired = a});

-- | The string identifier of the associated 'RestApi' .
putRestAPIId :: Lens' PutMethod Text
putRestAPIId = lens _putRestAPIId (\ s a -> s{_putRestAPIId = a});

-- | The 'Resource' identifier for the new 'Method' resource.
putResourceId :: Lens' PutMethod Text
putResourceId = lens _putResourceId (\ s a -> s{_putResourceId = a});

-- | Specifies the method request's HTTP method type.
putHttpMethod :: Lens' PutMethod Text
putHttpMethod = lens _putHttpMethod (\ s a -> s{_putHttpMethod = a});

-- | The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
putAuthorizationType :: Lens' PutMethod Text
putAuthorizationType = lens _putAuthorizationType (\ s a -> s{_putAuthorizationType = a});

instance AWSRequest PutMethod where
        type Rs PutMethod = Method
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutMethod

instance NFData PutMethod

instance ToHeaders PutMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutMethod where
        toJSON PutMethod'{..}
          = object
              (catMaybes
                 [("requestValidatorId" .=) <$>
                    _putRequestValidatorId,
                  ("requestModels" .=) <$> _putRequestModels,
                  ("requestParameters" .=) <$> _putRequestParameters,
                  ("authorizerId" .=) <$> _putAuthorizerId,
                  ("operationName" .=) <$> _putOperationName,
                  ("apiKeyRequired" .=) <$> _putApiKeyRequired,
                  Just ("authorizationType" .= _putAuthorizationType)])

instance ToPath PutMethod where
        toPath PutMethod'{..}
          = mconcat
              ["/restapis/", toBS _putRestAPIId, "/resources/",
               toBS _putResourceId, "/methods/",
               toBS _putHttpMethod]

instance ToQuery PutMethod where
        toQuery = const mempty

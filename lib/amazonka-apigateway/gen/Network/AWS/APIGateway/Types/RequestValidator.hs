{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.RequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.RequestValidator where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of validation rules for incoming 'Method' requests.
--
--
-- In OpenAPI, a 'RequestValidator' of an API is defined by the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html x-amazon-apigateway-request-validators.requestValidator> object. It the referenced using the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator x-amazon-apigateway-request-validator> property.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'requestValidator' smart constructor.
data RequestValidator = RequestValidator'
  { _rvValidateRequestParameters ::
      !(Maybe Bool),
    _rvName :: !(Maybe Text),
    _rvValidateRequestBody :: !(Maybe Bool),
    _rvId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestValidator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvValidateRequestParameters' - A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
--
-- * 'rvName' - The name of this 'RequestValidator'
--
-- * 'rvValidateRequestBody' - A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
--
-- * 'rvId' - The identifier of this 'RequestValidator' .
requestValidator ::
  RequestValidator
requestValidator =
  RequestValidator'
    { _rvValidateRequestParameters = Nothing,
      _rvName = Nothing,
      _rvValidateRequestBody = Nothing,
      _rvId = Nothing
    }

-- | A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
rvValidateRequestParameters :: Lens' RequestValidator (Maybe Bool)
rvValidateRequestParameters = lens _rvValidateRequestParameters (\s a -> s {_rvValidateRequestParameters = a})

-- | The name of this 'RequestValidator'
rvName :: Lens' RequestValidator (Maybe Text)
rvName = lens _rvName (\s a -> s {_rvName = a})

-- | A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
rvValidateRequestBody :: Lens' RequestValidator (Maybe Bool)
rvValidateRequestBody = lens _rvValidateRequestBody (\s a -> s {_rvValidateRequestBody = a})

-- | The identifier of this 'RequestValidator' .
rvId :: Lens' RequestValidator (Maybe Text)
rvId = lens _rvId (\s a -> s {_rvId = a})

instance FromJSON RequestValidator where
  parseJSON =
    withObject
      "RequestValidator"
      ( \x ->
          RequestValidator'
            <$> (x .:? "validateRequestParameters")
            <*> (x .:? "name")
            <*> (x .:? "validateRequestBody")
            <*> (x .:? "id")
      )

instance Hashable RequestValidator

instance NFData RequestValidator

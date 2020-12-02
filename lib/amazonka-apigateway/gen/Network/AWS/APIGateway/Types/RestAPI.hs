{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.RestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.RestAPI where

import Network.AWS.APIGateway.Types.APIKeySourceType
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a REST API.
--
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'restAPI' smart constructor.
data RestAPI = RestAPI'
  { _raMinimumCompressionSize :: !(Maybe Int),
    _raDisableExecuteAPIEndpoint :: !(Maybe Bool),
    _raBinaryMediaTypes :: !(Maybe [Text]),
    _raWarnings :: !(Maybe [Text]),
    _raCreatedDate :: !(Maybe POSIX),
    _raName :: !(Maybe Text),
    _raVersion :: !(Maybe Text),
    _raApiKeySource :: !(Maybe APIKeySourceType),
    _raId :: !(Maybe Text),
    _raPolicy :: !(Maybe Text),
    _raEndpointConfiguration :: !(Maybe EndpointConfiguration),
    _raDescription :: !(Maybe Text),
    _raTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raMinimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- * 'raDisableExecuteAPIEndpoint' - Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
--
-- * 'raBinaryMediaTypes' - The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- * 'raWarnings' - The warning messages reported when @failonwarnings@ is turned on during API import.
--
-- * 'raCreatedDate' - The timestamp when the API was created.
--
-- * 'raName' - The API's name.
--
-- * 'raVersion' - A version identifier for the API.
--
-- * 'raApiKeySource' - The source of the API key for metering requests according to a usage plan. Valid values are:     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
-- * 'raId' - The API's identifier. This identifier is unique across all of your APIs in API Gateway.
--
-- * 'raPolicy' - A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
--
-- * 'raEndpointConfiguration' - The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- * 'raDescription' - The API's description.
--
-- * 'raTags' - The collection of tags. Each tag element is associated with a given resource.
restAPI ::
  RestAPI
restAPI =
  RestAPI'
    { _raMinimumCompressionSize = Nothing,
      _raDisableExecuteAPIEndpoint = Nothing,
      _raBinaryMediaTypes = Nothing,
      _raWarnings = Nothing,
      _raCreatedDate = Nothing,
      _raName = Nothing,
      _raVersion = Nothing,
      _raApiKeySource = Nothing,
      _raId = Nothing,
      _raPolicy = Nothing,
      _raEndpointConfiguration = Nothing,
      _raDescription = Nothing,
      _raTags = Nothing
    }

-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
raMinimumCompressionSize :: Lens' RestAPI (Maybe Int)
raMinimumCompressionSize = lens _raMinimumCompressionSize (\s a -> s {_raMinimumCompressionSize = a})

-- | Specifies whether clients can invoke your API by using the default @execute-api@ endpoint. By default, clients can invoke your API with the default https://{api_id}.execute-api.{region}.amazonaws.com endpoint. To require that clients use a custom domain name to invoke your API, disable the default endpoint.
raDisableExecuteAPIEndpoint :: Lens' RestAPI (Maybe Bool)
raDisableExecuteAPIEndpoint = lens _raDisableExecuteAPIEndpoint (\s a -> s {_raDisableExecuteAPIEndpoint = a})

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
raBinaryMediaTypes :: Lens' RestAPI [Text]
raBinaryMediaTypes = lens _raBinaryMediaTypes (\s a -> s {_raBinaryMediaTypes = a}) . _Default . _Coerce

-- | The warning messages reported when @failonwarnings@ is turned on during API import.
raWarnings :: Lens' RestAPI [Text]
raWarnings = lens _raWarnings (\s a -> s {_raWarnings = a}) . _Default . _Coerce

-- | The timestamp when the API was created.
raCreatedDate :: Lens' RestAPI (Maybe UTCTime)
raCreatedDate = lens _raCreatedDate (\s a -> s {_raCreatedDate = a}) . mapping _Time

-- | The API's name.
raName :: Lens' RestAPI (Maybe Text)
raName = lens _raName (\s a -> s {_raName = a})

-- | A version identifier for the API.
raVersion :: Lens' RestAPI (Maybe Text)
raVersion = lens _raVersion (\s a -> s {_raVersion = a})

-- | The source of the API key for metering requests according to a usage plan. Valid values are:     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
raApiKeySource :: Lens' RestAPI (Maybe APIKeySourceType)
raApiKeySource = lens _raApiKeySource (\s a -> s {_raApiKeySource = a})

-- | The API's identifier. This identifier is unique across all of your APIs in API Gateway.
raId :: Lens' RestAPI (Maybe Text)
raId = lens _raId (\s a -> s {_raId = a})

-- | A stringified JSON policy document that applies to this RestApi regardless of the caller and 'Method' configuration.
raPolicy :: Lens' RestAPI (Maybe Text)
raPolicy = lens _raPolicy (\s a -> s {_raPolicy = a})

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
raEndpointConfiguration :: Lens' RestAPI (Maybe EndpointConfiguration)
raEndpointConfiguration = lens _raEndpointConfiguration (\s a -> s {_raEndpointConfiguration = a})

-- | The API's description.
raDescription :: Lens' RestAPI (Maybe Text)
raDescription = lens _raDescription (\s a -> s {_raDescription = a})

-- | The collection of tags. Each tag element is associated with a given resource.
raTags :: Lens' RestAPI (HashMap Text (Text))
raTags = lens _raTags (\s a -> s {_raTags = a}) . _Default . _Map

instance FromJSON RestAPI where
  parseJSON =
    withObject
      "RestAPI"
      ( \x ->
          RestAPI'
            <$> (x .:? "minimumCompressionSize")
            <*> (x .:? "disableExecuteApiEndpoint")
            <*> (x .:? "binaryMediaTypes" .!= mempty)
            <*> (x .:? "warnings" .!= mempty)
            <*> (x .:? "createdDate")
            <*> (x .:? "name")
            <*> (x .:? "version")
            <*> (x .:? "apiKeySource")
            <*> (x .:? "id")
            <*> (x .:? "policy")
            <*> (x .:? "endpointConfiguration")
            <*> (x .:? "description")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable RestAPI

instance NFData RestAPI

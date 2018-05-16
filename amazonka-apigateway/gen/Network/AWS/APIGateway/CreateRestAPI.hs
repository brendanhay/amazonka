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
-- Module      : Network.AWS.APIGateway.CreateRestAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'RestApi' resource.
--
--
module Network.AWS.APIGateway.CreateRestAPI
    (
    -- * Creating a Request
      createRestAPI
    , CreateRestAPI
    -- * Request Lenses
    , craMinimumCompressionSize
    , craBinaryMediaTypes
    , craVersion
    , craApiKeySource
    , craCloneFrom
    , craPolicy
    , craEndpointConfiguration
    , craDescription
    , craName

    -- * Destructuring the Response
    , restAPI
    , RestAPI
    -- * Response Lenses
    , raMinimumCompressionSize
    , raBinaryMediaTypes
    , raWarnings
    , raCreatedDate
    , raName
    , raVersion
    , raApiKeySource
    , raId
    , raPolicy
    , raEndpointConfiguration
    , raDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The POST Request to add a new 'RestApi' resource to your collection.
--
--
--
-- /See:/ 'createRestAPI' smart constructor.
data CreateRestAPI = CreateRestAPI'
  { _craMinimumCompressionSize :: !(Maybe Int)
  , _craBinaryMediaTypes       :: !(Maybe [Text])
  , _craVersion                :: !(Maybe Text)
  , _craApiKeySource           :: !(Maybe APIKeySourceType)
  , _craCloneFrom              :: !(Maybe Text)
  , _craPolicy                 :: !(Maybe Text)
  , _craEndpointConfiguration  :: !(Maybe EndpointConfiguration)
  , _craDescription            :: !(Maybe Text)
  , _craName                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craMinimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
--
-- * 'craBinaryMediaTypes' - The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- * 'craVersion' - A version identifier for the API.
--
-- * 'craApiKeySource' - The source of the API key for metering requests according to a usage plan. Valid values are:     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
--
-- * 'craCloneFrom' - The ID of the 'RestApi' that you want to clone from.
--
-- * 'craPolicy' - 'Method'
--
-- * 'craEndpointConfiguration' - The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
--
-- * 'craDescription' - The description of the 'RestApi' .
--
-- * 'craName' - [Required] The name of the 'RestApi' .
createRestAPI
    :: Text -- ^ 'craName'
    -> CreateRestAPI
createRestAPI pName_ =
  CreateRestAPI'
    { _craMinimumCompressionSize = Nothing
    , _craBinaryMediaTypes = Nothing
    , _craVersion = Nothing
    , _craApiKeySource = Nothing
    , _craCloneFrom = Nothing
    , _craPolicy = Nothing
    , _craEndpointConfiguration = Nothing
    , _craDescription = Nothing
    , _craName = pName_
    }


-- | A nullable integer that is used to enable compression (with non-negative between 0 and 10485760 (10M) bytes, inclusive) or disable compression (with a null value) on an API. When compression is enabled, compression or decompression is not applied on the payload if the payload size is smaller than this value. Setting it to zero allows compression for any payload size.
craMinimumCompressionSize :: Lens' CreateRestAPI (Maybe Int)
craMinimumCompressionSize = lens _craMinimumCompressionSize (\ s a -> s{_craMinimumCompressionSize = a})

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
craBinaryMediaTypes :: Lens' CreateRestAPI [Text]
craBinaryMediaTypes = lens _craBinaryMediaTypes (\ s a -> s{_craBinaryMediaTypes = a}) . _Default . _Coerce

-- | A version identifier for the API.
craVersion :: Lens' CreateRestAPI (Maybe Text)
craVersion = lens _craVersion (\ s a -> s{_craVersion = a})

-- | The source of the API key for metering requests according to a usage plan. Valid values are:     * @HEADER@ to read the API key from the @X-API-Key@ header of a request.     * @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from a custom authorizer.
craApiKeySource :: Lens' CreateRestAPI (Maybe APIKeySourceType)
craApiKeySource = lens _craApiKeySource (\ s a -> s{_craApiKeySource = a})

-- | The ID of the 'RestApi' that you want to clone from.
craCloneFrom :: Lens' CreateRestAPI (Maybe Text)
craCloneFrom = lens _craCloneFrom (\ s a -> s{_craCloneFrom = a})

-- | 'Method'
craPolicy :: Lens' CreateRestAPI (Maybe Text)
craPolicy = lens _craPolicy (\ s a -> s{_craPolicy = a})

-- | The endpoint configuration of this 'RestApi' showing the endpoint types of the API.
craEndpointConfiguration :: Lens' CreateRestAPI (Maybe EndpointConfiguration)
craEndpointConfiguration = lens _craEndpointConfiguration (\ s a -> s{_craEndpointConfiguration = a})

-- | The description of the 'RestApi' .
craDescription :: Lens' CreateRestAPI (Maybe Text)
craDescription = lens _craDescription (\ s a -> s{_craDescription = a})

-- | [Required] The name of the 'RestApi' .
craName :: Lens' CreateRestAPI Text
craName = lens _craName (\ s a -> s{_craName = a})

instance AWSRequest CreateRestAPI where
        type Rs CreateRestAPI = RestAPI
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateRestAPI where

instance NFData CreateRestAPI where

instance ToHeaders CreateRestAPI where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateRestAPI where
        toJSON CreateRestAPI'{..}
          = object
              (catMaybes
                 [("minimumCompressionSize" .=) <$>
                    _craMinimumCompressionSize,
                  ("binaryMediaTypes" .=) <$> _craBinaryMediaTypes,
                  ("version" .=) <$> _craVersion,
                  ("apiKeySource" .=) <$> _craApiKeySource,
                  ("cloneFrom" .=) <$> _craCloneFrom,
                  ("policy" .=) <$> _craPolicy,
                  ("endpointConfiguration" .=) <$>
                    _craEndpointConfiguration,
                  ("description" .=) <$> _craDescription,
                  Just ("name" .= _craName)])

instance ToPath CreateRestAPI where
        toPath = const "/restapis"

instance ToQuery CreateRestAPI where
        toQuery = const mempty

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
-- Module      : Network.AWS.APIGateway.ImportRestAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for creating a new API from an external API definition file.
--
--
module Network.AWS.APIGateway.ImportRestAPI
    (
    -- * Creating a Request
      importRestAPI
    , ImportRestAPI
    -- * Request Lenses
    , iraFailOnWarnings
    , iraParameters
    , iraBody

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

-- | A POST request to import an API to API Gateway using an input of an API definition file.
--
--
--
-- /See:/ 'importRestAPI' smart constructor.
data ImportRestAPI = ImportRestAPI'
  { _iraFailOnWarnings :: !(Maybe Bool)
  , _iraParameters     :: !(Maybe (Map Text Text))
  , _iraBody           :: !ByteString
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iraFailOnWarnings' - A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- * 'iraParameters' - A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values. To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ . To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ or@endpointConfigurationTypes=REGIONAL@ . The default endpoint type is @EDGE@ . To handle imported @basePath@ , set @parameters@ as @basePath=ignore@ , @basePath=prepend@ or @basePath=split@ . For example, the AWS CLI command to exclude documentation from the imported API is: @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ @ The AWS CLI command to set the regional endpoint on the imported API is: @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json@ @
--
-- * 'iraBody' - [Required] The POST request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
importRestAPI
    :: ByteString -- ^ 'iraBody'
    -> ImportRestAPI
importRestAPI pBody_ =
  ImportRestAPI'
    {_iraFailOnWarnings = Nothing, _iraParameters = Nothing, _iraBody = pBody_}


-- | A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
iraFailOnWarnings :: Lens' ImportRestAPI (Maybe Bool)
iraFailOnWarnings = lens _iraFailOnWarnings (\ s a -> s{_iraFailOnWarnings = a})

-- | A key-value map of context-specific query string parameters specifying the behavior of different API importing operations. The following shows operation-specific parameters and their supported values. To exclude 'DocumentationParts' from the import, set @parameters@ as @ignore=documentation@ . To configure the endpoint type, set @parameters@ as @endpointConfigurationTypes=EDGE@ or@endpointConfigurationTypes=REGIONAL@ . The default endpoint type is @EDGE@ . To handle imported @basePath@ , set @parameters@ as @basePath=ignore@ , @basePath=prepend@ or @basePath=split@ . For example, the AWS CLI command to exclude documentation from the imported API is: @@aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ @ The AWS CLI command to set the regional endpoint on the imported API is: @@aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json@ @
iraParameters :: Lens' ImportRestAPI (HashMap Text Text)
iraParameters = lens _iraParameters (\ s a -> s{_iraParameters = a}) . _Default . _Map

-- | [Required] The POST request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
iraBody :: Lens' ImportRestAPI ByteString
iraBody = lens _iraBody (\ s a -> s{_iraBody = a})

instance AWSRequest ImportRestAPI where
        type Rs ImportRestAPI = RestAPI
        request = postBody apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable ImportRestAPI where

instance NFData ImportRestAPI where

instance ToBody ImportRestAPI where
        toBody = toBody . _iraBody

instance ToHeaders ImportRestAPI where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath ImportRestAPI where
        toPath = const "/restapis"

instance ToQuery ImportRestAPI where
        toQuery ImportRestAPI'{..}
          = mconcat
              ["failonwarnings" =: _iraFailOnWarnings,
               "parameters" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _iraParameters),
               "mode=import"]

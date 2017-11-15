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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the Amazon API Gateway control service for creating a new API from an external API definition file.
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
    , raBinaryMediaTypes
    , raWarnings
    , raCreatedDate
    , raName
    , raVersion
    , raId
    , raEndpointConfiguration
    , raDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A POST request to import an API to Amazon API Gateway using an input of an API definition file.
--
--
--
-- /See:/ 'importRestAPI' smart constructor.
data ImportRestAPI = ImportRestAPI'
  { _iraFailOnWarnings :: !(Maybe Bool)
  , _iraParameters     :: !(Maybe (Map Text Text))
  , _iraBody           :: !(HashMap Text Value)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iraFailOnWarnings' - A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- * 'iraParameters' - Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ .
--
-- * 'iraBody' - The POST request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
importRestAPI
    :: HashMap Text Value -- ^ 'iraBody'
    -> ImportRestAPI
importRestAPI pBody_ =
  ImportRestAPI'
  {_iraFailOnWarnings = Nothing, _iraParameters = Nothing, _iraBody = pBody_}


-- | A query parameter to indicate whether to rollback the API creation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
iraFailOnWarnings :: Lens' ImportRestAPI (Maybe Bool)
iraFailOnWarnings = lens _iraFailOnWarnings (\ s a -> s{_iraFailOnWarnings = a});

-- | Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ .
iraParameters :: Lens' ImportRestAPI (HashMap Text Text)
iraParameters = lens _iraParameters (\ s a -> s{_iraParameters = a}) . _Default . _Map;

-- | The POST request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
iraBody :: Lens' ImportRestAPI (HashMap Text Value)
iraBody = lens _iraBody (\ s a -> s{_iraBody = a});

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

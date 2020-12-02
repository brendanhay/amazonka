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
-- Module      : Network.AWS.APIGateway.PutRestAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for updating an existing API with an input of external API definitions. The update can take the form of merging the supplied definition into the existing API or overwriting the existing API.
--
--
module Network.AWS.APIGateway.PutRestAPI
    (
    -- * Creating a Request
      putRestAPI
    , PutRestAPI
    -- * Request Lenses
    , praMode
    , praFailOnWarnings
    , praParameters
    , praRestAPIId
    , praBody

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

-- | A PUT request to update an existing API, with external API definitions specified as the request body.
--
--
--
-- /See:/ 'putRestAPI' smart constructor.
data PutRestAPI = PutRestAPI'
  { _praMode           :: !(Maybe PutMode)
  , _praFailOnWarnings :: !(Maybe Bool)
  , _praParameters     :: !(Maybe (Map Text Text))
  , _praRestAPIId      :: !Text
  , _praBody           :: !ByteString
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'praMode' - The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
--
-- * 'praFailOnWarnings' - A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- * 'praParameters' - Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ .
--
-- * 'praRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'praBody' - [Required] The PUT request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
putRestAPI
    :: Text -- ^ 'praRestAPIId'
    -> ByteString -- ^ 'praBody'
    -> PutRestAPI
putRestAPI pRestAPIId_ pBody_ =
  PutRestAPI'
    { _praMode = Nothing
    , _praFailOnWarnings = Nothing
    , _praParameters = Nothing
    , _praRestAPIId = pRestAPIId_
    , _praBody = pBody_
    }


-- | The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
praMode :: Lens' PutRestAPI (Maybe PutMode)
praMode = lens _praMode (\ s a -> s{_praMode = a})

-- | A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
praFailOnWarnings :: Lens' PutRestAPI (Maybe Bool)
praFailOnWarnings = lens _praFailOnWarnings (\ s a -> s{_praFailOnWarnings = a})

-- | Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json@ .
praParameters :: Lens' PutRestAPI (HashMap Text Text)
praParameters = lens _praParameters (\ s a -> s{_praParameters = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
praRestAPIId :: Lens' PutRestAPI Text
praRestAPIId = lens _praRestAPIId (\ s a -> s{_praRestAPIId = a})

-- | [Required] The PUT request body containing external API definitions. Currently, only Swagger definition JSON files are supported. The maximum size of the API definition file is 2MB.
praBody :: Lens' PutRestAPI ByteString
praBody = lens _praBody (\ s a -> s{_praBody = a})

instance AWSRequest PutRestAPI where
        type Rs PutRestAPI = RestAPI
        request = putBody apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutRestAPI where

instance NFData PutRestAPI where

instance ToBody PutRestAPI where
        toBody = toBody . _praBody

instance ToHeaders PutRestAPI where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath PutRestAPI where
        toPath PutRestAPI'{..}
          = mconcat ["/restapis/", toBS _praRestAPIId]

instance ToQuery PutRestAPI where
        toQuery PutRestAPI'{..}
          = mconcat
              ["mode" =: _praMode,
               "failonwarnings" =: _praFailOnWarnings,
               "parameters" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _praParameters)]

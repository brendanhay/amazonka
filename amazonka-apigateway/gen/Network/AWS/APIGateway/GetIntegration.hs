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
-- Module      : Network.AWS.APIGateway.GetIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the integration settings.
--
--
module Network.AWS.APIGateway.GetIntegration
    (
    -- * Creating a Request
      getIntegration
    , GetIntegration
    -- * Request Lenses
    , giRestAPIId
    , giResourceId
    , giHttpMethod

    -- * Destructuring the Response
    , integration
    , Integration
    -- * Response Lenses
    , iHttpMethod
    , iRequestTemplates
    , iCredentials
    , iConnectionId
    , iRequestParameters
    , iContentHandling
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iTimeoutInMillis
    , iType
    , iConnectionType
    , iCacheKeyParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to get the integration configuration.
--
--
--
-- /See:/ 'getIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { _giRestAPIId  :: !Text
  , _giResourceId :: !Text
  , _giHttpMethod :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'giResourceId' - [Required] Specifies a get integration request's resource identifier
--
-- * 'giHttpMethod' - [Required] Specifies a get integration request's HTTP method.
getIntegration
    :: Text -- ^ 'giRestAPIId'
    -> Text -- ^ 'giResourceId'
    -> Text -- ^ 'giHttpMethod'
    -> GetIntegration
getIntegration pRestAPIId_ pResourceId_ pHttpMethod_ =
  GetIntegration'
    { _giRestAPIId = pRestAPIId_
    , _giResourceId = pResourceId_
    , _giHttpMethod = pHttpMethod_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
giRestAPIId :: Lens' GetIntegration Text
giRestAPIId = lens _giRestAPIId (\ s a -> s{_giRestAPIId = a})

-- | [Required] Specifies a get integration request's resource identifier
giResourceId :: Lens' GetIntegration Text
giResourceId = lens _giResourceId (\ s a -> s{_giResourceId = a})

-- | [Required] Specifies a get integration request's HTTP method.
giHttpMethod :: Lens' GetIntegration Text
giHttpMethod = lens _giHttpMethod (\ s a -> s{_giHttpMethod = a})

instance AWSRequest GetIntegration where
        type Rs GetIntegration = Integration
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetIntegration where

instance NFData GetIntegration where

instance ToHeaders GetIntegration where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetIntegration where
        toPath GetIntegration'{..}
          = mconcat
              ["/restapis/", toBS _giRestAPIId, "/resources/",
               toBS _giResourceId, "/methods/", toBS _giHttpMethod,
               "/integration"]

instance ToQuery GetIntegration where
        toQuery = const mempty

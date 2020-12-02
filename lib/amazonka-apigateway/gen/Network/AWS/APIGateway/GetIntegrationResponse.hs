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
-- Module      : Network.AWS.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
--
--
module Network.AWS.APIGateway.GetIntegrationResponse
    (
    -- * Creating a Request
      getIntegrationResponse
    , GetIntegrationResponse
    -- * Request Lenses
    , giiRestAPIId
    , giiResourceId
    , giiHttpMethod
    , giiStatusCode

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

-- | Represents a get integration response request.
--
--
--
-- /See:/ 'getIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { _giiRestAPIId  :: !Text
  , _giiResourceId :: !Text
  , _giiHttpMethod :: !Text
  , _giiStatusCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giiRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'giiResourceId' - [Required] Specifies a get integration response request's resource identifier.
--
-- * 'giiHttpMethod' - [Required] Specifies a get integration response request's HTTP method.
--
-- * 'giiStatusCode' - [Required] Specifies a get integration response request's status code.
getIntegrationResponse
    :: Text -- ^ 'giiRestAPIId'
    -> Text -- ^ 'giiResourceId'
    -> Text -- ^ 'giiHttpMethod'
    -> Text -- ^ 'giiStatusCode'
    -> GetIntegrationResponse
getIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
  GetIntegrationResponse'
    { _giiRestAPIId = pRestAPIId_
    , _giiResourceId = pResourceId_
    , _giiHttpMethod = pHttpMethod_
    , _giiStatusCode = pStatusCode_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
giiRestAPIId :: Lens' GetIntegrationResponse Text
giiRestAPIId = lens _giiRestAPIId (\ s a -> s{_giiRestAPIId = a})

-- | [Required] Specifies a get integration response request's resource identifier.
giiResourceId :: Lens' GetIntegrationResponse Text
giiResourceId = lens _giiResourceId (\ s a -> s{_giiResourceId = a})

-- | [Required] Specifies a get integration response request's HTTP method.
giiHttpMethod :: Lens' GetIntegrationResponse Text
giiHttpMethod = lens _giiHttpMethod (\ s a -> s{_giiHttpMethod = a})

-- | [Required] Specifies a get integration response request's status code.
giiStatusCode :: Lens' GetIntegrationResponse Text
giiStatusCode = lens _giiStatusCode (\ s a -> s{_giiStatusCode = a})

instance AWSRequest GetIntegrationResponse where
        type Rs GetIntegrationResponse = IntegrationResponse
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetIntegrationResponse where

instance NFData GetIntegrationResponse where

instance ToHeaders GetIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetIntegrationResponse where
        toPath GetIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _giiRestAPIId, "/resources/",
               toBS _giiResourceId, "/methods/",
               toBS _giiHttpMethod, "/integration/responses/",
               toBS _giiStatusCode]

instance ToQuery GetIntegrationResponse where
        toQuery = const mempty

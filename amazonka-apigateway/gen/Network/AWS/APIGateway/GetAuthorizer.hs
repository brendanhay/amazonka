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
-- Module      : Network.AWS.APIGateway.GetAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizer' resource.
--
--
-- <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html AWS CLI>
module Network.AWS.APIGateway.GetAuthorizer
    (
    -- * Creating a Request
      getAuthorizer
    , GetAuthorizer
    -- * Request Lenses
    , gaaRestAPIId
    , gaaAuthorizerId

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aProviderARNs
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aAuthType
    , aType
    , aIdentitySource
    , aAuthorizerCredentials
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to describe an existing 'Authorizer' resource.
--
--
--
-- /See:/ 'getAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { _gaaRestAPIId    :: !Text
  , _gaaAuthorizerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaaRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gaaAuthorizerId' - [Required] The identifier of the 'Authorizer' resource.
getAuthorizer
    :: Text -- ^ 'gaaRestAPIId'
    -> Text -- ^ 'gaaAuthorizerId'
    -> GetAuthorizer
getAuthorizer pRestAPIId_ pAuthorizerId_ =
  GetAuthorizer'
    {_gaaRestAPIId = pRestAPIId_, _gaaAuthorizerId = pAuthorizerId_}


-- | [Required] The string identifier of the associated 'RestApi' .
gaaRestAPIId :: Lens' GetAuthorizer Text
gaaRestAPIId = lens _gaaRestAPIId (\ s a -> s{_gaaRestAPIId = a})

-- | [Required] The identifier of the 'Authorizer' resource.
gaaAuthorizerId :: Lens' GetAuthorizer Text
gaaAuthorizerId = lens _gaaAuthorizerId (\ s a -> s{_gaaAuthorizerId = a})

instance AWSRequest GetAuthorizer where
        type Rs GetAuthorizer = Authorizer
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetAuthorizer where

instance NFData GetAuthorizer where

instance ToHeaders GetAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetAuthorizer where
        toPath GetAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _gaaRestAPIId, "/authorizers/",
               toBS _gaaAuthorizerId]

instance ToQuery GetAuthorizer where
        toQuery = const mempty

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
-- Module      : Network.AWS.AppSync.CreateGraphqlAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @GraphqlApi@ object.
--
--
module Network.AWS.AppSync.CreateGraphqlAPI
    (
    -- * Creating a Request
      createGraphqlAPI
    , CreateGraphqlAPI
    -- * Request Lenses
    , cgaOpenIdConnectConfig
    , cgaUserPoolConfig
    , cgaLogConfig
    , cgaName
    , cgaAuthenticationType

    -- * Destructuring the Response
    , createGraphqlAPIResponse
    , CreateGraphqlAPIResponse
    -- * Response Lenses
    , cgarsGraphqlAPI
    , cgarsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGraphqlAPI' smart constructor.
data CreateGraphqlAPI = CreateGraphqlAPI'
  { _cgaOpenIdConnectConfig :: !(Maybe OpenIdConnectConfig)
  , _cgaUserPoolConfig      :: !(Maybe UserPoolConfig)
  , _cgaLogConfig           :: !(Maybe LogConfig)
  , _cgaName                :: !Text
  , _cgaAuthenticationType  :: !AuthenticationType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGraphqlAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgaOpenIdConnectConfig' - The Open Id Connect configuration configuration.
--
-- * 'cgaUserPoolConfig' - The Amazon Cognito User Pool configuration.
--
-- * 'cgaLogConfig' - The Amazon CloudWatch logs configuration.
--
-- * 'cgaName' - A user-supplied name for the @GraphqlApi@ .
--
-- * 'cgaAuthenticationType' - The authentication type: API key, IAM, or Amazon Cognito User Pools.
createGraphqlAPI
    :: Text -- ^ 'cgaName'
    -> AuthenticationType -- ^ 'cgaAuthenticationType'
    -> CreateGraphqlAPI
createGraphqlAPI pName_ pAuthenticationType_ =
  CreateGraphqlAPI'
    { _cgaOpenIdConnectConfig = Nothing
    , _cgaUserPoolConfig = Nothing
    , _cgaLogConfig = Nothing
    , _cgaName = pName_
    , _cgaAuthenticationType = pAuthenticationType_
    }


-- | The Open Id Connect configuration configuration.
cgaOpenIdConnectConfig :: Lens' CreateGraphqlAPI (Maybe OpenIdConnectConfig)
cgaOpenIdConnectConfig = lens _cgaOpenIdConnectConfig (\ s a -> s{_cgaOpenIdConnectConfig = a})

-- | The Amazon Cognito User Pool configuration.
cgaUserPoolConfig :: Lens' CreateGraphqlAPI (Maybe UserPoolConfig)
cgaUserPoolConfig = lens _cgaUserPoolConfig (\ s a -> s{_cgaUserPoolConfig = a})

-- | The Amazon CloudWatch logs configuration.
cgaLogConfig :: Lens' CreateGraphqlAPI (Maybe LogConfig)
cgaLogConfig = lens _cgaLogConfig (\ s a -> s{_cgaLogConfig = a})

-- | A user-supplied name for the @GraphqlApi@ .
cgaName :: Lens' CreateGraphqlAPI Text
cgaName = lens _cgaName (\ s a -> s{_cgaName = a})

-- | The authentication type: API key, IAM, or Amazon Cognito User Pools.
cgaAuthenticationType :: Lens' CreateGraphqlAPI AuthenticationType
cgaAuthenticationType = lens _cgaAuthenticationType (\ s a -> s{_cgaAuthenticationType = a})

instance AWSRequest CreateGraphqlAPI where
        type Rs CreateGraphqlAPI = CreateGraphqlAPIResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateGraphqlAPIResponse' <$>
                   (x .?> "graphqlApi") <*> (pure (fromEnum s)))

instance Hashable CreateGraphqlAPI where

instance NFData CreateGraphqlAPI where

instance ToHeaders CreateGraphqlAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGraphqlAPI where
        toJSON CreateGraphqlAPI'{..}
          = object
              (catMaybes
                 [("openIDConnectConfig" .=) <$>
                    _cgaOpenIdConnectConfig,
                  ("userPoolConfig" .=) <$> _cgaUserPoolConfig,
                  ("logConfig" .=) <$> _cgaLogConfig,
                  Just ("name" .= _cgaName),
                  Just
                    ("authenticationType" .= _cgaAuthenticationType)])

instance ToPath CreateGraphqlAPI where
        toPath = const "/v1/apis"

instance ToQuery CreateGraphqlAPI where
        toQuery = const mempty

-- | /See:/ 'createGraphqlAPIResponse' smart constructor.
data CreateGraphqlAPIResponse = CreateGraphqlAPIResponse'
  { _cgarsGraphqlAPI     :: !(Maybe GraphqlAPI)
  , _cgarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgarsGraphqlAPI' - The @GraphqlApi@ .
--
-- * 'cgarsResponseStatus' - -- | The response status code.
createGraphqlAPIResponse
    :: Int -- ^ 'cgarsResponseStatus'
    -> CreateGraphqlAPIResponse
createGraphqlAPIResponse pResponseStatus_ =
  CreateGraphqlAPIResponse'
    {_cgarsGraphqlAPI = Nothing, _cgarsResponseStatus = pResponseStatus_}


-- | The @GraphqlApi@ .
cgarsGraphqlAPI :: Lens' CreateGraphqlAPIResponse (Maybe GraphqlAPI)
cgarsGraphqlAPI = lens _cgarsGraphqlAPI (\ s a -> s{_cgarsGraphqlAPI = a})

-- | -- | The response status code.
cgarsResponseStatus :: Lens' CreateGraphqlAPIResponse Int
cgarsResponseStatus = lens _cgarsResponseStatus (\ s a -> s{_cgarsResponseStatus = a})

instance NFData CreateGraphqlAPIResponse where

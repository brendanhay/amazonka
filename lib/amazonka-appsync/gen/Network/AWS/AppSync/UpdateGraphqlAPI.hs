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
-- Module      : Network.AWS.AppSync.UpdateGraphqlAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @GraphqlApi@ object.
--
--
module Network.AWS.AppSync.UpdateGraphqlAPI
    (
    -- * Creating a Request
      updateGraphqlAPI
    , UpdateGraphqlAPI
    -- * Request Lenses
    , ugaOpenIdConnectConfig
    , ugaUserPoolConfig
    , ugaAuthenticationType
    , ugaLogConfig
    , ugaApiId
    , ugaName

    -- * Destructuring the Response
    , updateGraphqlAPIResponse
    , UpdateGraphqlAPIResponse
    -- * Response Lenses
    , ugarsGraphqlAPI
    , ugarsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGraphqlAPI' smart constructor.
data UpdateGraphqlAPI = UpdateGraphqlAPI'
  { _ugaOpenIdConnectConfig :: !(Maybe OpenIdConnectConfig)
  , _ugaUserPoolConfig      :: !(Maybe UserPoolConfig)
  , _ugaAuthenticationType  :: !(Maybe AuthenticationType)
  , _ugaLogConfig           :: !(Maybe LogConfig)
  , _ugaApiId               :: !Text
  , _ugaName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGraphqlAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugaOpenIdConnectConfig' - The Open Id Connect configuration configuration for the @GraphqlApi@ object.
--
-- * 'ugaUserPoolConfig' - The new Amazon Cognito User Pool configuration for the @GraphqlApi@ object.
--
-- * 'ugaAuthenticationType' - The new authentication type for the @GraphqlApi@ object.
--
-- * 'ugaLogConfig' - The Amazon CloudWatch logs configuration for the @GraphqlApi@ object.
--
-- * 'ugaApiId' - The API ID.
--
-- * 'ugaName' - The new name for the @GraphqlApi@ object.
updateGraphqlAPI
    :: Text -- ^ 'ugaApiId'
    -> Text -- ^ 'ugaName'
    -> UpdateGraphqlAPI
updateGraphqlAPI pApiId_ pName_ =
  UpdateGraphqlAPI'
    { _ugaOpenIdConnectConfig = Nothing
    , _ugaUserPoolConfig = Nothing
    , _ugaAuthenticationType = Nothing
    , _ugaLogConfig = Nothing
    , _ugaApiId = pApiId_
    , _ugaName = pName_
    }


-- | The Open Id Connect configuration configuration for the @GraphqlApi@ object.
ugaOpenIdConnectConfig :: Lens' UpdateGraphqlAPI (Maybe OpenIdConnectConfig)
ugaOpenIdConnectConfig = lens _ugaOpenIdConnectConfig (\ s a -> s{_ugaOpenIdConnectConfig = a})

-- | The new Amazon Cognito User Pool configuration for the @GraphqlApi@ object.
ugaUserPoolConfig :: Lens' UpdateGraphqlAPI (Maybe UserPoolConfig)
ugaUserPoolConfig = lens _ugaUserPoolConfig (\ s a -> s{_ugaUserPoolConfig = a})

-- | The new authentication type for the @GraphqlApi@ object.
ugaAuthenticationType :: Lens' UpdateGraphqlAPI (Maybe AuthenticationType)
ugaAuthenticationType = lens _ugaAuthenticationType (\ s a -> s{_ugaAuthenticationType = a})

-- | The Amazon CloudWatch logs configuration for the @GraphqlApi@ object.
ugaLogConfig :: Lens' UpdateGraphqlAPI (Maybe LogConfig)
ugaLogConfig = lens _ugaLogConfig (\ s a -> s{_ugaLogConfig = a})

-- | The API ID.
ugaApiId :: Lens' UpdateGraphqlAPI Text
ugaApiId = lens _ugaApiId (\ s a -> s{_ugaApiId = a})

-- | The new name for the @GraphqlApi@ object.
ugaName :: Lens' UpdateGraphqlAPI Text
ugaName = lens _ugaName (\ s a -> s{_ugaName = a})

instance AWSRequest UpdateGraphqlAPI where
        type Rs UpdateGraphqlAPI = UpdateGraphqlAPIResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGraphqlAPIResponse' <$>
                   (x .?> "graphqlApi") <*> (pure (fromEnum s)))

instance Hashable UpdateGraphqlAPI where

instance NFData UpdateGraphqlAPI where

instance ToHeaders UpdateGraphqlAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGraphqlAPI where
        toJSON UpdateGraphqlAPI'{..}
          = object
              (catMaybes
                 [("openIDConnectConfig" .=) <$>
                    _ugaOpenIdConnectConfig,
                  ("userPoolConfig" .=) <$> _ugaUserPoolConfig,
                  ("authenticationType" .=) <$> _ugaAuthenticationType,
                  ("logConfig" .=) <$> _ugaLogConfig,
                  Just ("name" .= _ugaName)])

instance ToPath UpdateGraphqlAPI where
        toPath UpdateGraphqlAPI'{..}
          = mconcat ["/v1/apis/", toBS _ugaApiId]

instance ToQuery UpdateGraphqlAPI where
        toQuery = const mempty

-- | /See:/ 'updateGraphqlAPIResponse' smart constructor.
data UpdateGraphqlAPIResponse = UpdateGraphqlAPIResponse'
  { _ugarsGraphqlAPI     :: !(Maybe GraphqlAPI)
  , _ugarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugarsGraphqlAPI' - The updated @GraphqlApi@ object.
--
-- * 'ugarsResponseStatus' - -- | The response status code.
updateGraphqlAPIResponse
    :: Int -- ^ 'ugarsResponseStatus'
    -> UpdateGraphqlAPIResponse
updateGraphqlAPIResponse pResponseStatus_ =
  UpdateGraphqlAPIResponse'
    {_ugarsGraphqlAPI = Nothing, _ugarsResponseStatus = pResponseStatus_}


-- | The updated @GraphqlApi@ object.
ugarsGraphqlAPI :: Lens' UpdateGraphqlAPIResponse (Maybe GraphqlAPI)
ugarsGraphqlAPI = lens _ugarsGraphqlAPI (\ s a -> s{_ugarsGraphqlAPI = a})

-- | -- | The response status code.
ugarsResponseStatus :: Lens' UpdateGraphqlAPIResponse Int
ugarsResponseStatus = lens _ugarsResponseStatus (\ s a -> s{_ugarsResponseStatus = a})

instance NFData UpdateGraphqlAPIResponse where

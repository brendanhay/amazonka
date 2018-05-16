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
-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings of an identity pool.
--
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
    (
    -- * Creating a Request
      getIdentityPoolConfiguration
    , GetIdentityPoolConfiguration
    -- * Request Lenses
    , gipcIdentityPoolId

    -- * Destructuring the Response
    , getIdentityPoolConfigurationResponse
    , GetIdentityPoolConfigurationResponse
    -- * Response Lenses
    , gipcrsIdentityPoolId
    , gipcrsCognitoStreams
    , gipcrsPushSync
    , gipcrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetIdentityPoolConfiguration operation.
--
--
--
-- /See:/ 'getIdentityPoolConfiguration' smart constructor.
newtype GetIdentityPoolConfiguration = GetIdentityPoolConfiguration'
  { _gipcIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityPoolConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipcIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
getIdentityPoolConfiguration
    :: Text -- ^ 'gipcIdentityPoolId'
    -> GetIdentityPoolConfiguration
getIdentityPoolConfiguration pIdentityPoolId_ =
  GetIdentityPoolConfiguration' {_gipcIdentityPoolId = pIdentityPoolId_}


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
gipcIdentityPoolId :: Lens' GetIdentityPoolConfiguration Text
gipcIdentityPoolId = lens _gipcIdentityPoolId (\ s a -> s{_gipcIdentityPoolId = a})

instance AWSRequest GetIdentityPoolConfiguration
         where
        type Rs GetIdentityPoolConfiguration =
             GetIdentityPoolConfigurationResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 GetIdentityPoolConfigurationResponse' <$>
                   (x .?> "IdentityPoolId") <*> (x .?> "CognitoStreams")
                     <*> (x .?> "PushSync")
                     <*> (pure (fromEnum s)))

instance Hashable GetIdentityPoolConfiguration where

instance NFData GetIdentityPoolConfiguration where

instance ToHeaders GetIdentityPoolConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIdentityPoolConfiguration where
        toPath GetIdentityPoolConfiguration'{..}
          = mconcat
              ["/identitypools/", toBS _gipcIdentityPoolId,
               "/configuration"]

instance ToQuery GetIdentityPoolConfiguration where
        toQuery = const mempty

-- | The output for the GetIdentityPoolConfiguration operation.
--
--
--
-- /See:/ 'getIdentityPoolConfigurationResponse' smart constructor.
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
  { _gipcrsIdentityPoolId :: !(Maybe Text)
  , _gipcrsCognitoStreams :: !(Maybe CognitoStreams)
  , _gipcrsPushSync       :: !(Maybe PushSync)
  , _gipcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityPoolConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipcrsIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
--
-- * 'gipcrsCognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- * 'gipcrsPushSync' - Options to apply to this identity pool for push synchronization.
--
-- * 'gipcrsResponseStatus' - -- | The response status code.
getIdentityPoolConfigurationResponse
    :: Int -- ^ 'gipcrsResponseStatus'
    -> GetIdentityPoolConfigurationResponse
getIdentityPoolConfigurationResponse pResponseStatus_ =
  GetIdentityPoolConfigurationResponse'
    { _gipcrsIdentityPoolId = Nothing
    , _gipcrsCognitoStreams = Nothing
    , _gipcrsPushSync = Nothing
    , _gipcrsResponseStatus = pResponseStatus_
    }


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
gipcrsIdentityPoolId :: Lens' GetIdentityPoolConfigurationResponse (Maybe Text)
gipcrsIdentityPoolId = lens _gipcrsIdentityPoolId (\ s a -> s{_gipcrsIdentityPoolId = a})

-- | Options to apply to this identity pool for Amazon Cognito streams.
gipcrsCognitoStreams :: Lens' GetIdentityPoolConfigurationResponse (Maybe CognitoStreams)
gipcrsCognitoStreams = lens _gipcrsCognitoStreams (\ s a -> s{_gipcrsCognitoStreams = a})

-- | Options to apply to this identity pool for push synchronization.
gipcrsPushSync :: Lens' GetIdentityPoolConfigurationResponse (Maybe PushSync)
gipcrsPushSync = lens _gipcrsPushSync (\ s a -> s{_gipcrsPushSync = a})

-- | -- | The response status code.
gipcrsResponseStatus :: Lens' GetIdentityPoolConfigurationResponse Int
gipcrsResponseStatus = lens _gipcrsResponseStatus (\ s a -> s{_gipcrsResponseStatus = a})

instance NFData GetIdentityPoolConfigurationResponse
         where

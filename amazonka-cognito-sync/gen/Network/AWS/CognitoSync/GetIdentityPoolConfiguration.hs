{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the configuration settings of an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetIdentityPoolConfiguration.html>
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
    (
    -- * Request
      GetIdentityPoolConfiguration
    -- ** Request constructor
    , getIdentityPoolConfiguration
    -- ** Request lenses
    , gipcIdentityPoolId

    -- * Response
    , GetIdentityPoolConfigurationResponse
    -- ** Response constructor
    , getIdentityPoolConfigurationResponse
    -- ** Response lenses
    , gipcrIdentityPoolId
    , gipcrCognitoStreams
    , gipcrPushSync
    , gipcrStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'getIdentityPoolConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipcIdentityPoolId'
newtype GetIdentityPoolConfiguration = GetIdentityPoolConfiguration'
    { _gipcIdentityPoolId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetIdentityPoolConfiguration' smart constructor.
getIdentityPoolConfiguration :: Text -> GetIdentityPoolConfiguration
getIdentityPoolConfiguration pIdentityPoolId =
    GetIdentityPoolConfiguration'
    { _gipcIdentityPoolId = pIdentityPoolId
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool for which to return a configuration.
gipcIdentityPoolId :: Lens' GetIdentityPoolConfiguration Text
gipcIdentityPoolId = lens _gipcIdentityPoolId (\ s a -> s{_gipcIdentityPoolId = a});

instance AWSRequest GetIdentityPoolConfiguration
         where
        type Sv GetIdentityPoolConfiguration = CognitoSync
        type Rs GetIdentityPoolConfiguration =
             GetIdentityPoolConfigurationResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetIdentityPoolConfigurationResponse' <$>
                   (x .?> "IdentityPoolId") <*> (x .?> "CognitoStreams")
                     <*> (x .?> "PushSync")
                     <*> (pure s))

instance ToHeaders GetIdentityPoolConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIdentityPoolConfiguration where
        toPath GetIdentityPoolConfiguration'{..}
          = mconcat
              ["/identitypools/", toText _gipcIdentityPoolId,
               "/configuration"]

instance ToQuery GetIdentityPoolConfiguration where
        toQuery = const mempty

-- | The output for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'getIdentityPoolConfigurationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipcrIdentityPoolId'
--
-- * 'gipcrCognitoStreams'
--
-- * 'gipcrPushSync'
--
-- * 'gipcrStatus'
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
    { _gipcrIdentityPoolId :: !(Maybe Text)
    , _gipcrCognitoStreams :: !(Maybe CognitoStreams)
    , _gipcrPushSync       :: !(Maybe PushSync)
    , _gipcrStatus         :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetIdentityPoolConfigurationResponse' smart constructor.
getIdentityPoolConfigurationResponse :: Status -> GetIdentityPoolConfigurationResponse
getIdentityPoolConfigurationResponse pStatus =
    GetIdentityPoolConfigurationResponse'
    { _gipcrIdentityPoolId = Nothing
    , _gipcrCognitoStreams = Nothing
    , _gipcrPushSync = Nothing
    , _gipcrStatus = pStatus
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
gipcrIdentityPoolId :: Lens' GetIdentityPoolConfigurationResponse (Maybe Text)
gipcrIdentityPoolId = lens _gipcrIdentityPoolId (\ s a -> s{_gipcrIdentityPoolId = a});

-- | Options to apply to this identity pool for Amazon Cognito streams.
gipcrCognitoStreams :: Lens' GetIdentityPoolConfigurationResponse (Maybe CognitoStreams)
gipcrCognitoStreams = lens _gipcrCognitoStreams (\ s a -> s{_gipcrCognitoStreams = a});

-- | Options to apply to this identity pool for push synchronization.
gipcrPushSync :: Lens' GetIdentityPoolConfigurationResponse (Maybe PushSync)
gipcrPushSync = lens _gipcrPushSync (\ s a -> s{_gipcrPushSync = a});

-- | FIXME: Undocumented member.
gipcrStatus :: Lens' GetIdentityPoolConfigurationResponse Status
gipcrStatus = lens _gipcrStatus (\ s a -> s{_gipcrStatus = a});

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
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

-- | Sets the necessary configuration for push sync.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SetIdentityPoolConfiguration.html>
module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
    (
    -- * Request
      SetIdentityPoolConfiguration
    -- ** Request constructor
    , setIdentityPoolConfiguration
    -- ** Request lenses
    , sipcCognitoStreams
    , sipcPushSync
    , sipcIdentityPoolId

    -- * Response
    , SetIdentityPoolConfigurationResponse
    -- ** Response constructor
    , setIdentityPoolConfigurationResponse
    -- ** Response lenses
    , sipcrCognitoStreams
    , sipcrPushSync
    , sipcrIdentityPoolId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'setIdentityPoolConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcCognitoStreams'
--
-- * 'sipcPushSync'
--
-- * 'sipcIdentityPoolId'
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'{_sipcCognitoStreams :: Maybe CognitoStreams, _sipcPushSync :: Maybe PushSync, _sipcIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'SetIdentityPoolConfiguration' smart constructor.
setIdentityPoolConfiguration :: Text -> SetIdentityPoolConfiguration
setIdentityPoolConfiguration pIdentityPoolId = SetIdentityPoolConfiguration'{_sipcCognitoStreams = Nothing, _sipcPushSync = Nothing, _sipcIdentityPoolId = pIdentityPoolId};

-- | Options to apply to this identity pool for Amazon Cognito streams.
sipcCognitoStreams :: Lens' SetIdentityPoolConfiguration (Maybe CognitoStreams)
sipcCognitoStreams = lens _sipcCognitoStreams (\ s a -> s{_sipcCognitoStreams = a});

-- | Options to apply to this identity pool for push synchronization.
sipcPushSync :: Lens' SetIdentityPoolConfiguration (Maybe PushSync)
sipcPushSync = lens _sipcPushSync (\ s a -> s{_sipcPushSync = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool to modify.
sipcIdentityPoolId :: Lens' SetIdentityPoolConfiguration Text
sipcIdentityPoolId = lens _sipcIdentityPoolId (\ s a -> s{_sipcIdentityPoolId = a});

instance AWSRequest SetIdentityPoolConfiguration
         where
        type Sv SetIdentityPoolConfiguration = CognitoSync
        type Rs SetIdentityPoolConfiguration =
             SetIdentityPoolConfigurationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 SetIdentityPoolConfigurationResponse' <$>
                   x .?> "CognitoStreams" <*> x .?> "PushSync" <*>
                     x .:> "IdentityPoolId")

instance ToHeaders SetIdentityPoolConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetIdentityPoolConfiguration where
        toJSON SetIdentityPoolConfiguration'{..}
          = object
              ["CognitoStreams" .= _sipcCognitoStreams,
               "PushSync" .= _sipcPushSync]

instance ToPath SetIdentityPoolConfiguration where
        toPath SetIdentityPoolConfiguration'{..}
          = mconcat
              ["/identitypools/", toText _sipcIdentityPoolId,
               "/configuration"]

instance ToQuery SetIdentityPoolConfiguration where
        toQuery = const mempty

-- | /See:/ 'setIdentityPoolConfigurationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcrCognitoStreams'
--
-- * 'sipcrPushSync'
--
-- * 'sipcrIdentityPoolId'
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'{_sipcrCognitoStreams :: Maybe CognitoStreams, _sipcrPushSync :: Maybe PushSync, _sipcrIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'SetIdentityPoolConfigurationResponse' smart constructor.
setIdentityPoolConfigurationResponse :: Text -> SetIdentityPoolConfigurationResponse
setIdentityPoolConfigurationResponse pIdentityPoolId = SetIdentityPoolConfigurationResponse'{_sipcrCognitoStreams = Nothing, _sipcrPushSync = Nothing, _sipcrIdentityPoolId = pIdentityPoolId};

-- | Options to apply to this identity pool for Amazon Cognito streams.
sipcrCognitoStreams :: Lens' SetIdentityPoolConfigurationResponse (Maybe CognitoStreams)
sipcrCognitoStreams = lens _sipcrCognitoStreams (\ s a -> s{_sipcrCognitoStreams = a});

-- | Options to apply to this identity pool for push synchronization.
sipcrPushSync :: Lens' SetIdentityPoolConfigurationResponse (Maybe PushSync)
sipcrPushSync = lens _sipcrPushSync (\ s a -> s{_sipcrPushSync = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
sipcrIdentityPoolId :: Lens' SetIdentityPoolConfigurationResponse Text
sipcrIdentityPoolId = lens _sipcrIdentityPoolId (\ s a -> s{_sipcrIdentityPoolId = a});

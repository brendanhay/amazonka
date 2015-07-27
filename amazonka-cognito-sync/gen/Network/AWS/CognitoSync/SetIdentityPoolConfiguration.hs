{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the necessary configuration for push sync.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
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
    , sipcrsIdentityPoolId
    , sipcrsCognitoStreams
    , sipcrsPushSync
    , sipcrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the SetIdentityPoolConfiguration operation.
--
-- /See:/ 'setIdentityPoolConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcCognitoStreams'
--
-- * 'sipcPushSync'
--
-- * 'sipcIdentityPoolId'
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'
    { _sipcCognitoStreams :: !(Maybe CognitoStreams)
    , _sipcPushSync       :: !(Maybe PushSync)
    , _sipcIdentityPoolId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetIdentityPoolConfiguration' smart constructor.
setIdentityPoolConfiguration :: Text -> SetIdentityPoolConfiguration
setIdentityPoolConfiguration pIdentityPoolId_ =
    SetIdentityPoolConfiguration'
    { _sipcCognitoStreams = Nothing
    , _sipcPushSync = Nothing
    , _sipcIdentityPoolId = pIdentityPoolId_
    }

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
                   (x .?> "IdentityPoolId") <*> (x .?> "CognitoStreams")
                     <*> (x .?> "PushSync")
                     <*> (pure (fromEnum s)))

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
              ["/identitypools/", toPath _sipcIdentityPoolId,
               "/configuration"]

instance ToQuery SetIdentityPoolConfiguration where
        toQuery = const mempty

-- | The output for the SetIdentityPoolConfiguration operation
--
-- /See:/ 'setIdentityPoolConfigurationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcrsIdentityPoolId'
--
-- * 'sipcrsCognitoStreams'
--
-- * 'sipcrsPushSync'
--
-- * 'sipcrsStatus'
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'
    { _sipcrsIdentityPoolId :: !(Maybe Text)
    , _sipcrsCognitoStreams :: !(Maybe CognitoStreams)
    , _sipcrsPushSync       :: !(Maybe PushSync)
    , _sipcrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetIdentityPoolConfigurationResponse' smart constructor.
setIdentityPoolConfigurationResponse :: Int -> SetIdentityPoolConfigurationResponse
setIdentityPoolConfigurationResponse pStatus_ =
    SetIdentityPoolConfigurationResponse'
    { _sipcrsIdentityPoolId = Nothing
    , _sipcrsCognitoStreams = Nothing
    , _sipcrsPushSync = Nothing
    , _sipcrsStatus = pStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
sipcrsIdentityPoolId :: Lens' SetIdentityPoolConfigurationResponse (Maybe Text)
sipcrsIdentityPoolId = lens _sipcrsIdentityPoolId (\ s a -> s{_sipcrsIdentityPoolId = a});

-- | Options to apply to this identity pool for Amazon Cognito streams.
sipcrsCognitoStreams :: Lens' SetIdentityPoolConfigurationResponse (Maybe CognitoStreams)
sipcrsCognitoStreams = lens _sipcrsCognitoStreams (\ s a -> s{_sipcrsCognitoStreams = a});

-- | Options to apply to this identity pool for push synchronization.
sipcrsPushSync :: Lens' SetIdentityPoolConfigurationResponse (Maybe PushSync)
sipcrsPushSync = lens _sipcrsPushSync (\ s a -> s{_sipcrsPushSync = a});

-- | FIXME: Undocumented member.
sipcrsStatus :: Lens' SetIdentityPoolConfigurationResponse Int
sipcrsStatus = lens _sipcrsStatus (\ s a -> s{_sipcrsStatus = a});

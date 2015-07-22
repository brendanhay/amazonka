{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.RegisterDevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a device to receive push sync notifications.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_RegisterDevice.html>
module Network.AWS.CognitoSync.RegisterDevice
    (
    -- * Request
      RegisterDevice
    -- ** Request constructor
    , registerDevice
    -- ** Request lenses
    , rdrqIdentityPoolId
    , rdrqIdentityId
    , rdrqPlatform
    , rdrqToken

    -- * Response
    , RegisterDeviceResponse
    -- ** Response constructor
    , registerDeviceResponse
    -- ** Response lenses
    , rdrsDeviceId
    , rdrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to RegisterDevice.
--
-- /See:/ 'registerDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrqIdentityPoolId'
--
-- * 'rdrqIdentityId'
--
-- * 'rdrqPlatform'
--
-- * 'rdrqToken'
data RegisterDevice = RegisterDevice'
    { _rdrqIdentityPoolId :: !Text
    , _rdrqIdentityId     :: !Text
    , _rdrqPlatform       :: !Platform
    , _rdrqToken          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDevice' smart constructor.
registerDevice :: Text -> Text -> Platform -> Text -> RegisterDevice
registerDevice pIdentityPoolId pIdentityId pPlatform pToken =
    RegisterDevice'
    { _rdrqIdentityPoolId = pIdentityPoolId
    , _rdrqIdentityId = pIdentityId
    , _rdrqPlatform = pPlatform
    , _rdrqToken = pToken
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. Here, the ID of the pool that the identity belongs to.
rdrqIdentityPoolId :: Lens' RegisterDevice Text
rdrqIdentityPoolId = lens _rdrqIdentityPoolId (\ s a -> s{_rdrqIdentityPoolId = a});

-- | The unique ID for this identity.
rdrqIdentityId :: Lens' RegisterDevice Text
rdrqIdentityId = lens _rdrqIdentityId (\ s a -> s{_rdrqIdentityId = a});

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
rdrqPlatform :: Lens' RegisterDevice Platform
rdrqPlatform = lens _rdrqPlatform (\ s a -> s{_rdrqPlatform = a});

-- | The push token.
rdrqToken :: Lens' RegisterDevice Text
rdrqToken = lens _rdrqToken (\ s a -> s{_rdrqToken = a});

instance AWSRequest RegisterDevice where
        type Sv RegisterDevice = CognitoSync
        type Rs RegisterDevice = RegisterDeviceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDeviceResponse' <$>
                   (x .?> "DeviceId") <*> (pure (fromEnum s)))

instance ToHeaders RegisterDevice where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterDevice where
        toJSON RegisterDevice'{..}
          = object
              ["Platform" .= _rdrqPlatform, "Token" .= _rdrqToken]

instance ToPath RegisterDevice where
        toPath RegisterDevice'{..}
          = mconcat
              ["/identitypools/", toText _rdrqIdentityPoolId,
               "/identity/", toText _rdrqIdentityId, "/device"]

instance ToQuery RegisterDevice where
        toQuery = const mempty

-- | Response to a RegisterDevice request.
--
-- /See:/ 'registerDeviceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrsDeviceId'
--
-- * 'rdrsStatus'
data RegisterDeviceResponse = RegisterDeviceResponse'
    { _rdrsDeviceId :: !(Maybe Text)
    , _rdrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDeviceResponse' smart constructor.
registerDeviceResponse :: Int -> RegisterDeviceResponse
registerDeviceResponse pStatus =
    RegisterDeviceResponse'
    { _rdrsDeviceId = Nothing
    , _rdrsStatus = pStatus
    }

-- | The unique ID generated for this device by Cognito.
rdrsDeviceId :: Lens' RegisterDeviceResponse (Maybe Text)
rdrsDeviceId = lens _rdrsDeviceId (\ s a -> s{_rdrsDeviceId = a});

-- | FIXME: Undocumented member.
rdrsStatus :: Lens' RegisterDeviceResponse Int
rdrsStatus = lens _rdrsStatus (\ s a -> s{_rdrsStatus = a});

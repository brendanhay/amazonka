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
-- Module      : Network.AWS.CognitoSync.RegisterDevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device to receive push sync notifications.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
--
-- /See:/ <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_RegisterDevice.html AWS API Reference> for RegisterDevice.
module Network.AWS.CognitoSync.RegisterDevice
    (
    -- * Creating a Request
      registerDevice
    , RegisterDevice
    -- * Request Lenses
    , rdIdentityPoolId
    , rdIdentityId
    , rdPlatform
    , rdToken

    -- * Destructuring the Response
    , registerDeviceResponse
    , RegisterDeviceResponse
    -- * Response Lenses
    , rdrsDeviceId
    , rdrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.CognitoSync.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to RegisterDevice.
--
-- /See:/ 'registerDevice' smart constructor.
data RegisterDevice = RegisterDevice'
    { _rdIdentityPoolId :: !Text
    , _rdIdentityId     :: !Text
    , _rdPlatform       :: !Platform
    , _rdToken          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdIdentityPoolId'
--
-- * 'rdIdentityId'
--
-- * 'rdPlatform'
--
-- * 'rdToken'
registerDevice
    :: Text -- ^ 'rdIdentityPoolId'
    -> Text -- ^ 'rdIdentityId'
    -> Platform -- ^ 'rdPlatform'
    -> Text -- ^ 'rdToken'
    -> RegisterDevice
registerDevice pIdentityPoolId_ pIdentityId_ pPlatform_ pToken_ =
    RegisterDevice'
    { _rdIdentityPoolId = pIdentityPoolId_
    , _rdIdentityId = pIdentityId_
    , _rdPlatform = pPlatform_
    , _rdToken = pToken_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. Here, the ID of the pool that the identity belongs to.
rdIdentityPoolId :: Lens' RegisterDevice Text
rdIdentityPoolId = lens _rdIdentityPoolId (\ s a -> s{_rdIdentityPoolId = a});

-- | The unique ID for this identity.
rdIdentityId :: Lens' RegisterDevice Text
rdIdentityId = lens _rdIdentityId (\ s a -> s{_rdIdentityId = a});

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
rdPlatform :: Lens' RegisterDevice Platform
rdPlatform = lens _rdPlatform (\ s a -> s{_rdPlatform = a});

-- | The push token.
rdToken :: Lens' RegisterDevice Text
rdToken = lens _rdToken (\ s a -> s{_rdToken = a});

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
              (catMaybes
                 [Just ("Platform" .= _rdPlatform),
                  Just ("Token" .= _rdToken)])

instance ToPath RegisterDevice where
        toPath RegisterDevice'{..}
          = mconcat
              ["/identitypools/", toBS _rdIdentityPoolId,
               "/identity/", toBS _rdIdentityId, "/device"]

instance ToQuery RegisterDevice where
        toQuery = const mempty

-- | Response to a RegisterDevice request.
--
-- /See:/ 'registerDeviceResponse' smart constructor.
data RegisterDeviceResponse = RegisterDeviceResponse'
    { _rdrsDeviceId :: !(Maybe Text)
    , _rdrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdrsDeviceId'
--
-- * 'rdrsStatus'
registerDeviceResponse
    :: Int -- ^ 'rdrsStatus'
    -> RegisterDeviceResponse
registerDeviceResponse pStatus_ =
    RegisterDeviceResponse'
    { _rdrsDeviceId = Nothing
    , _rdrsStatus = pStatus_
    }

-- | The unique ID generated for this device by Cognito.
rdrsDeviceId :: Lens' RegisterDeviceResponse (Maybe Text)
rdrsDeviceId = lens _rdrsDeviceId (\ s a -> s{_rdrsDeviceId = a});

-- | The response status code.
rdrsStatus :: Lens' RegisterDeviceResponse Int
rdrsStatus = lens _rdrsStatus (\ s a -> s{_rdrsStatus = a});

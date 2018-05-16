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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device to receive push sync notifications.
--
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
--
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
    , rdrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to RegisterDevice.
--
--
--
-- /See:/ 'registerDevice' smart constructor.
data RegisterDevice = RegisterDevice'
  { _rdIdentityPoolId :: !Text
  , _rdIdentityId     :: !Text
  , _rdPlatform       :: !Platform
  , _rdToken          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
--
-- * 'rdIdentityId' - The unique ID for this identity.
--
-- * 'rdPlatform' - The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
--
-- * 'rdToken' - The push token.
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


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. Here, the ID of the pool that the identity belongs to.
rdIdentityPoolId :: Lens' RegisterDevice Text
rdIdentityPoolId = lens _rdIdentityPoolId (\ s a -> s{_rdIdentityPoolId = a})

-- | The unique ID for this identity.
rdIdentityId :: Lens' RegisterDevice Text
rdIdentityId = lens _rdIdentityId (\ s a -> s{_rdIdentityId = a})

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
rdPlatform :: Lens' RegisterDevice Platform
rdPlatform = lens _rdPlatform (\ s a -> s{_rdPlatform = a})

-- | The push token.
rdToken :: Lens' RegisterDevice Text
rdToken = lens _rdToken (\ s a -> s{_rdToken = a})

instance AWSRequest RegisterDevice where
        type Rs RegisterDevice = RegisterDeviceResponse
        request = postJSON cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDeviceResponse' <$>
                   (x .?> "DeviceId") <*> (pure (fromEnum s)))

instance Hashable RegisterDevice where

instance NFData RegisterDevice where

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
--
--
-- /See:/ 'registerDeviceResponse' smart constructor.
data RegisterDeviceResponse = RegisterDeviceResponse'
  { _rdrsDeviceId       :: !(Maybe Text)
  , _rdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdrsDeviceId' - The unique ID generated for this device by Cognito.
--
-- * 'rdrsResponseStatus' - -- | The response status code.
registerDeviceResponse
    :: Int -- ^ 'rdrsResponseStatus'
    -> RegisterDeviceResponse
registerDeviceResponse pResponseStatus_ =
  RegisterDeviceResponse'
    {_rdrsDeviceId = Nothing, _rdrsResponseStatus = pResponseStatus_}


-- | The unique ID generated for this device by Cognito.
rdrsDeviceId :: Lens' RegisterDeviceResponse (Maybe Text)
rdrsDeviceId = lens _rdrsDeviceId (\ s a -> s{_rdrsDeviceId = a})

-- | -- | The response status code.
rdrsResponseStatus :: Lens' RegisterDeviceResponse Int
rdrsResponseStatus = lens _rdrsResponseStatus (\ s a -> s{_rdrsResponseStatus = a})

instance NFData RegisterDeviceResponse where

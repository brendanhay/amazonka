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
-- Module      : Network.AWS.DeviceFarm.CreateRemoteAccessSession
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies and starts a remote access session.
--
--
module Network.AWS.DeviceFarm.CreateRemoteAccessSession
    (
    -- * Creating a Request
      createRemoteAccessSession
    , CreateRemoteAccessSession
    -- * Request Lenses
    , crasName
    , crasConfiguration
    , crasProjectARN
    , crasDeviceARN

    -- * Destructuring the Response
    , createRemoteAccessSessionResponse
    , CreateRemoteAccessSessionResponse
    -- * Response Lenses
    , crasrsRemoteAccessSession
    , crasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates and submits a request to start a remote access session.
--
--
--
-- /See:/ 'createRemoteAccessSession' smart constructor.
data CreateRemoteAccessSession = CreateRemoteAccessSession'
  { _crasName :: {-# NOUNPACK #-}!(Maybe Text)
  , _crasConfiguration :: {-# NOUNPACK #-}!(Maybe CreateRemoteAccessSessionConfiguration)
  , _crasProjectARN :: {-# NOUNPACK #-}!Text
  , _crasDeviceARN :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crasName' - The name of the remote access session that you wish to create.
--
-- * 'crasConfiguration' - The configuration information for the remote access session request.
--
-- * 'crasProjectARN' - The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
--
-- * 'crasDeviceARN' - The Amazon Resource Name (ARN) of the device for which you want to create a remote access session.
createRemoteAccessSession
    :: Text -- ^ 'crasProjectARN'
    -> Text -- ^ 'crasDeviceARN'
    -> CreateRemoteAccessSession
createRemoteAccessSession pProjectARN_ pDeviceARN_ =
  CreateRemoteAccessSession'
  { _crasName = Nothing
  , _crasConfiguration = Nothing
  , _crasProjectARN = pProjectARN_
  , _crasDeviceARN = pDeviceARN_
  }


-- | The name of the remote access session that you wish to create.
crasName :: Lens' CreateRemoteAccessSession (Maybe Text)
crasName = lens _crasName (\ s a -> s{_crasName = a});

-- | The configuration information for the remote access session request.
crasConfiguration :: Lens' CreateRemoteAccessSession (Maybe CreateRemoteAccessSessionConfiguration)
crasConfiguration = lens _crasConfiguration (\ s a -> s{_crasConfiguration = a});

-- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
crasProjectARN :: Lens' CreateRemoteAccessSession Text
crasProjectARN = lens _crasProjectARN (\ s a -> s{_crasProjectARN = a});

-- | The Amazon Resource Name (ARN) of the device for which you want to create a remote access session.
crasDeviceARN :: Lens' CreateRemoteAccessSession Text
crasDeviceARN = lens _crasDeviceARN (\ s a -> s{_crasDeviceARN = a});

instance AWSRequest CreateRemoteAccessSession where
        type Rs CreateRemoteAccessSession =
             CreateRemoteAccessSessionResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateRemoteAccessSessionResponse' <$>
                   (x .?> "remoteAccessSession") <*>
                     (pure (fromEnum s)))

instance Hashable CreateRemoteAccessSession where

instance NFData CreateRemoteAccessSession where

instance ToHeaders CreateRemoteAccessSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateRemoteAccessSession" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRemoteAccessSession where
        toJSON CreateRemoteAccessSession'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _crasName,
                  ("configuration" .=) <$> _crasConfiguration,
                  Just ("projectArn" .= _crasProjectARN),
                  Just ("deviceArn" .= _crasDeviceARN)])

instance ToPath CreateRemoteAccessSession where
        toPath = const "/"

instance ToQuery CreateRemoteAccessSession where
        toQuery = const mempty

-- | Represents the server response from a request to create a remote access session.
--
--
--
-- /See:/ 'createRemoteAccessSessionResponse' smart constructor.
data CreateRemoteAccessSessionResponse = CreateRemoteAccessSessionResponse'
  { _crasrsRemoteAccessSession :: {-# NOUNPACK #-}!(Maybe RemoteAccessSession)
  , _crasrsResponseStatus      :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crasrsRemoteAccessSession' - A container that describes the remote access session when the request to create a remote access session is sent.
--
-- * 'crasrsResponseStatus' - -- | The response status code.
createRemoteAccessSessionResponse
    :: Int -- ^ 'crasrsResponseStatus'
    -> CreateRemoteAccessSessionResponse
createRemoteAccessSessionResponse pResponseStatus_ =
  CreateRemoteAccessSessionResponse'
  { _crasrsRemoteAccessSession = Nothing
  , _crasrsResponseStatus = pResponseStatus_
  }


-- | A container that describes the remote access session when the request to create a remote access session is sent.
crasrsRemoteAccessSession :: Lens' CreateRemoteAccessSessionResponse (Maybe RemoteAccessSession)
crasrsRemoteAccessSession = lens _crasrsRemoteAccessSession (\ s a -> s{_crasrsRemoteAccessSession = a});

-- | -- | The response status code.
crasrsResponseStatus :: Lens' CreateRemoteAccessSessionResponse Int
crasrsResponseStatus = lens _crasrsResponseStatus (\ s a -> s{_crasrsResponseStatus = a});

instance NFData CreateRemoteAccessSessionResponse
         where

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
-- Module      : Network.AWS.DeviceFarm.StopRemoteAccessSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a specified remote access session.
--
--
module Network.AWS.DeviceFarm.StopRemoteAccessSession
    (
    -- * Creating a Request
      stopRemoteAccessSession
    , StopRemoteAccessSession
    -- * Request Lenses
    , srasArn

    -- * Destructuring the Response
    , stopRemoteAccessSessionResponse
    , StopRemoteAccessSessionResponse
    -- * Response Lenses
    , srasrsRemoteAccessSession
    , srasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to stop the remote access session.
--
--
--
-- /See:/ 'stopRemoteAccessSession' smart constructor.
newtype StopRemoteAccessSession = StopRemoteAccessSession'
  { _srasArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srasArn' - The Amazon Resource Name (ARN) of the remote access session you wish to stop.
stopRemoteAccessSession
    :: Text -- ^ 'srasArn'
    -> StopRemoteAccessSession
stopRemoteAccessSession pArn_ = StopRemoteAccessSession' {_srasArn = pArn_}


-- | The Amazon Resource Name (ARN) of the remote access session you wish to stop.
srasArn :: Lens' StopRemoteAccessSession Text
srasArn = lens _srasArn (\ s a -> s{_srasArn = a})

instance AWSRequest StopRemoteAccessSession where
        type Rs StopRemoteAccessSession =
             StopRemoteAccessSessionResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 StopRemoteAccessSessionResponse' <$>
                   (x .?> "remoteAccessSession") <*>
                     (pure (fromEnum s)))

instance Hashable StopRemoteAccessSession where

instance NFData StopRemoteAccessSession where

instance ToHeaders StopRemoteAccessSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.StopRemoteAccessSession" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopRemoteAccessSession where
        toJSON StopRemoteAccessSession'{..}
          = object (catMaybes [Just ("arn" .= _srasArn)])

instance ToPath StopRemoteAccessSession where
        toPath = const "/"

instance ToQuery StopRemoteAccessSession where
        toQuery = const mempty

-- | Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.
--
--
--
-- /See:/ 'stopRemoteAccessSessionResponse' smart constructor.
data StopRemoteAccessSessionResponse = StopRemoteAccessSessionResponse'
  { _srasrsRemoteAccessSession :: !(Maybe RemoteAccessSession)
  , _srasrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srasrsRemoteAccessSession' - A container representing the metadata from the service about the remote access session you are stopping.
--
-- * 'srasrsResponseStatus' - -- | The response status code.
stopRemoteAccessSessionResponse
    :: Int -- ^ 'srasrsResponseStatus'
    -> StopRemoteAccessSessionResponse
stopRemoteAccessSessionResponse pResponseStatus_ =
  StopRemoteAccessSessionResponse'
    { _srasrsRemoteAccessSession = Nothing
    , _srasrsResponseStatus = pResponseStatus_
    }


-- | A container representing the metadata from the service about the remote access session you are stopping.
srasrsRemoteAccessSession :: Lens' StopRemoteAccessSessionResponse (Maybe RemoteAccessSession)
srasrsRemoteAccessSession = lens _srasrsRemoteAccessSession (\ s a -> s{_srasrsRemoteAccessSession = a})

-- | -- | The response status code.
srasrsResponseStatus :: Lens' StopRemoteAccessSessionResponse Int
srasrsResponseStatus = lens _srasrsResponseStatus (\ s a -> s{_srasrsResponseStatus = a})

instance NFData StopRemoteAccessSessionResponse where

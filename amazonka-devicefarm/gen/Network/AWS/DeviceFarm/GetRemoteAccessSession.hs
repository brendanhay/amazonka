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
-- Module      : Network.AWS.DeviceFarm.GetRemoteAccessSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to a currently running remote access session.
--
--
module Network.AWS.DeviceFarm.GetRemoteAccessSession
    (
    -- * Creating a Request
      getRemoteAccessSession
    , GetRemoteAccessSession
    -- * Request Lenses
    , grasArn

    -- * Destructuring the Response
    , getRemoteAccessSessionResponse
    , GetRemoteAccessSessionResponse
    -- * Response Lenses
    , grasrsRemoteAccessSession
    , grasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get information about the specified remote access session.
--
--
--
-- /See:/ 'getRemoteAccessSession' smart constructor.
newtype GetRemoteAccessSession = GetRemoteAccessSession'
  { _grasArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grasArn' - The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
getRemoteAccessSession
    :: Text -- ^ 'grasArn'
    -> GetRemoteAccessSession
getRemoteAccessSession pArn_ = GetRemoteAccessSession' {_grasArn = pArn_}


-- | The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
grasArn :: Lens' GetRemoteAccessSession Text
grasArn = lens _grasArn (\ s a -> s{_grasArn = a})

instance AWSRequest GetRemoteAccessSession where
        type Rs GetRemoteAccessSession =
             GetRemoteAccessSessionResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetRemoteAccessSessionResponse' <$>
                   (x .?> "remoteAccessSession") <*>
                     (pure (fromEnum s)))

instance Hashable GetRemoteAccessSession where

instance NFData GetRemoteAccessSession where

instance ToHeaders GetRemoteAccessSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetRemoteAccessSession" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRemoteAccessSession where
        toJSON GetRemoteAccessSession'{..}
          = object (catMaybes [Just ("arn" .= _grasArn)])

instance ToPath GetRemoteAccessSession where
        toPath = const "/"

instance ToQuery GetRemoteAccessSession where
        toQuery = const mempty

-- | Represents the response from the server that lists detailed information about the remote access session.
--
--
--
-- /See:/ 'getRemoteAccessSessionResponse' smart constructor.
data GetRemoteAccessSessionResponse = GetRemoteAccessSessionResponse'
  { _grasrsRemoteAccessSession :: !(Maybe RemoteAccessSession)
  , _grasrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grasrsRemoteAccessSession' - A container that lists detailed information about the remote access session.
--
-- * 'grasrsResponseStatus' - -- | The response status code.
getRemoteAccessSessionResponse
    :: Int -- ^ 'grasrsResponseStatus'
    -> GetRemoteAccessSessionResponse
getRemoteAccessSessionResponse pResponseStatus_ =
  GetRemoteAccessSessionResponse'
    { _grasrsRemoteAccessSession = Nothing
    , _grasrsResponseStatus = pResponseStatus_
    }


-- | A container that lists detailed information about the remote access session.
grasrsRemoteAccessSession :: Lens' GetRemoteAccessSessionResponse (Maybe RemoteAccessSession)
grasrsRemoteAccessSession = lens _grasrsRemoteAccessSession (\ s a -> s{_grasrsRemoteAccessSession = a})

-- | -- | The response status code.
grasrsResponseStatus :: Lens' GetRemoteAccessSessionResponse Int
grasrsResponseStatus = lens _grasrsResponseStatus (\ s a -> s{_grasrsResponseStatus = a})

instance NFData GetRemoteAccessSessionResponse where

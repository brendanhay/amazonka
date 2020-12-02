{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network profile details by the network profile ARN.
module Network.AWS.AlexaBusiness.GetNetworkProfile
  ( -- * Creating a Request
    getNetworkProfile,
    GetNetworkProfile,

    -- * Request Lenses
    gnpNetworkProfileARN,

    -- * Destructuring the Response
    getNetworkProfileResponse,
    GetNetworkProfileResponse,

    -- * Response Lenses
    gnprsNetworkProfile,
    gnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getNetworkProfile' smart constructor.
newtype GetNetworkProfile = GetNetworkProfile'
  { _gnpNetworkProfileARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnpNetworkProfileARN' - The ARN of the network profile associated with a device.
getNetworkProfile ::
  -- | 'gnpNetworkProfileARN'
  Text ->
  GetNetworkProfile
getNetworkProfile pNetworkProfileARN_ =
  GetNetworkProfile' {_gnpNetworkProfileARN = pNetworkProfileARN_}

-- | The ARN of the network profile associated with a device.
gnpNetworkProfileARN :: Lens' GetNetworkProfile Text
gnpNetworkProfileARN = lens _gnpNetworkProfileARN (\s a -> s {_gnpNetworkProfileARN = a})

instance AWSRequest GetNetworkProfile where
  type Rs GetNetworkProfile = GetNetworkProfileResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          GetNetworkProfileResponse'
            <$> (x .?> "NetworkProfile") <*> (pure (fromEnum s))
      )

instance Hashable GetNetworkProfile

instance NFData GetNetworkProfile

instance ToHeaders GetNetworkProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.GetNetworkProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetNetworkProfile where
  toJSON GetNetworkProfile' {..} =
    object
      (catMaybes [Just ("NetworkProfileArn" .= _gnpNetworkProfileARN)])

instance ToPath GetNetworkProfile where
  toPath = const "/"

instance ToQuery GetNetworkProfile where
  toQuery = const mempty

-- | /See:/ 'getNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { _gnprsNetworkProfile ::
      !(Maybe NetworkProfile),
    _gnprsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnprsNetworkProfile' - The network profile associated with a device.
--
-- * 'gnprsResponseStatus' - -- | The response status code.
getNetworkProfileResponse ::
  -- | 'gnprsResponseStatus'
  Int ->
  GetNetworkProfileResponse
getNetworkProfileResponse pResponseStatus_ =
  GetNetworkProfileResponse'
    { _gnprsNetworkProfile = Nothing,
      _gnprsResponseStatus = pResponseStatus_
    }

-- | The network profile associated with a device.
gnprsNetworkProfile :: Lens' GetNetworkProfileResponse (Maybe NetworkProfile)
gnprsNetworkProfile = lens _gnprsNetworkProfile (\s a -> s {_gnprsNetworkProfile = a})

-- | -- | The response status code.
gnprsResponseStatus :: Lens' GetNetworkProfileResponse Int
gnprsResponseStatus = lens _gnprsResponseStatus (\s a -> s {_gnprsResponseStatus = a})

instance NFData GetNetworkProfileResponse

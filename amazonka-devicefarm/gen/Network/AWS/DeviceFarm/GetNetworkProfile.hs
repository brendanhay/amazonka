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
-- Module      : Network.AWS.DeviceFarm.GetNetworkProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a network profile.
--
--
module Network.AWS.DeviceFarm.GetNetworkProfile
    (
    -- * Creating a Request
      getNetworkProfile
    , GetNetworkProfile
    -- * Request Lenses
    , gnpArn

    -- * Destructuring the Response
    , getNetworkProfileResponse
    , GetNetworkProfileResponse
    -- * Response Lenses
    , gnprsNetworkProfile
    , gnprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getNetworkProfile' smart constructor.
newtype GetNetworkProfile = GetNetworkProfile'
  { _gnpArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnpArn' - The Amazon Resource Name (ARN) of the network profile you want to return information about.
getNetworkProfile
    :: Text -- ^ 'gnpArn'
    -> GetNetworkProfile
getNetworkProfile pArn_ = GetNetworkProfile' {_gnpArn = pArn_}


-- | The Amazon Resource Name (ARN) of the network profile you want to return information about.
gnpArn :: Lens' GetNetworkProfile Text
gnpArn = lens _gnpArn (\ s a -> s{_gnpArn = a})

instance AWSRequest GetNetworkProfile where
        type Rs GetNetworkProfile = GetNetworkProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetNetworkProfileResponse' <$>
                   (x .?> "networkProfile") <*> (pure (fromEnum s)))

instance Hashable GetNetworkProfile where

instance NFData GetNetworkProfile where

instance ToHeaders GetNetworkProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetNetworkProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetNetworkProfile where
        toJSON GetNetworkProfile'{..}
          = object (catMaybes [Just ("arn" .= _gnpArn)])

instance ToPath GetNetworkProfile where
        toPath = const "/"

instance ToQuery GetNetworkProfile where
        toQuery = const mempty

-- | /See:/ 'getNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { _gnprsNetworkProfile :: !(Maybe NetworkProfile)
  , _gnprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnprsNetworkProfile' - The network profile.
--
-- * 'gnprsResponseStatus' - -- | The response status code.
getNetworkProfileResponse
    :: Int -- ^ 'gnprsResponseStatus'
    -> GetNetworkProfileResponse
getNetworkProfileResponse pResponseStatus_ =
  GetNetworkProfileResponse'
    {_gnprsNetworkProfile = Nothing, _gnprsResponseStatus = pResponseStatus_}


-- | The network profile.
gnprsNetworkProfile :: Lens' GetNetworkProfileResponse (Maybe NetworkProfile)
gnprsNetworkProfile = lens _gnprsNetworkProfile (\ s a -> s{_gnprsNetworkProfile = a})

-- | -- | The response status code.
gnprsResponseStatus :: Lens' GetNetworkProfileResponse Int
gnprsResponseStatus = lens _gnprsResponseStatus (\ s a -> s{_gnprsResponseStatus = a})

instance NFData GetNetworkProfileResponse where

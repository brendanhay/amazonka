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
-- Module      : Network.AWS.DeviceFarm.GetInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified instance profile.
--
--
module Network.AWS.DeviceFarm.GetInstanceProfile
    (
    -- * Creating a Request
      getInstanceProfile
    , GetInstanceProfile
    -- * Request Lenses
    , gipArn

    -- * Destructuring the Response
    , getInstanceProfileResponse
    , GetInstanceProfileResponse
    -- * Response Lenses
    , giprsInstanceProfile
    , giprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile'
  { _gipArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipArn' - The Amazon Resource Name (ARN) of your instance profile.
getInstanceProfile
    :: Text -- ^ 'gipArn'
    -> GetInstanceProfile
getInstanceProfile pArn_ = GetInstanceProfile' {_gipArn = pArn_}


-- | The Amazon Resource Name (ARN) of your instance profile.
gipArn :: Lens' GetInstanceProfile Text
gipArn = lens _gipArn (\ s a -> s{_gipArn = a})

instance AWSRequest GetInstanceProfile where
        type Rs GetInstanceProfile =
             GetInstanceProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceProfileResponse' <$>
                   (x .?> "instanceProfile") <*> (pure (fromEnum s)))

instance Hashable GetInstanceProfile where

instance NFData GetInstanceProfile where

instance ToHeaders GetInstanceProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetInstanceProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceProfile where
        toJSON GetInstanceProfile'{..}
          = object (catMaybes [Just ("arn" .= _gipArn)])

instance ToPath GetInstanceProfile where
        toPath = const "/"

instance ToQuery GetInstanceProfile where
        toQuery = const mempty

-- | /See:/ 'getInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { _giprsInstanceProfile :: !(Maybe InstanceProfile)
  , _giprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giprsInstanceProfile' - An object containing information about your instance profile.
--
-- * 'giprsResponseStatus' - -- | The response status code.
getInstanceProfileResponse
    :: Int -- ^ 'giprsResponseStatus'
    -> GetInstanceProfileResponse
getInstanceProfileResponse pResponseStatus_ =
  GetInstanceProfileResponse'
    {_giprsInstanceProfile = Nothing, _giprsResponseStatus = pResponseStatus_}


-- | An object containing information about your instance profile.
giprsInstanceProfile :: Lens' GetInstanceProfileResponse (Maybe InstanceProfile)
giprsInstanceProfile = lens _giprsInstanceProfile (\ s a -> s{_giprsInstanceProfile = a})

-- | -- | The response status code.
giprsResponseStatus :: Lens' GetInstanceProfileResponse Int
giprsResponseStatus = lens _giprsResponseStatus (\ s a -> s{_giprsResponseStatus = a})

instance NFData GetInstanceProfileResponse where

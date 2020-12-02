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
-- Module      : Network.AWS.AlexaBusiness.GetProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a room profile by profile ARN.
--
--
module Network.AWS.AlexaBusiness.GetProfile
    (
    -- * Creating a Request
      getProfile
    , GetProfile
    -- * Request Lenses
    , gpProfileARN

    -- * Destructuring the Response
    , getProfileResponse
    , GetProfileResponse
    -- * Response Lenses
    , gprsProfile
    , gprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getProfile' smart constructor.
newtype GetProfile = GetProfile'
  { _gpProfileARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpProfileARN' - The ARN of the room profile for which to request details. Required.
getProfile
    :: GetProfile
getProfile = GetProfile' {_gpProfileARN = Nothing}


-- | The ARN of the room profile for which to request details. Required.
gpProfileARN :: Lens' GetProfile (Maybe Text)
gpProfileARN = lens _gpProfileARN (\ s a -> s{_gpProfileARN = a})

instance AWSRequest GetProfile where
        type Rs GetProfile = GetProfileResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetProfileResponse' <$>
                   (x .?> "Profile") <*> (pure (fromEnum s)))

instance Hashable GetProfile where

instance NFData GetProfile where

instance ToHeaders GetProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetProfile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetProfile where
        toJSON GetProfile'{..}
          = object
              (catMaybes [("ProfileArn" .=) <$> _gpProfileARN])

instance ToPath GetProfile where
        toPath = const "/"

instance ToQuery GetProfile where
        toQuery = const mempty

-- | /See:/ 'getProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { _gprsProfile        :: !(Maybe Profile)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsProfile' - The details of the room profile requested. Required.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getProfileResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetProfileResponse
getProfileResponse pResponseStatus_ =
  GetProfileResponse'
    {_gprsProfile = Nothing, _gprsResponseStatus = pResponseStatus_}


-- | The details of the room profile requested. Required.
gprsProfile :: Lens' GetProfileResponse (Maybe Profile)
gprsProfile = lens _gprsProfile (\ s a -> s{_gprsProfile = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetProfileResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetProfileResponse where

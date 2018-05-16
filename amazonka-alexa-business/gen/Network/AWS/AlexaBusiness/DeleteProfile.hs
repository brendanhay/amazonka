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
-- Module      : Network.AWS.AlexaBusiness.DeleteProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room profile by the profile ARN.
--
--
module Network.AWS.AlexaBusiness.DeleteProfile
    (
    -- * Creating a Request
      deleteProfile
    , DeleteProfile
    -- * Request Lenses
    , dpProfileARN

    -- * Destructuring the Response
    , deleteProfileResponse
    , DeleteProfileResponse
    -- * Response Lenses
    , dprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProfile' smart constructor.
newtype DeleteProfile = DeleteProfile'
  { _dpProfileARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpProfileARN' - The ARN of the room profile to delete. Required.
deleteProfile
    :: DeleteProfile
deleteProfile = DeleteProfile' {_dpProfileARN = Nothing}


-- | The ARN of the room profile to delete. Required.
dpProfileARN :: Lens' DeleteProfile (Maybe Text)
dpProfileARN = lens _dpProfileARN (\ s a -> s{_dpProfileARN = a})

instance AWSRequest DeleteProfile where
        type Rs DeleteProfile = DeleteProfileResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProfileResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteProfile where

instance NFData DeleteProfile where

instance ToHeaders DeleteProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteProfile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProfile where
        toJSON DeleteProfile'{..}
          = object
              (catMaybes [("ProfileArn" .=) <$> _dpProfileARN])

instance ToPath DeleteProfile where
        toPath = const "/"

instance ToQuery DeleteProfile where
        toQuery = const mempty

-- | /See:/ 'deleteProfileResponse' smart constructor.
newtype DeleteProfileResponse = DeleteProfileResponse'
  { _dprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsResponseStatus' - -- | The response status code.
deleteProfileResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DeleteProfileResponse
deleteProfileResponse pResponseStatus_ =
  DeleteProfileResponse' {_dprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dprsResponseStatus :: Lens' DeleteProfileResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DeleteProfileResponse where

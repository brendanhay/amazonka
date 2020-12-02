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
-- Module      : Network.AWS.SageMaker.UpdateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user profile.
module Network.AWS.SageMaker.UpdateUserProfile
  ( -- * Creating a Request
    updateUserProfile,
    UpdateUserProfile,

    -- * Request Lenses
    uupUserSettings,
    uupDomainId,
    uupUserProfileName,

    -- * Destructuring the Response
    updateUserProfileResponse,
    UpdateUserProfileResponse,

    -- * Response Lenses
    uuprsUserProfileARN,
    uuprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { _uupUserSettings ::
      !(Maybe UserSettings),
    _uupDomainId :: !Text,
    _uupUserProfileName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupUserSettings' - A collection of settings.
--
-- * 'uupDomainId' - The domain ID.
--
-- * 'uupUserProfileName' - The user profile name.
updateUserProfile ::
  -- | 'uupDomainId'
  Text ->
  -- | 'uupUserProfileName'
  Text ->
  UpdateUserProfile
updateUserProfile pDomainId_ pUserProfileName_ =
  UpdateUserProfile'
    { _uupUserSettings = Nothing,
      _uupDomainId = pDomainId_,
      _uupUserProfileName = pUserProfileName_
    }

-- | A collection of settings.
uupUserSettings :: Lens' UpdateUserProfile (Maybe UserSettings)
uupUserSettings = lens _uupUserSettings (\s a -> s {_uupUserSettings = a})

-- | The domain ID.
uupDomainId :: Lens' UpdateUserProfile Text
uupDomainId = lens _uupDomainId (\s a -> s {_uupDomainId = a})

-- | The user profile name.
uupUserProfileName :: Lens' UpdateUserProfile Text
uupUserProfileName = lens _uupUserProfileName (\s a -> s {_uupUserProfileName = a})

instance AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            <$> (x .?> "UserProfileArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateUserProfile

instance NFData UpdateUserProfile

instance ToHeaders UpdateUserProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.UpdateUserProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    object
      ( catMaybes
          [ ("UserSettings" .=) <$> _uupUserSettings,
            Just ("DomainId" .= _uupDomainId),
            Just ("UserProfileName" .= _uupUserProfileName)
          ]
      )

instance ToPath UpdateUserProfile where
  toPath = const "/"

instance ToQuery UpdateUserProfile where
  toQuery = const mempty

-- | /See:/ 'updateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { _uuprsUserProfileARN ::
      !(Maybe Text),
    _uuprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuprsUserProfileARN' - The user profile Amazon Resource Name (ARN).
--
-- * 'uuprsResponseStatus' - -- | The response status code.
updateUserProfileResponse ::
  -- | 'uuprsResponseStatus'
  Int ->
  UpdateUserProfileResponse
updateUserProfileResponse pResponseStatus_ =
  UpdateUserProfileResponse'
    { _uuprsUserProfileARN = Nothing,
      _uuprsResponseStatus = pResponseStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
uuprsUserProfileARN :: Lens' UpdateUserProfileResponse (Maybe Text)
uuprsUserProfileARN = lens _uuprsUserProfileARN (\s a -> s {_uuprsUserProfileARN = a})

-- | -- | The response status code.
uuprsResponseStatus :: Lens' UpdateUserProfileResponse Int
uuprsResponseStatus = lens _uuprsResponseStatus (\s a -> s {_uuprsResponseStatus = a})

instance NFData UpdateUserProfileResponse

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
-- Module      : Network.AWS.SageMaker.DeleteUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile. When a user profile is deleted, the user loses access to their EFS volume, including data, notebooks, and other artifacts.
module Network.AWS.SageMaker.DeleteUserProfile
  ( -- * Creating a Request
    deleteUserProfile,
    DeleteUserProfile,

    -- * Request Lenses
    delDomainId,
    delUserProfileName,

    -- * Destructuring the Response
    deleteUserProfileResponse,
    DeleteUserProfileResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteUserProfile' smart constructor.
data DeleteUserProfile = DeleteUserProfile'
  { _delDomainId :: !Text,
    _delUserProfileName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDomainId' - The domain ID.
--
-- * 'delUserProfileName' - The user profile name.
deleteUserProfile ::
  -- | 'delDomainId'
  Text ->
  -- | 'delUserProfileName'
  Text ->
  DeleteUserProfile
deleteUserProfile pDomainId_ pUserProfileName_ =
  DeleteUserProfile'
    { _delDomainId = pDomainId_,
      _delUserProfileName = pUserProfileName_
    }

-- | The domain ID.
delDomainId :: Lens' DeleteUserProfile Text
delDomainId = lens _delDomainId (\s a -> s {_delDomainId = a})

-- | The user profile name.
delUserProfileName :: Lens' DeleteUserProfile Text
delUserProfileName = lens _delUserProfileName (\s a -> s {_delUserProfileName = a})

instance AWSRequest DeleteUserProfile where
  type Rs DeleteUserProfile = DeleteUserProfileResponse
  request = postJSON sageMaker
  response = receiveNull DeleteUserProfileResponse'

instance Hashable DeleteUserProfile

instance NFData DeleteUserProfile

instance ToHeaders DeleteUserProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteUserProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteUserProfile where
  toJSON DeleteUserProfile' {..} =
    object
      ( catMaybes
          [ Just ("DomainId" .= _delDomainId),
            Just ("UserProfileName" .= _delUserProfileName)
          ]
      )

instance ToPath DeleteUserProfile where
  toPath = const "/"

instance ToQuery DeleteUserProfile where
  toQuery = const mempty

-- | /See:/ 'deleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserProfileResponse' with the minimum fields required to make a request.
deleteUserProfileResponse ::
  DeleteUserProfileResponse
deleteUserProfileResponse = DeleteUserProfileResponse'

instance NFData DeleteUserProfileResponse

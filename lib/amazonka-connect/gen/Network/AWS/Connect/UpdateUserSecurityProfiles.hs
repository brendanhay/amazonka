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
-- Module      : Network.AWS.Connect.UpdateUserSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified security profiles to the specified user.
module Network.AWS.Connect.UpdateUserSecurityProfiles
  ( -- * Creating a Request
    updateUserSecurityProfiles,
    UpdateUserSecurityProfiles,

    -- * Request Lenses
    uuspSecurityProfileIds,
    uuspUserId,
    uuspInstanceId,

    -- * Destructuring the Response
    updateUserSecurityProfilesResponse,
    UpdateUserSecurityProfilesResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { _uuspSecurityProfileIds ::
      !(List1 Text),
    _uuspUserId :: !Text,
    _uuspInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserSecurityProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuspSecurityProfileIds' - The identifiers of the security profiles for the user.
--
-- * 'uuspUserId' - The identifier of the user account.
--
-- * 'uuspInstanceId' - The identifier of the Amazon Connect instance.
updateUserSecurityProfiles ::
  -- | 'uuspSecurityProfileIds'
  NonEmpty Text ->
  -- | 'uuspUserId'
  Text ->
  -- | 'uuspInstanceId'
  Text ->
  UpdateUserSecurityProfiles
updateUserSecurityProfiles
  pSecurityProfileIds_
  pUserId_
  pInstanceId_ =
    UpdateUserSecurityProfiles'
      { _uuspSecurityProfileIds =
          _List1 # pSecurityProfileIds_,
        _uuspUserId = pUserId_,
        _uuspInstanceId = pInstanceId_
      }

-- | The identifiers of the security profiles for the user.
uuspSecurityProfileIds :: Lens' UpdateUserSecurityProfiles (NonEmpty Text)
uuspSecurityProfileIds = lens _uuspSecurityProfileIds (\s a -> s {_uuspSecurityProfileIds = a}) . _List1

-- | The identifier of the user account.
uuspUserId :: Lens' UpdateUserSecurityProfiles Text
uuspUserId = lens _uuspUserId (\s a -> s {_uuspUserId = a})

-- | The identifier of the Amazon Connect instance.
uuspInstanceId :: Lens' UpdateUserSecurityProfiles Text
uuspInstanceId = lens _uuspInstanceId (\s a -> s {_uuspInstanceId = a})

instance AWSRequest UpdateUserSecurityProfiles where
  type
    Rs UpdateUserSecurityProfiles =
      UpdateUserSecurityProfilesResponse
  request = postJSON connect
  response = receiveNull UpdateUserSecurityProfilesResponse'

instance Hashable UpdateUserSecurityProfiles

instance NFData UpdateUserSecurityProfiles

instance ToHeaders UpdateUserSecurityProfiles where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserSecurityProfiles where
  toJSON UpdateUserSecurityProfiles' {..} =
    object
      ( catMaybes
          [Just ("SecurityProfileIds" .= _uuspSecurityProfileIds)]
      )

instance ToPath UpdateUserSecurityProfiles where
  toPath UpdateUserSecurityProfiles' {..} =
    mconcat
      [ "/users/",
        toBS _uuspInstanceId,
        "/",
        toBS _uuspUserId,
        "/security-profiles"
      ]

instance ToQuery UpdateUserSecurityProfiles where
  toQuery = const mempty

-- | /See:/ 'updateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserSecurityProfilesResponse' with the minimum fields required to make a request.
updateUserSecurityProfilesResponse ::
  UpdateUserSecurityProfilesResponse
updateUserSecurityProfilesResponse =
  UpdateUserSecurityProfilesResponse'

instance NFData UpdateUserSecurityProfilesResponse

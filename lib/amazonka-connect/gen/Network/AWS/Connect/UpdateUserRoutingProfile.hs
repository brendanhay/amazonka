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
-- Module      : Network.AWS.Connect.UpdateUserRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified routing profile to the specified user.
module Network.AWS.Connect.UpdateUserRoutingProfile
  ( -- * Creating a Request
    updateUserRoutingProfile,
    UpdateUserRoutingProfile,

    -- * Request Lenses
    uurpRoutingProfileId,
    uurpUserId,
    uurpInstanceId,

    -- * Destructuring the Response
    updateUserRoutingProfileResponse,
    UpdateUserRoutingProfileResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { _uurpRoutingProfileId ::
      !Text,
    _uurpUserId :: !Text,
    _uurpInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserRoutingProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uurpRoutingProfileId' - The identifier of the routing profile for the user.
--
-- * 'uurpUserId' - The identifier of the user account.
--
-- * 'uurpInstanceId' - The identifier of the Amazon Connect instance.
updateUserRoutingProfile ::
  -- | 'uurpRoutingProfileId'
  Text ->
  -- | 'uurpUserId'
  Text ->
  -- | 'uurpInstanceId'
  Text ->
  UpdateUserRoutingProfile
updateUserRoutingProfile pRoutingProfileId_ pUserId_ pInstanceId_ =
  UpdateUserRoutingProfile'
    { _uurpRoutingProfileId =
        pRoutingProfileId_,
      _uurpUserId = pUserId_,
      _uurpInstanceId = pInstanceId_
    }

-- | The identifier of the routing profile for the user.
uurpRoutingProfileId :: Lens' UpdateUserRoutingProfile Text
uurpRoutingProfileId = lens _uurpRoutingProfileId (\s a -> s {_uurpRoutingProfileId = a})

-- | The identifier of the user account.
uurpUserId :: Lens' UpdateUserRoutingProfile Text
uurpUserId = lens _uurpUserId (\s a -> s {_uurpUserId = a})

-- | The identifier of the Amazon Connect instance.
uurpInstanceId :: Lens' UpdateUserRoutingProfile Text
uurpInstanceId = lens _uurpInstanceId (\s a -> s {_uurpInstanceId = a})

instance AWSRequest UpdateUserRoutingProfile where
  type Rs UpdateUserRoutingProfile = UpdateUserRoutingProfileResponse
  request = postJSON connect
  response = receiveNull UpdateUserRoutingProfileResponse'

instance Hashable UpdateUserRoutingProfile

instance NFData UpdateUserRoutingProfile

instance ToHeaders UpdateUserRoutingProfile where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserRoutingProfile where
  toJSON UpdateUserRoutingProfile' {..} =
    object
      (catMaybes [Just ("RoutingProfileId" .= _uurpRoutingProfileId)])

instance ToPath UpdateUserRoutingProfile where
  toPath UpdateUserRoutingProfile' {..} =
    mconcat
      [ "/users/",
        toBS _uurpInstanceId,
        "/",
        toBS _uurpUserId,
        "/routing-profile"
      ]

instance ToQuery UpdateUserRoutingProfile where
  toQuery = const mempty

-- | /See:/ 'updateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserRoutingProfileResponse' with the minimum fields required to make a request.
updateUserRoutingProfileResponse ::
  UpdateUserRoutingProfileResponse
updateUserRoutingProfileResponse =
  UpdateUserRoutingProfileResponse'

instance NFData UpdateUserRoutingProfileResponse

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
-- Module      : Network.AWS.Connect.UpdateUserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity information for the specified user.
--
--
-- /Important:/ Someone with the ability to invoke @UpdateUserIndentityInfo@ can change the login credentials of other users by changing their email address. This poses a security risk to your organization. They can change the email address of a user to the attacker's email address, and then reset the password through email. We strongly recommend limiting who has the ability to invoke @UpdateUserIndentityInfo@ . For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-best-practices.html Best Practices for Security Profiles> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.UpdateUserIdentityInfo
  ( -- * Creating a Request
    updateUserIdentityInfo,
    UpdateUserIdentityInfo,

    -- * Request Lenses
    uuiiIdentityInfo,
    uuiiUserId,
    uuiiInstanceId,

    -- * Destructuring the Response
    updateUserIdentityInfoResponse,
    UpdateUserIdentityInfoResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserIdentityInfo' smart constructor.
data UpdateUserIdentityInfo = UpdateUserIdentityInfo'
  { _uuiiIdentityInfo ::
      !UserIdentityInfo,
    _uuiiUserId :: !Text,
    _uuiiInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserIdentityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuiiIdentityInfo' - The identity information for the user.
--
-- * 'uuiiUserId' - The identifier of the user account.
--
-- * 'uuiiInstanceId' - The identifier of the Amazon Connect instance.
updateUserIdentityInfo ::
  -- | 'uuiiIdentityInfo'
  UserIdentityInfo ->
  -- | 'uuiiUserId'
  Text ->
  -- | 'uuiiInstanceId'
  Text ->
  UpdateUserIdentityInfo
updateUserIdentityInfo pIdentityInfo_ pUserId_ pInstanceId_ =
  UpdateUserIdentityInfo'
    { _uuiiIdentityInfo = pIdentityInfo_,
      _uuiiUserId = pUserId_,
      _uuiiInstanceId = pInstanceId_
    }

-- | The identity information for the user.
uuiiIdentityInfo :: Lens' UpdateUserIdentityInfo UserIdentityInfo
uuiiIdentityInfo = lens _uuiiIdentityInfo (\s a -> s {_uuiiIdentityInfo = a})

-- | The identifier of the user account.
uuiiUserId :: Lens' UpdateUserIdentityInfo Text
uuiiUserId = lens _uuiiUserId (\s a -> s {_uuiiUserId = a})

-- | The identifier of the Amazon Connect instance.
uuiiInstanceId :: Lens' UpdateUserIdentityInfo Text
uuiiInstanceId = lens _uuiiInstanceId (\s a -> s {_uuiiInstanceId = a})

instance AWSRequest UpdateUserIdentityInfo where
  type Rs UpdateUserIdentityInfo = UpdateUserIdentityInfoResponse
  request = postJSON connect
  response = receiveNull UpdateUserIdentityInfoResponse'

instance Hashable UpdateUserIdentityInfo

instance NFData UpdateUserIdentityInfo

instance ToHeaders UpdateUserIdentityInfo where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserIdentityInfo where
  toJSON UpdateUserIdentityInfo' {..} =
    object (catMaybes [Just ("IdentityInfo" .= _uuiiIdentityInfo)])

instance ToPath UpdateUserIdentityInfo where
  toPath UpdateUserIdentityInfo' {..} =
    mconcat
      [ "/users/",
        toBS _uuiiInstanceId,
        "/",
        toBS _uuiiUserId,
        "/identity-info"
      ]

instance ToQuery UpdateUserIdentityInfo where
  toQuery = const mempty

-- | /See:/ 'updateUserIdentityInfoResponse' smart constructor.
data UpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserIdentityInfoResponse' with the minimum fields required to make a request.
updateUserIdentityInfoResponse ::
  UpdateUserIdentityInfoResponse
updateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'

instance NFData UpdateUserIdentityInfoResponse

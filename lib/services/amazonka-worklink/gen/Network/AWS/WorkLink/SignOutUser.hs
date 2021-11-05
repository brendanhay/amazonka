{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkLink.SignOutUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs the user out from all of their devices. The user can sign in again
-- if they have valid credentials.
module Network.AWS.WorkLink.SignOutUser
  ( -- * Creating a Request
    SignOutUser (..),
    newSignOutUser,

    -- * Request Lenses
    signOutUser_fleetArn,
    signOutUser_username,

    -- * Destructuring the Response
    SignOutUserResponse (..),
    newSignOutUserResponse,

    -- * Response Lenses
    signOutUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newSignOutUser' smart constructor.
data SignOutUser = SignOutUser'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignOutUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'signOutUser_fleetArn' - The ARN of the fleet.
--
-- 'username', 'signOutUser_username' - The name of the user.
newSignOutUser ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  SignOutUser
newSignOutUser pFleetArn_ pUsername_ =
  SignOutUser'
    { fleetArn = pFleetArn_,
      username = pUsername_
    }

-- | The ARN of the fleet.
signOutUser_fleetArn :: Lens.Lens' SignOutUser Prelude.Text
signOutUser_fleetArn = Lens.lens (\SignOutUser' {fleetArn} -> fleetArn) (\s@SignOutUser' {} a -> s {fleetArn = a} :: SignOutUser)

-- | The name of the user.
signOutUser_username :: Lens.Lens' SignOutUser Prelude.Text
signOutUser_username = Lens.lens (\SignOutUser' {username} -> username) (\s@SignOutUser' {} a -> s {username = a} :: SignOutUser)

instance Core.AWSRequest SignOutUser where
  type AWSResponse SignOutUser = SignOutUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SignOutUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SignOutUser

instance Prelude.NFData SignOutUser

instance Core.ToHeaders SignOutUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SignOutUser where
  toJSON SignOutUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath SignOutUser where
  toPath = Prelude.const "/signOutUser"

instance Core.ToQuery SignOutUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignOutUserResponse' smart constructor.
data SignOutUserResponse = SignOutUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignOutUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'signOutUserResponse_httpStatus' - The response's http status code.
newSignOutUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SignOutUserResponse
newSignOutUserResponse pHttpStatus_ =
  SignOutUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
signOutUserResponse_httpStatus :: Lens.Lens' SignOutUserResponse Prelude.Int
signOutUserResponse_httpStatus = Lens.lens (\SignOutUserResponse' {httpStatus} -> httpStatus) (\s@SignOutUserResponse' {} a -> s {httpStatus = a} :: SignOutUserResponse)

instance Prelude.NFData SignOutUserResponse

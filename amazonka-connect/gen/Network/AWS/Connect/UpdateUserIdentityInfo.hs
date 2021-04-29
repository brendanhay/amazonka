{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.UpdateUserIdentityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity information for the specified user.
--
-- We strongly recommend limiting who has the ability to invoke
-- @UpdateUserIdentityInfo@. Someone with that ability can change the login
-- credentials of other users by changing their email address. This poses a
-- security risk to your organization. They can change the email address of
-- a user to the attacker\'s email address, and then reset the password
-- through email. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-best-practices.html Best Practices for Security Profiles>
-- in the /Amazon Connect Administrator Guide/.
module Network.AWS.Connect.UpdateUserIdentityInfo
  ( -- * Creating a Request
    UpdateUserIdentityInfo (..),
    newUpdateUserIdentityInfo,

    -- * Request Lenses
    updateUserIdentityInfo_identityInfo,
    updateUserIdentityInfo_userId,
    updateUserIdentityInfo_instanceId,

    -- * Destructuring the Response
    UpdateUserIdentityInfoResponse (..),
    newUpdateUserIdentityInfoResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserIdentityInfo' smart constructor.
data UpdateUserIdentityInfo = UpdateUserIdentityInfo'
  { -- | The identity information for the user.
    identityInfo :: UserIdentityInfo,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserIdentityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityInfo', 'updateUserIdentityInfo_identityInfo' - The identity information for the user.
--
-- 'userId', 'updateUserIdentityInfo_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserIdentityInfo_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserIdentityInfo ::
  -- | 'identityInfo'
  UserIdentityInfo ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserIdentityInfo
newUpdateUserIdentityInfo
  pIdentityInfo_
  pUserId_
  pInstanceId_ =
    UpdateUserIdentityInfo'
      { identityInfo =
          pIdentityInfo_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | The identity information for the user.
updateUserIdentityInfo_identityInfo :: Lens.Lens' UpdateUserIdentityInfo UserIdentityInfo
updateUserIdentityInfo_identityInfo = Lens.lens (\UpdateUserIdentityInfo' {identityInfo} -> identityInfo) (\s@UpdateUserIdentityInfo' {} a -> s {identityInfo = a} :: UpdateUserIdentityInfo)

-- | The identifier of the user account.
updateUserIdentityInfo_userId :: Lens.Lens' UpdateUserIdentityInfo Prelude.Text
updateUserIdentityInfo_userId = Lens.lens (\UpdateUserIdentityInfo' {userId} -> userId) (\s@UpdateUserIdentityInfo' {} a -> s {userId = a} :: UpdateUserIdentityInfo)

-- | The identifier of the Amazon Connect instance.
updateUserIdentityInfo_instanceId :: Lens.Lens' UpdateUserIdentityInfo Prelude.Text
updateUserIdentityInfo_instanceId = Lens.lens (\UpdateUserIdentityInfo' {instanceId} -> instanceId) (\s@UpdateUserIdentityInfo' {} a -> s {instanceId = a} :: UpdateUserIdentityInfo)

instance Prelude.AWSRequest UpdateUserIdentityInfo where
  type
    Rs UpdateUserIdentityInfo =
      UpdateUserIdentityInfoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateUserIdentityInfoResponse'

instance Prelude.Hashable UpdateUserIdentityInfo

instance Prelude.NFData UpdateUserIdentityInfo

instance Prelude.ToHeaders UpdateUserIdentityInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserIdentityInfo where
  toJSON UpdateUserIdentityInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityInfo" Prelude..= identityInfo)
          ]
      )

instance Prelude.ToPath UpdateUserIdentityInfo where
  toPath UpdateUserIdentityInfo' {..} =
    Prelude.mconcat
      [ "/users/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS userId,
        "/identity-info"
      ]

instance Prelude.ToQuery UpdateUserIdentityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserIdentityInfoResponse' smart constructor.
data UpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserIdentityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserIdentityInfoResponse ::
  UpdateUserIdentityInfoResponse
newUpdateUserIdentityInfoResponse =
  UpdateUserIdentityInfoResponse'

instance
  Prelude.NFData
    UpdateUserIdentityInfoResponse

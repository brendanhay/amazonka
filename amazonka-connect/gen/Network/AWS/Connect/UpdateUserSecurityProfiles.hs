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
-- Module      : Network.AWS.Connect.UpdateUserSecurityProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified security profiles to the specified user.
module Network.AWS.Connect.UpdateUserSecurityProfiles
  ( -- * Creating a Request
    UpdateUserSecurityProfiles (..),
    newUpdateUserSecurityProfiles,

    -- * Request Lenses
    updateUserSecurityProfiles_securityProfileIds,
    updateUserSecurityProfiles_userId,
    updateUserSecurityProfiles_instanceId,

    -- * Destructuring the Response
    UpdateUserSecurityProfilesResponse (..),
    newUpdateUserSecurityProfilesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Core.NonEmpty Core.Text,
    -- | The identifier of the user account.
    userId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileIds', 'updateUserSecurityProfiles_securityProfileIds' - The identifiers of the security profiles for the user.
--
-- 'userId', 'updateUserSecurityProfiles_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserSecurityProfiles_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserSecurityProfiles ::
  -- | 'securityProfileIds'
  Core.NonEmpty Core.Text ->
  -- | 'userId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  UpdateUserSecurityProfiles
newUpdateUserSecurityProfiles
  pSecurityProfileIds_
  pUserId_
  pInstanceId_ =
    UpdateUserSecurityProfiles'
      { securityProfileIds =
          Lens._Coerce Lens.# pSecurityProfileIds_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | The identifiers of the security profiles for the user.
updateUserSecurityProfiles_securityProfileIds :: Lens.Lens' UpdateUserSecurityProfiles (Core.NonEmpty Core.Text)
updateUserSecurityProfiles_securityProfileIds = Lens.lens (\UpdateUserSecurityProfiles' {securityProfileIds} -> securityProfileIds) (\s@UpdateUserSecurityProfiles' {} a -> s {securityProfileIds = a} :: UpdateUserSecurityProfiles) Core.. Lens._Coerce

-- | The identifier of the user account.
updateUserSecurityProfiles_userId :: Lens.Lens' UpdateUserSecurityProfiles Core.Text
updateUserSecurityProfiles_userId = Lens.lens (\UpdateUserSecurityProfiles' {userId} -> userId) (\s@UpdateUserSecurityProfiles' {} a -> s {userId = a} :: UpdateUserSecurityProfiles)

-- | The identifier of the Amazon Connect instance.
updateUserSecurityProfiles_instanceId :: Lens.Lens' UpdateUserSecurityProfiles Core.Text
updateUserSecurityProfiles_instanceId = Lens.lens (\UpdateUserSecurityProfiles' {instanceId} -> instanceId) (\s@UpdateUserSecurityProfiles' {} a -> s {instanceId = a} :: UpdateUserSecurityProfiles)

instance Core.AWSRequest UpdateUserSecurityProfiles where
  type
    AWSResponse UpdateUserSecurityProfiles =
      UpdateUserSecurityProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateUserSecurityProfilesResponse'

instance Core.Hashable UpdateUserSecurityProfiles

instance Core.NFData UpdateUserSecurityProfiles

instance Core.ToHeaders UpdateUserSecurityProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUserSecurityProfiles where
  toJSON UpdateUserSecurityProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SecurityProfileIds" Core..= securityProfileIds)
          ]
      )

instance Core.ToPath UpdateUserSecurityProfiles where
  toPath UpdateUserSecurityProfiles' {..} =
    Core.mconcat
      [ "/users/",
        Core.toBS instanceId,
        "/",
        Core.toBS userId,
        "/security-profiles"
      ]

instance Core.ToQuery UpdateUserSecurityProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserSecurityProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserSecurityProfilesResponse ::
  UpdateUserSecurityProfilesResponse
newUpdateUserSecurityProfilesResponse =
  UpdateUserSecurityProfilesResponse'

instance
  Core.NFData
    UpdateUserSecurityProfilesResponse

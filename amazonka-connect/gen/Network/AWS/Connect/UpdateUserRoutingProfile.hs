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
-- Module      : Network.AWS.Connect.UpdateUserRoutingProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified routing profile to the specified user.
module Network.AWS.Connect.UpdateUserRoutingProfile
  ( -- * Creating a Request
    UpdateUserRoutingProfile (..),
    newUpdateUserRoutingProfile,

    -- * Request Lenses
    updateUserRoutingProfile_routingProfileId,
    updateUserRoutingProfile_userId,
    updateUserRoutingProfile_instanceId,

    -- * Destructuring the Response
    UpdateUserRoutingProfileResponse (..),
    newUpdateUserRoutingProfileResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { -- | The identifier of the routing profile for the user.
    routingProfileId :: Core.Text,
    -- | The identifier of the user account.
    userId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserRoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingProfileId', 'updateUserRoutingProfile_routingProfileId' - The identifier of the routing profile for the user.
--
-- 'userId', 'updateUserRoutingProfile_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserRoutingProfile_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserRoutingProfile ::
  -- | 'routingProfileId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  UpdateUserRoutingProfile
newUpdateUserRoutingProfile
  pRoutingProfileId_
  pUserId_
  pInstanceId_ =
    UpdateUserRoutingProfile'
      { routingProfileId =
          pRoutingProfileId_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | The identifier of the routing profile for the user.
updateUserRoutingProfile_routingProfileId :: Lens.Lens' UpdateUserRoutingProfile Core.Text
updateUserRoutingProfile_routingProfileId = Lens.lens (\UpdateUserRoutingProfile' {routingProfileId} -> routingProfileId) (\s@UpdateUserRoutingProfile' {} a -> s {routingProfileId = a} :: UpdateUserRoutingProfile)

-- | The identifier of the user account.
updateUserRoutingProfile_userId :: Lens.Lens' UpdateUserRoutingProfile Core.Text
updateUserRoutingProfile_userId = Lens.lens (\UpdateUserRoutingProfile' {userId} -> userId) (\s@UpdateUserRoutingProfile' {} a -> s {userId = a} :: UpdateUserRoutingProfile)

-- | The identifier of the Amazon Connect instance.
updateUserRoutingProfile_instanceId :: Lens.Lens' UpdateUserRoutingProfile Core.Text
updateUserRoutingProfile_instanceId = Lens.lens (\UpdateUserRoutingProfile' {instanceId} -> instanceId) (\s@UpdateUserRoutingProfile' {} a -> s {instanceId = a} :: UpdateUserRoutingProfile)

instance Core.AWSRequest UpdateUserRoutingProfile where
  type
    AWSResponse UpdateUserRoutingProfile =
      UpdateUserRoutingProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateUserRoutingProfileResponse'

instance Core.Hashable UpdateUserRoutingProfile

instance Core.NFData UpdateUserRoutingProfile

instance Core.ToHeaders UpdateUserRoutingProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUserRoutingProfile where
  toJSON UpdateUserRoutingProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RoutingProfileId" Core..= routingProfileId)
          ]
      )

instance Core.ToPath UpdateUserRoutingProfile where
  toPath UpdateUserRoutingProfile' {..} =
    Core.mconcat
      [ "/users/",
        Core.toBS instanceId,
        "/",
        Core.toBS userId,
        "/routing-profile"
      ]

instance Core.ToQuery UpdateUserRoutingProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserRoutingProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserRoutingProfileResponse ::
  UpdateUserRoutingProfileResponse
newUpdateUserRoutingProfileResponse =
  UpdateUserRoutingProfileResponse'

instance Core.NFData UpdateUserRoutingProfileResponse

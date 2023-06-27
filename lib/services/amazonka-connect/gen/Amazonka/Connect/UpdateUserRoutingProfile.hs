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
-- Module      : Amazonka.Connect.UpdateUserRoutingProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified routing profile to the specified user.
module Amazonka.Connect.UpdateUserRoutingProfile
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { -- | The identifier of the routing profile for the user.
    routingProfileId :: Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'instanceId', 'updateUserRoutingProfile_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newUpdateUserRoutingProfile ::
  -- | 'routingProfileId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
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
updateUserRoutingProfile_routingProfileId :: Lens.Lens' UpdateUserRoutingProfile Prelude.Text
updateUserRoutingProfile_routingProfileId = Lens.lens (\UpdateUserRoutingProfile' {routingProfileId} -> routingProfileId) (\s@UpdateUserRoutingProfile' {} a -> s {routingProfileId = a} :: UpdateUserRoutingProfile)

-- | The identifier of the user account.
updateUserRoutingProfile_userId :: Lens.Lens' UpdateUserRoutingProfile Prelude.Text
updateUserRoutingProfile_userId = Lens.lens (\UpdateUserRoutingProfile' {userId} -> userId) (\s@UpdateUserRoutingProfile' {} a -> s {userId = a} :: UpdateUserRoutingProfile)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
updateUserRoutingProfile_instanceId :: Lens.Lens' UpdateUserRoutingProfile Prelude.Text
updateUserRoutingProfile_instanceId = Lens.lens (\UpdateUserRoutingProfile' {instanceId} -> instanceId) (\s@UpdateUserRoutingProfile' {} a -> s {instanceId = a} :: UpdateUserRoutingProfile)

instance Core.AWSRequest UpdateUserRoutingProfile where
  type
    AWSResponse UpdateUserRoutingProfile =
      UpdateUserRoutingProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateUserRoutingProfileResponse'

instance Prelude.Hashable UpdateUserRoutingProfile where
  hashWithSalt _salt UpdateUserRoutingProfile' {..} =
    _salt
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateUserRoutingProfile where
  rnf UpdateUserRoutingProfile' {..} =
    Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders UpdateUserRoutingProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserRoutingProfile where
  toJSON UpdateUserRoutingProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingProfileId" Data..= routingProfileId)
          ]
      )

instance Data.ToPath UpdateUserRoutingProfile where
  toPath UpdateUserRoutingProfile' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId,
        "/routing-profile"
      ]

instance Data.ToQuery UpdateUserRoutingProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserRoutingProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserRoutingProfileResponse ::
  UpdateUserRoutingProfileResponse
newUpdateUserRoutingProfileResponse =
  UpdateUserRoutingProfileResponse'

instance
  Prelude.NFData
    UpdateUserRoutingProfileResponse
  where
  rnf _ = ()

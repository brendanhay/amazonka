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
-- Module      : Amazonka.FinSpaceData.AssociateUserToPermissionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a user account to a permission group to grant permissions for
-- actions a user can perform in FinSpace.
module Amazonka.FinSpaceData.AssociateUserToPermissionGroup
  ( -- * Creating a Request
    AssociateUserToPermissionGroup (..),
    newAssociateUserToPermissionGroup,

    -- * Request Lenses
    associateUserToPermissionGroup_clientToken,
    associateUserToPermissionGroup_permissionGroupId,
    associateUserToPermissionGroup_userId,

    -- * Destructuring the Response
    AssociateUserToPermissionGroupResponse (..),
    newAssociateUserToPermissionGroupResponse,

    -- * Response Lenses
    associateUserToPermissionGroupResponse_statusCode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateUserToPermissionGroup' smart constructor.
data AssociateUserToPermissionGroup = AssociateUserToPermissionGroup'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Text,
    -- | The unique identifier for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserToPermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateUserToPermissionGroup_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'permissionGroupId', 'associateUserToPermissionGroup_permissionGroupId' - The unique identifier for the permission group.
--
-- 'userId', 'associateUserToPermissionGroup_userId' - The unique identifier for the user.
newAssociateUserToPermissionGroup ::
  -- | 'permissionGroupId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  AssociateUserToPermissionGroup
newAssociateUserToPermissionGroup
  pPermissionGroupId_
  pUserId_ =
    AssociateUserToPermissionGroup'
      { clientToken =
          Prelude.Nothing,
        permissionGroupId = pPermissionGroupId_,
        userId = pUserId_
      }

-- | A token that ensures idempotency. This token expires in 10 minutes.
associateUserToPermissionGroup_clientToken :: Lens.Lens' AssociateUserToPermissionGroup (Prelude.Maybe Prelude.Text)
associateUserToPermissionGroup_clientToken = Lens.lens (\AssociateUserToPermissionGroup' {clientToken} -> clientToken) (\s@AssociateUserToPermissionGroup' {} a -> s {clientToken = a} :: AssociateUserToPermissionGroup)

-- | The unique identifier for the permission group.
associateUserToPermissionGroup_permissionGroupId :: Lens.Lens' AssociateUserToPermissionGroup Prelude.Text
associateUserToPermissionGroup_permissionGroupId = Lens.lens (\AssociateUserToPermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@AssociateUserToPermissionGroup' {} a -> s {permissionGroupId = a} :: AssociateUserToPermissionGroup)

-- | The unique identifier for the user.
associateUserToPermissionGroup_userId :: Lens.Lens' AssociateUserToPermissionGroup Prelude.Text
associateUserToPermissionGroup_userId = Lens.lens (\AssociateUserToPermissionGroup' {userId} -> userId) (\s@AssociateUserToPermissionGroup' {} a -> s {userId = a} :: AssociateUserToPermissionGroup)

instance
  Core.AWSRequest
    AssociateUserToPermissionGroup
  where
  type
    AWSResponse AssociateUserToPermissionGroup =
      AssociateUserToPermissionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateUserToPermissionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateUserToPermissionGroup
  where
  hashWithSalt
    _salt
    AssociateUserToPermissionGroup' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` permissionGroupId
        `Prelude.hashWithSalt` userId

instance
  Prelude.NFData
    AssociateUserToPermissionGroup
  where
  rnf AssociateUserToPermissionGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionGroupId
      `Prelude.seq` Prelude.rnf userId

instance
  Core.ToHeaders
    AssociateUserToPermissionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateUserToPermissionGroup where
  toJSON AssociateUserToPermissionGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [("clientToken" Core..=) Prelude.<$> clientToken]
      )

instance Core.ToPath AssociateUserToPermissionGroup where
  toPath AssociateUserToPermissionGroup' {..} =
    Prelude.mconcat
      [ "/permission-group/",
        Core.toBS permissionGroupId,
        "/users/",
        Core.toBS userId
      ]

instance Core.ToQuery AssociateUserToPermissionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateUserToPermissionGroupResponse' smart constructor.
data AssociateUserToPermissionGroupResponse = AssociateUserToPermissionGroupResponse'
  { -- | The returned status code of the response.
    statusCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserToPermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'associateUserToPermissionGroupResponse_statusCode' - The returned status code of the response.
newAssociateUserToPermissionGroupResponse ::
  -- | 'statusCode'
  Prelude.Int ->
  AssociateUserToPermissionGroupResponse
newAssociateUserToPermissionGroupResponse
  pStatusCode_ =
    AssociateUserToPermissionGroupResponse'
      { statusCode =
          pStatusCode_
      }

-- | The returned status code of the response.
associateUserToPermissionGroupResponse_statusCode :: Lens.Lens' AssociateUserToPermissionGroupResponse Prelude.Int
associateUserToPermissionGroupResponse_statusCode = Lens.lens (\AssociateUserToPermissionGroupResponse' {statusCode} -> statusCode) (\s@AssociateUserToPermissionGroupResponse' {} a -> s {statusCode = a} :: AssociateUserToPermissionGroupResponse)

instance
  Prelude.NFData
    AssociateUserToPermissionGroupResponse
  where
  rnf AssociateUserToPermissionGroupResponse' {..} =
    Prelude.rnf statusCode

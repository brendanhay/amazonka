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
-- Module      : Amazonka.FinSpaceData.DisassociateUserFromPermissionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user account from a permission group.
module Amazonka.FinSpaceData.DisassociateUserFromPermissionGroup
  ( -- * Creating a Request
    DisassociateUserFromPermissionGroup (..),
    newDisassociateUserFromPermissionGroup,

    -- * Request Lenses
    disassociateUserFromPermissionGroup_clientToken,
    disassociateUserFromPermissionGroup_permissionGroupId,
    disassociateUserFromPermissionGroup_userId,

    -- * Destructuring the Response
    DisassociateUserFromPermissionGroupResponse (..),
    newDisassociateUserFromPermissionGroupResponse,

    -- * Response Lenses
    disassociateUserFromPermissionGroupResponse_statusCode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateUserFromPermissionGroup' smart constructor.
data DisassociateUserFromPermissionGroup = DisassociateUserFromPermissionGroup'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Text,
    -- | The unique identifier for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserFromPermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateUserFromPermissionGroup_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'permissionGroupId', 'disassociateUserFromPermissionGroup_permissionGroupId' - The unique identifier for the permission group.
--
-- 'userId', 'disassociateUserFromPermissionGroup_userId' - The unique identifier for the user.
newDisassociateUserFromPermissionGroup ::
  -- | 'permissionGroupId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DisassociateUserFromPermissionGroup
newDisassociateUserFromPermissionGroup
  pPermissionGroupId_
  pUserId_ =
    DisassociateUserFromPermissionGroup'
      { clientToken =
          Prelude.Nothing,
        permissionGroupId =
          pPermissionGroupId_,
        userId = pUserId_
      }

-- | A token that ensures idempotency. This token expires in 10 minutes.
disassociateUserFromPermissionGroup_clientToken :: Lens.Lens' DisassociateUserFromPermissionGroup (Prelude.Maybe Prelude.Text)
disassociateUserFromPermissionGroup_clientToken = Lens.lens (\DisassociateUserFromPermissionGroup' {clientToken} -> clientToken) (\s@DisassociateUserFromPermissionGroup' {} a -> s {clientToken = a} :: DisassociateUserFromPermissionGroup)

-- | The unique identifier for the permission group.
disassociateUserFromPermissionGroup_permissionGroupId :: Lens.Lens' DisassociateUserFromPermissionGroup Prelude.Text
disassociateUserFromPermissionGroup_permissionGroupId = Lens.lens (\DisassociateUserFromPermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@DisassociateUserFromPermissionGroup' {} a -> s {permissionGroupId = a} :: DisassociateUserFromPermissionGroup)

-- | The unique identifier for the user.
disassociateUserFromPermissionGroup_userId :: Lens.Lens' DisassociateUserFromPermissionGroup Prelude.Text
disassociateUserFromPermissionGroup_userId = Lens.lens (\DisassociateUserFromPermissionGroup' {userId} -> userId) (\s@DisassociateUserFromPermissionGroup' {} a -> s {userId = a} :: DisassociateUserFromPermissionGroup)

instance
  Core.AWSRequest
    DisassociateUserFromPermissionGroup
  where
  type
    AWSResponse DisassociateUserFromPermissionGroup =
      DisassociateUserFromPermissionGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateUserFromPermissionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateUserFromPermissionGroup
  where
  hashWithSalt
    _salt
    DisassociateUserFromPermissionGroup' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` permissionGroupId
        `Prelude.hashWithSalt` userId

instance
  Prelude.NFData
    DisassociateUserFromPermissionGroup
  where
  rnf DisassociateUserFromPermissionGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionGroupId
      `Prelude.seq` Prelude.rnf userId

instance
  Data.ToHeaders
    DisassociateUserFromPermissionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociateUserFromPermissionGroup
  where
  toPath DisassociateUserFromPermissionGroup' {..} =
    Prelude.mconcat
      [ "/permission-group/",
        Data.toBS permissionGroupId,
        "/users/",
        Data.toBS userId
      ]

instance
  Data.ToQuery
    DisassociateUserFromPermissionGroup
  where
  toQuery DisassociateUserFromPermissionGroup' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDisassociateUserFromPermissionGroupResponse' smart constructor.
data DisassociateUserFromPermissionGroupResponse = DisassociateUserFromPermissionGroupResponse'
  { -- | The returned status code of the response.
    statusCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserFromPermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'disassociateUserFromPermissionGroupResponse_statusCode' - The returned status code of the response.
newDisassociateUserFromPermissionGroupResponse ::
  -- | 'statusCode'
  Prelude.Int ->
  DisassociateUserFromPermissionGroupResponse
newDisassociateUserFromPermissionGroupResponse
  pStatusCode_ =
    DisassociateUserFromPermissionGroupResponse'
      { statusCode =
          pStatusCode_
      }

-- | The returned status code of the response.
disassociateUserFromPermissionGroupResponse_statusCode :: Lens.Lens' DisassociateUserFromPermissionGroupResponse Prelude.Int
disassociateUserFromPermissionGroupResponse_statusCode = Lens.lens (\DisassociateUserFromPermissionGroupResponse' {statusCode} -> statusCode) (\s@DisassociateUserFromPermissionGroupResponse' {} a -> s {statusCode = a} :: DisassociateUserFromPermissionGroupResponse)

instance
  Prelude.NFData
    DisassociateUserFromPermissionGroupResponse
  where
  rnf DisassociateUserFromPermissionGroupResponse' {..} =
    Prelude.rnf statusCode

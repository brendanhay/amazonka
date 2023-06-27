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
-- Module      : Amazonka.WorkSpaces.UpdateConnectionAliasPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares a connection alias with one account by specifying
-- whether that account has permission to associate the connection alias
-- with a directory. If the association permission is granted, the
-- connection alias is shared with that account. If the association
-- permission is revoked, the connection alias is unshared with the
-- account. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- -   Before performing this operation, call
--     <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeConnectionAliases.html DescribeConnectionAliases>
--     to make sure that the current state of the connection alias is
--     @CREATED@.
--
-- -   To delete a connection alias that has been shared, the shared
--     account must first disassociate the connection alias from any
--     directories it has been associated with. Then you must unshare the
--     connection alias from the account it has been shared with. You can
--     delete a connection alias only after it is no longer shared with any
--     accounts or associated with any directories.
module Amazonka.WorkSpaces.UpdateConnectionAliasPermission
  ( -- * Creating a Request
    UpdateConnectionAliasPermission (..),
    newUpdateConnectionAliasPermission,

    -- * Request Lenses
    updateConnectionAliasPermission_aliasId,
    updateConnectionAliasPermission_connectionAliasPermission,

    -- * Destructuring the Response
    UpdateConnectionAliasPermissionResponse (..),
    newUpdateConnectionAliasPermissionResponse,

    -- * Response Lenses
    updateConnectionAliasPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newUpdateConnectionAliasPermission' smart constructor.
data UpdateConnectionAliasPermission = UpdateConnectionAliasPermission'
  { -- | The identifier of the connection alias that you want to update
    -- permissions for.
    aliasId :: Prelude.Text,
    -- | Indicates whether to share or unshare the connection alias with the
    -- specified Amazon Web Services account.
    connectionAliasPermission :: ConnectionAliasPermission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionAliasPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'updateConnectionAliasPermission_aliasId' - The identifier of the connection alias that you want to update
-- permissions for.
--
-- 'connectionAliasPermission', 'updateConnectionAliasPermission_connectionAliasPermission' - Indicates whether to share or unshare the connection alias with the
-- specified Amazon Web Services account.
newUpdateConnectionAliasPermission ::
  -- | 'aliasId'
  Prelude.Text ->
  -- | 'connectionAliasPermission'
  ConnectionAliasPermission ->
  UpdateConnectionAliasPermission
newUpdateConnectionAliasPermission
  pAliasId_
  pConnectionAliasPermission_ =
    UpdateConnectionAliasPermission'
      { aliasId =
          pAliasId_,
        connectionAliasPermission =
          pConnectionAliasPermission_
      }

-- | The identifier of the connection alias that you want to update
-- permissions for.
updateConnectionAliasPermission_aliasId :: Lens.Lens' UpdateConnectionAliasPermission Prelude.Text
updateConnectionAliasPermission_aliasId = Lens.lens (\UpdateConnectionAliasPermission' {aliasId} -> aliasId) (\s@UpdateConnectionAliasPermission' {} a -> s {aliasId = a} :: UpdateConnectionAliasPermission)

-- | Indicates whether to share or unshare the connection alias with the
-- specified Amazon Web Services account.
updateConnectionAliasPermission_connectionAliasPermission :: Lens.Lens' UpdateConnectionAliasPermission ConnectionAliasPermission
updateConnectionAliasPermission_connectionAliasPermission = Lens.lens (\UpdateConnectionAliasPermission' {connectionAliasPermission} -> connectionAliasPermission) (\s@UpdateConnectionAliasPermission' {} a -> s {connectionAliasPermission = a} :: UpdateConnectionAliasPermission)

instance
  Core.AWSRequest
    UpdateConnectionAliasPermission
  where
  type
    AWSResponse UpdateConnectionAliasPermission =
      UpdateConnectionAliasPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectionAliasPermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateConnectionAliasPermission
  where
  hashWithSalt
    _salt
    UpdateConnectionAliasPermission' {..} =
      _salt
        `Prelude.hashWithSalt` aliasId
        `Prelude.hashWithSalt` connectionAliasPermission

instance
  Prelude.NFData
    UpdateConnectionAliasPermission
  where
  rnf UpdateConnectionAliasPermission' {..} =
    Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf connectionAliasPermission

instance
  Data.ToHeaders
    UpdateConnectionAliasPermission
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.UpdateConnectionAliasPermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectionAliasPermission where
  toJSON UpdateConnectionAliasPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasId" Data..= aliasId),
            Prelude.Just
              ( "ConnectionAliasPermission"
                  Data..= connectionAliasPermission
              )
          ]
      )

instance Data.ToPath UpdateConnectionAliasPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnectionAliasPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectionAliasPermissionResponse' smart constructor.
data UpdateConnectionAliasPermissionResponse = UpdateConnectionAliasPermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionAliasPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectionAliasPermissionResponse_httpStatus' - The response's http status code.
newUpdateConnectionAliasPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectionAliasPermissionResponse
newUpdateConnectionAliasPermissionResponse
  pHttpStatus_ =
    UpdateConnectionAliasPermissionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateConnectionAliasPermissionResponse_httpStatus :: Lens.Lens' UpdateConnectionAliasPermissionResponse Prelude.Int
updateConnectionAliasPermissionResponse_httpStatus = Lens.lens (\UpdateConnectionAliasPermissionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionAliasPermissionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionAliasPermissionResponse)

instance
  Prelude.NFData
    UpdateConnectionAliasPermissionResponse
  where
  rnf UpdateConnectionAliasPermissionResponse' {..} =
    Prelude.rnf httpStatus

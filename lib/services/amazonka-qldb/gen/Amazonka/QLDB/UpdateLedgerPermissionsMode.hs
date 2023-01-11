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
-- Module      : Amazonka.QLDB.UpdateLedgerPermissionsMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the permissions mode of a ledger.
--
-- Before you switch to the @STANDARD@ permissions mode, you must first
-- create all required IAM policies and table tags to avoid disruption to
-- your users. To learn more, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/ledger-management.basics.html#ledger-mgmt.basics.update-permissions.migrating Migrating to the standard permissions mode>
-- in the /Amazon QLDB Developer Guide/.
module Amazonka.QLDB.UpdateLedgerPermissionsMode
  ( -- * Creating a Request
    UpdateLedgerPermissionsMode (..),
    newUpdateLedgerPermissionsMode,

    -- * Request Lenses
    updateLedgerPermissionsMode_name,
    updateLedgerPermissionsMode_permissionsMode,

    -- * Destructuring the Response
    UpdateLedgerPermissionsModeResponse (..),
    newUpdateLedgerPermissionsModeResponse,

    -- * Response Lenses
    updateLedgerPermissionsModeResponse_arn,
    updateLedgerPermissionsModeResponse_name,
    updateLedgerPermissionsModeResponse_permissionsMode,
    updateLedgerPermissionsModeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLedgerPermissionsMode' smart constructor.
data UpdateLedgerPermissionsMode = UpdateLedgerPermissionsMode'
  { -- | The name of the ledger.
    name :: Prelude.Text,
    -- | The permissions mode to assign to the ledger. This parameter can have
    -- one of the following values:
    --
    -- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
    --     with API-level granularity for ledgers.
    --
    --     This mode allows users who have the @SendCommand@ API permission for
    --     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
    --     tables in the specified ledger. This mode disregards any table-level
    --     or command-level IAM permissions policies that you create for the
    --     ledger.
    --
    -- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
    --     control with finer granularity for ledgers, tables, and PartiQL
    --     commands.
    --
    --     By default, this mode denies all user requests to run any PartiQL
    --     commands on any tables in this ledger. To allow PartiQL commands to
    --     run, you must create IAM permissions policies for specific table
    --     resources and PartiQL actions, in addition to the @SendCommand@ API
    --     permission for the ledger. For information, see
    --     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
    --     in the /Amazon QLDB Developer Guide/.
    --
    -- We strongly recommend using the @STANDARD@ permissions mode to maximize
    -- the security of your ledger data.
    permissionsMode :: PermissionsMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLedgerPermissionsMode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateLedgerPermissionsMode_name' - The name of the ledger.
--
-- 'permissionsMode', 'updateLedgerPermissionsMode_permissionsMode' - The permissions mode to assign to the ledger. This parameter can have
-- one of the following values:
--
-- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
--     with API-level granularity for ledgers.
--
--     This mode allows users who have the @SendCommand@ API permission for
--     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
--     tables in the specified ledger. This mode disregards any table-level
--     or command-level IAM permissions policies that you create for the
--     ledger.
--
-- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
--     control with finer granularity for ledgers, tables, and PartiQL
--     commands.
--
--     By default, this mode denies all user requests to run any PartiQL
--     commands on any tables in this ledger. To allow PartiQL commands to
--     run, you must create IAM permissions policies for specific table
--     resources and PartiQL actions, in addition to the @SendCommand@ API
--     permission for the ledger. For information, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
--     in the /Amazon QLDB Developer Guide/.
--
-- We strongly recommend using the @STANDARD@ permissions mode to maximize
-- the security of your ledger data.
newUpdateLedgerPermissionsMode ::
  -- | 'name'
  Prelude.Text ->
  -- | 'permissionsMode'
  PermissionsMode ->
  UpdateLedgerPermissionsMode
newUpdateLedgerPermissionsMode
  pName_
  pPermissionsMode_ =
    UpdateLedgerPermissionsMode'
      { name = pName_,
        permissionsMode = pPermissionsMode_
      }

-- | The name of the ledger.
updateLedgerPermissionsMode_name :: Lens.Lens' UpdateLedgerPermissionsMode Prelude.Text
updateLedgerPermissionsMode_name = Lens.lens (\UpdateLedgerPermissionsMode' {name} -> name) (\s@UpdateLedgerPermissionsMode' {} a -> s {name = a} :: UpdateLedgerPermissionsMode)

-- | The permissions mode to assign to the ledger. This parameter can have
-- one of the following values:
--
-- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
--     with API-level granularity for ledgers.
--
--     This mode allows users who have the @SendCommand@ API permission for
--     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
--     tables in the specified ledger. This mode disregards any table-level
--     or command-level IAM permissions policies that you create for the
--     ledger.
--
-- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
--     control with finer granularity for ledgers, tables, and PartiQL
--     commands.
--
--     By default, this mode denies all user requests to run any PartiQL
--     commands on any tables in this ledger. To allow PartiQL commands to
--     run, you must create IAM permissions policies for specific table
--     resources and PartiQL actions, in addition to the @SendCommand@ API
--     permission for the ledger. For information, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
--     in the /Amazon QLDB Developer Guide/.
--
-- We strongly recommend using the @STANDARD@ permissions mode to maximize
-- the security of your ledger data.
updateLedgerPermissionsMode_permissionsMode :: Lens.Lens' UpdateLedgerPermissionsMode PermissionsMode
updateLedgerPermissionsMode_permissionsMode = Lens.lens (\UpdateLedgerPermissionsMode' {permissionsMode} -> permissionsMode) (\s@UpdateLedgerPermissionsMode' {} a -> s {permissionsMode = a} :: UpdateLedgerPermissionsMode)

instance Core.AWSRequest UpdateLedgerPermissionsMode where
  type
    AWSResponse UpdateLedgerPermissionsMode =
      UpdateLedgerPermissionsModeResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLedgerPermissionsModeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PermissionsMode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLedgerPermissionsMode where
  hashWithSalt _salt UpdateLedgerPermissionsMode' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionsMode

instance Prelude.NFData UpdateLedgerPermissionsMode where
  rnf UpdateLedgerPermissionsMode' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionsMode

instance Data.ToHeaders UpdateLedgerPermissionsMode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLedgerPermissionsMode where
  toJSON UpdateLedgerPermissionsMode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PermissionsMode" Data..= permissionsMode)
          ]
      )

instance Data.ToPath UpdateLedgerPermissionsMode where
  toPath UpdateLedgerPermissionsMode' {..} =
    Prelude.mconcat
      ["/ledgers/", Data.toBS name, "/permissions-mode"]

instance Data.ToQuery UpdateLedgerPermissionsMode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLedgerPermissionsModeResponse' smart constructor.
data UpdateLedgerPermissionsModeResponse = UpdateLedgerPermissionsModeResponse'
  { -- | The Amazon Resource Name (ARN) for the ledger.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ledger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current permissions mode of the ledger.
    permissionsMode :: Prelude.Maybe PermissionsMode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLedgerPermissionsModeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateLedgerPermissionsModeResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'name', 'updateLedgerPermissionsModeResponse_name' - The name of the ledger.
--
-- 'permissionsMode', 'updateLedgerPermissionsModeResponse_permissionsMode' - The current permissions mode of the ledger.
--
-- 'httpStatus', 'updateLedgerPermissionsModeResponse_httpStatus' - The response's http status code.
newUpdateLedgerPermissionsModeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLedgerPermissionsModeResponse
newUpdateLedgerPermissionsModeResponse pHttpStatus_ =
  UpdateLedgerPermissionsModeResponse'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      permissionsMode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the ledger.
updateLedgerPermissionsModeResponse_arn :: Lens.Lens' UpdateLedgerPermissionsModeResponse (Prelude.Maybe Prelude.Text)
updateLedgerPermissionsModeResponse_arn = Lens.lens (\UpdateLedgerPermissionsModeResponse' {arn} -> arn) (\s@UpdateLedgerPermissionsModeResponse' {} a -> s {arn = a} :: UpdateLedgerPermissionsModeResponse)

-- | The name of the ledger.
updateLedgerPermissionsModeResponse_name :: Lens.Lens' UpdateLedgerPermissionsModeResponse (Prelude.Maybe Prelude.Text)
updateLedgerPermissionsModeResponse_name = Lens.lens (\UpdateLedgerPermissionsModeResponse' {name} -> name) (\s@UpdateLedgerPermissionsModeResponse' {} a -> s {name = a} :: UpdateLedgerPermissionsModeResponse)

-- | The current permissions mode of the ledger.
updateLedgerPermissionsModeResponse_permissionsMode :: Lens.Lens' UpdateLedgerPermissionsModeResponse (Prelude.Maybe PermissionsMode)
updateLedgerPermissionsModeResponse_permissionsMode = Lens.lens (\UpdateLedgerPermissionsModeResponse' {permissionsMode} -> permissionsMode) (\s@UpdateLedgerPermissionsModeResponse' {} a -> s {permissionsMode = a} :: UpdateLedgerPermissionsModeResponse)

-- | The response's http status code.
updateLedgerPermissionsModeResponse_httpStatus :: Lens.Lens' UpdateLedgerPermissionsModeResponse Prelude.Int
updateLedgerPermissionsModeResponse_httpStatus = Lens.lens (\UpdateLedgerPermissionsModeResponse' {httpStatus} -> httpStatus) (\s@UpdateLedgerPermissionsModeResponse' {} a -> s {httpStatus = a} :: UpdateLedgerPermissionsModeResponse)

instance
  Prelude.NFData
    UpdateLedgerPermissionsModeResponse
  where
  rnf UpdateLedgerPermissionsModeResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionsMode
      `Prelude.seq` Prelude.rnf httpStatus

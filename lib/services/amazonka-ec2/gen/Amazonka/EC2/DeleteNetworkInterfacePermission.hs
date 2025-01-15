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
-- Module      : Amazonka.EC2.DeleteNetworkInterfacePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a permission for a network interface. By default, you cannot
-- delete the permission if the account for which you\'re removing the
-- permission has attached the network interface to an instance. However,
-- you can force delete the permission, regardless of any attachment.
module Amazonka.EC2.DeleteNetworkInterfacePermission
  ( -- * Creating a Request
    DeleteNetworkInterfacePermission (..),
    newDeleteNetworkInterfacePermission,

    -- * Request Lenses
    deleteNetworkInterfacePermission_dryRun,
    deleteNetworkInterfacePermission_force,
    deleteNetworkInterfacePermission_networkInterfacePermissionId,

    -- * Destructuring the Response
    DeleteNetworkInterfacePermissionResponse (..),
    newDeleteNetworkInterfacePermissionResponse,

    -- * Response Lenses
    deleteNetworkInterfacePermissionResponse_return,
    deleteNetworkInterfacePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteNetworkInterfacePermission.
--
-- /See:/ 'newDeleteNetworkInterfacePermission' smart constructor.
data DeleteNetworkInterfacePermission = DeleteNetworkInterfacePermission'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specify @true@ to remove the permission even if the network interface is
    -- attached to an instance.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInterfacePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInterfacePermission_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'force', 'deleteNetworkInterfacePermission_force' - Specify @true@ to remove the permission even if the network interface is
-- attached to an instance.
--
-- 'networkInterfacePermissionId', 'deleteNetworkInterfacePermission_networkInterfacePermissionId' - The ID of the network interface permission.
newDeleteNetworkInterfacePermission ::
  -- | 'networkInterfacePermissionId'
  Prelude.Text ->
  DeleteNetworkInterfacePermission
newDeleteNetworkInterfacePermission
  pNetworkInterfacePermissionId_ =
    DeleteNetworkInterfacePermission'
      { dryRun =
          Prelude.Nothing,
        force = Prelude.Nothing,
        networkInterfacePermissionId =
          pNetworkInterfacePermissionId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInterfacePermission_dryRun :: Lens.Lens' DeleteNetworkInterfacePermission (Prelude.Maybe Prelude.Bool)
deleteNetworkInterfacePermission_dryRun = Lens.lens (\DeleteNetworkInterfacePermission' {dryRun} -> dryRun) (\s@DeleteNetworkInterfacePermission' {} a -> s {dryRun = a} :: DeleteNetworkInterfacePermission)

-- | Specify @true@ to remove the permission even if the network interface is
-- attached to an instance.
deleteNetworkInterfacePermission_force :: Lens.Lens' DeleteNetworkInterfacePermission (Prelude.Maybe Prelude.Bool)
deleteNetworkInterfacePermission_force = Lens.lens (\DeleteNetworkInterfacePermission' {force} -> force) (\s@DeleteNetworkInterfacePermission' {} a -> s {force = a} :: DeleteNetworkInterfacePermission)

-- | The ID of the network interface permission.
deleteNetworkInterfacePermission_networkInterfacePermissionId :: Lens.Lens' DeleteNetworkInterfacePermission Prelude.Text
deleteNetworkInterfacePermission_networkInterfacePermissionId = Lens.lens (\DeleteNetworkInterfacePermission' {networkInterfacePermissionId} -> networkInterfacePermissionId) (\s@DeleteNetworkInterfacePermission' {} a -> s {networkInterfacePermissionId = a} :: DeleteNetworkInterfacePermission)

instance
  Core.AWSRequest
    DeleteNetworkInterfacePermission
  where
  type
    AWSResponse DeleteNetworkInterfacePermission =
      DeleteNetworkInterfacePermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInterfacePermissionResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkInterfacePermission
  where
  hashWithSalt
    _salt
    DeleteNetworkInterfacePermission' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` force
        `Prelude.hashWithSalt` networkInterfacePermissionId

instance
  Prelude.NFData
    DeleteNetworkInterfacePermission
  where
  rnf DeleteNetworkInterfacePermission' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf force `Prelude.seq`
        Prelude.rnf networkInterfacePermissionId

instance
  Data.ToHeaders
    DeleteNetworkInterfacePermission
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkInterfacePermission where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteNetworkInterfacePermission
  where
  toQuery DeleteNetworkInterfacePermission' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteNetworkInterfacePermission" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Force" Data.=: force,
        "NetworkInterfacePermissionId"
          Data.=: networkInterfacePermissionId
      ]

-- | Contains the output for DeleteNetworkInterfacePermission.
--
-- /See:/ 'newDeleteNetworkInterfacePermissionResponse' smart constructor.
data DeleteNetworkInterfacePermissionResponse = DeleteNetworkInterfacePermissionResponse'
  { -- | Returns @true@ if the request succeeds, otherwise returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInterfacePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'deleteNetworkInterfacePermissionResponse_return' - Returns @true@ if the request succeeds, otherwise returns an error.
--
-- 'httpStatus', 'deleteNetworkInterfacePermissionResponse_httpStatus' - The response's http status code.
newDeleteNetworkInterfacePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkInterfacePermissionResponse
newDeleteNetworkInterfacePermissionResponse
  pHttpStatus_ =
    DeleteNetworkInterfacePermissionResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds, otherwise returns an error.
deleteNetworkInterfacePermissionResponse_return :: Lens.Lens' DeleteNetworkInterfacePermissionResponse (Prelude.Maybe Prelude.Bool)
deleteNetworkInterfacePermissionResponse_return = Lens.lens (\DeleteNetworkInterfacePermissionResponse' {return'} -> return') (\s@DeleteNetworkInterfacePermissionResponse' {} a -> s {return' = a} :: DeleteNetworkInterfacePermissionResponse)

-- | The response's http status code.
deleteNetworkInterfacePermissionResponse_httpStatus :: Lens.Lens' DeleteNetworkInterfacePermissionResponse Prelude.Int
deleteNetworkInterfacePermissionResponse_httpStatus = Lens.lens (\DeleteNetworkInterfacePermissionResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInterfacePermissionResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInterfacePermissionResponse)

instance
  Prelude.NFData
    DeleteNetworkInterfacePermissionResponse
  where
  rnf DeleteNetworkInterfacePermissionResponse' {..} =
    Prelude.rnf return' `Prelude.seq`
      Prelude.rnf httpStatus

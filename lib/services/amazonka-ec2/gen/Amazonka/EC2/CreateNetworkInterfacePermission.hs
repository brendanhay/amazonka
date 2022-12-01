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
-- Module      : Amazonka.EC2.CreateNetworkInterfacePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an Amazon Web Services-authorized account permission to attach
-- the specified network interface to an instance in their account.
--
-- You can grant permission to a single Amazon Web Services account only,
-- and only one account at a time.
module Amazonka.EC2.CreateNetworkInterfacePermission
  ( -- * Creating a Request
    CreateNetworkInterfacePermission (..),
    newCreateNetworkInterfacePermission,

    -- * Request Lenses
    createNetworkInterfacePermission_awsAccountId,
    createNetworkInterfacePermission_dryRun,
    createNetworkInterfacePermission_awsService,
    createNetworkInterfacePermission_networkInterfaceId,
    createNetworkInterfacePermission_permission,

    -- * Destructuring the Response
    CreateNetworkInterfacePermissionResponse (..),
    newCreateNetworkInterfacePermissionResponse,

    -- * Response Lenses
    createNetworkInterfacePermissionResponse_interfacePermission,
    createNetworkInterfacePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
-- /See:/ 'newCreateNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Service. Currently not supported.
    awsService :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text,
    -- | The type of permission to grant.
    permission :: InterfacePermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterfacePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createNetworkInterfacePermission_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dryRun', 'createNetworkInterfacePermission_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'awsService', 'createNetworkInterfacePermission_awsService' - The Amazon Web Service. Currently not supported.
--
-- 'networkInterfaceId', 'createNetworkInterfacePermission_networkInterfaceId' - The ID of the network interface.
--
-- 'permission', 'createNetworkInterfacePermission_permission' - The type of permission to grant.
newCreateNetworkInterfacePermission ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  -- | 'permission'
  InterfacePermissionType ->
  CreateNetworkInterfacePermission
newCreateNetworkInterfacePermission
  pNetworkInterfaceId_
  pPermission_ =
    CreateNetworkInterfacePermission'
      { awsAccountId =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        awsService = Prelude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        permission = pPermission_
      }

-- | The Amazon Web Services account ID.
createNetworkInterfacePermission_awsAccountId :: Lens.Lens' CreateNetworkInterfacePermission (Prelude.Maybe Prelude.Text)
createNetworkInterfacePermission_awsAccountId = Lens.lens (\CreateNetworkInterfacePermission' {awsAccountId} -> awsAccountId) (\s@CreateNetworkInterfacePermission' {} a -> s {awsAccountId = a} :: CreateNetworkInterfacePermission)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInterfacePermission_dryRun :: Lens.Lens' CreateNetworkInterfacePermission (Prelude.Maybe Prelude.Bool)
createNetworkInterfacePermission_dryRun = Lens.lens (\CreateNetworkInterfacePermission' {dryRun} -> dryRun) (\s@CreateNetworkInterfacePermission' {} a -> s {dryRun = a} :: CreateNetworkInterfacePermission)

-- | The Amazon Web Service. Currently not supported.
createNetworkInterfacePermission_awsService :: Lens.Lens' CreateNetworkInterfacePermission (Prelude.Maybe Prelude.Text)
createNetworkInterfacePermission_awsService = Lens.lens (\CreateNetworkInterfacePermission' {awsService} -> awsService) (\s@CreateNetworkInterfacePermission' {} a -> s {awsService = a} :: CreateNetworkInterfacePermission)

-- | The ID of the network interface.
createNetworkInterfacePermission_networkInterfaceId :: Lens.Lens' CreateNetworkInterfacePermission Prelude.Text
createNetworkInterfacePermission_networkInterfaceId = Lens.lens (\CreateNetworkInterfacePermission' {networkInterfaceId} -> networkInterfaceId) (\s@CreateNetworkInterfacePermission' {} a -> s {networkInterfaceId = a} :: CreateNetworkInterfacePermission)

-- | The type of permission to grant.
createNetworkInterfacePermission_permission :: Lens.Lens' CreateNetworkInterfacePermission InterfacePermissionType
createNetworkInterfacePermission_permission = Lens.lens (\CreateNetworkInterfacePermission' {permission} -> permission) (\s@CreateNetworkInterfacePermission' {} a -> s {permission = a} :: CreateNetworkInterfacePermission)

instance
  Core.AWSRequest
    CreateNetworkInterfacePermission
  where
  type
    AWSResponse CreateNetworkInterfacePermission =
      CreateNetworkInterfacePermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInterfacePermissionResponse'
            Prelude.<$> (x Core..@? "interfacePermission")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateNetworkInterfacePermission
  where
  hashWithSalt
    _salt
    CreateNetworkInterfacePermission' {..} =
      _salt `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` awsService
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` permission

instance
  Prelude.NFData
    CreateNetworkInterfacePermission
  where
  rnf CreateNetworkInterfacePermission' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf permission

instance
  Core.ToHeaders
    CreateNetworkInterfacePermission
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateNetworkInterfacePermission where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateNetworkInterfacePermission
  where
  toQuery CreateNetworkInterfacePermission' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateNetworkInterfacePermission" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "AwsAccountId" Core.=: awsAccountId,
        "DryRun" Core.=: dryRun,
        "AwsService" Core.=: awsService,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "Permission" Core.=: permission
      ]

-- | Contains the output of CreateNetworkInterfacePermission.
--
-- /See:/ 'newCreateNetworkInterfacePermissionResponse' smart constructor.
data CreateNetworkInterfacePermissionResponse = CreateNetworkInterfacePermissionResponse'
  { -- | Information about the permission for the network interface.
    interfacePermission :: Prelude.Maybe NetworkInterfacePermission,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterfacePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interfacePermission', 'createNetworkInterfacePermissionResponse_interfacePermission' - Information about the permission for the network interface.
--
-- 'httpStatus', 'createNetworkInterfacePermissionResponse_httpStatus' - The response's http status code.
newCreateNetworkInterfacePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkInterfacePermissionResponse
newCreateNetworkInterfacePermissionResponse
  pHttpStatus_ =
    CreateNetworkInterfacePermissionResponse'
      { interfacePermission =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the permission for the network interface.
createNetworkInterfacePermissionResponse_interfacePermission :: Lens.Lens' CreateNetworkInterfacePermissionResponse (Prelude.Maybe NetworkInterfacePermission)
createNetworkInterfacePermissionResponse_interfacePermission = Lens.lens (\CreateNetworkInterfacePermissionResponse' {interfacePermission} -> interfacePermission) (\s@CreateNetworkInterfacePermissionResponse' {} a -> s {interfacePermission = a} :: CreateNetworkInterfacePermissionResponse)

-- | The response's http status code.
createNetworkInterfacePermissionResponse_httpStatus :: Lens.Lens' CreateNetworkInterfacePermissionResponse Prelude.Int
createNetworkInterfacePermissionResponse_httpStatus = Lens.lens (\CreateNetworkInterfacePermissionResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInterfacePermissionResponse' {} a -> s {httpStatus = a} :: CreateNetworkInterfacePermissionResponse)

instance
  Prelude.NFData
    CreateNetworkInterfacePermissionResponse
  where
  rnf CreateNetworkInterfacePermissionResponse' {..} =
    Prelude.rnf interfacePermission
      `Prelude.seq` Prelude.rnf httpStatus

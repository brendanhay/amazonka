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
-- Module      : Network.AWS.EC2.CreateNetworkInterfacePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS-authorized account permission to attach the specified
-- network interface to an instance in their account.
--
-- You can grant permission to a single AWS account only, and only one
-- account at a time.
module Network.AWS.EC2.CreateNetworkInterfacePermission
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
-- /See:/ 'newCreateNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { -- | The AWS account ID.
    awsAccountId :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The AWS service. Currently not supported.
    awsService :: Core.Maybe Core.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Text,
    -- | The type of permission to grant.
    permission :: InterfacePermissionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkInterfacePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createNetworkInterfacePermission_awsAccountId' - The AWS account ID.
--
-- 'dryRun', 'createNetworkInterfacePermission_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'awsService', 'createNetworkInterfacePermission_awsService' - The AWS service. Currently not supported.
--
-- 'networkInterfaceId', 'createNetworkInterfacePermission_networkInterfaceId' - The ID of the network interface.
--
-- 'permission', 'createNetworkInterfacePermission_permission' - The type of permission to grant.
newCreateNetworkInterfacePermission ::
  -- | 'networkInterfaceId'
  Core.Text ->
  -- | 'permission'
  InterfacePermissionType ->
  CreateNetworkInterfacePermission
newCreateNetworkInterfacePermission
  pNetworkInterfaceId_
  pPermission_ =
    CreateNetworkInterfacePermission'
      { awsAccountId =
          Core.Nothing,
        dryRun = Core.Nothing,
        awsService = Core.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        permission = pPermission_
      }

-- | The AWS account ID.
createNetworkInterfacePermission_awsAccountId :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Text)
createNetworkInterfacePermission_awsAccountId = Lens.lens (\CreateNetworkInterfacePermission' {awsAccountId} -> awsAccountId) (\s@CreateNetworkInterfacePermission' {} a -> s {awsAccountId = a} :: CreateNetworkInterfacePermission)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInterfacePermission_dryRun :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Bool)
createNetworkInterfacePermission_dryRun = Lens.lens (\CreateNetworkInterfacePermission' {dryRun} -> dryRun) (\s@CreateNetworkInterfacePermission' {} a -> s {dryRun = a} :: CreateNetworkInterfacePermission)

-- | The AWS service. Currently not supported.
createNetworkInterfacePermission_awsService :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Text)
createNetworkInterfacePermission_awsService = Lens.lens (\CreateNetworkInterfacePermission' {awsService} -> awsService) (\s@CreateNetworkInterfacePermission' {} a -> s {awsService = a} :: CreateNetworkInterfacePermission)

-- | The ID of the network interface.
createNetworkInterfacePermission_networkInterfaceId :: Lens.Lens' CreateNetworkInterfacePermission Core.Text
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInterfacePermissionResponse'
            Core.<$> (x Core..@? "interfacePermission")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateNetworkInterfacePermission

instance Core.NFData CreateNetworkInterfacePermission

instance
  Core.ToHeaders
    CreateNetworkInterfacePermission
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateNetworkInterfacePermission where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateNetworkInterfacePermission
  where
  toQuery CreateNetworkInterfacePermission' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "CreateNetworkInterfacePermission" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
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
    interfacePermission :: Core.Maybe NetworkInterfacePermission,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateNetworkInterfacePermissionResponse
newCreateNetworkInterfacePermissionResponse
  pHttpStatus_ =
    CreateNetworkInterfacePermissionResponse'
      { interfacePermission =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the permission for the network interface.
createNetworkInterfacePermissionResponse_interfacePermission :: Lens.Lens' CreateNetworkInterfacePermissionResponse (Core.Maybe NetworkInterfacePermission)
createNetworkInterfacePermissionResponse_interfacePermission = Lens.lens (\CreateNetworkInterfacePermissionResponse' {interfacePermission} -> interfacePermission) (\s@CreateNetworkInterfacePermissionResponse' {} a -> s {interfacePermission = a} :: CreateNetworkInterfacePermissionResponse)

-- | The response's http status code.
createNetworkInterfacePermissionResponse_httpStatus :: Lens.Lens' CreateNetworkInterfacePermissionResponse Core.Int
createNetworkInterfacePermissionResponse_httpStatus = Lens.lens (\CreateNetworkInterfacePermissionResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInterfacePermissionResponse' {} a -> s {httpStatus = a} :: CreateNetworkInterfacePermissionResponse)

instance
  Core.NFData
    CreateNetworkInterfacePermissionResponse

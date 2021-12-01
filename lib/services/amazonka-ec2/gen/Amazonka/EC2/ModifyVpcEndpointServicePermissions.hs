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
-- Module      : Amazonka.EC2.ModifyVpcEndpointServicePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the permissions for your
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC endpoint service>.
-- You can add or remove permissions for service consumers (IAM users, IAM
-- roles, and Amazon Web Services accounts) to connect to your endpoint
-- service.
--
-- If you grant permissions to all principals, the service is public. Any
-- users who know the name of a public service can send a request to attach
-- an endpoint. If the service does not require manual approval,
-- attachments are automatically approved.
module Amazonka.EC2.ModifyVpcEndpointServicePermissions
  ( -- * Creating a Request
    ModifyVpcEndpointServicePermissions (..),
    newModifyVpcEndpointServicePermissions,

    -- * Request Lenses
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_serviceId,

    -- * Destructuring the Response
    ModifyVpcEndpointServicePermissionsResponse (..),
    newModifyVpcEndpointServicePermissionsResponse,

    -- * Response Lenses
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcEndpointServicePermissions' smart constructor.
data ModifyVpcEndpointServicePermissions = ModifyVpcEndpointServicePermissions'
  { -- | The Amazon Resource Names (ARN) of one or more principals. Permissions
    -- are revoked for principals in this list.
    removeAllowedPrincipals :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARN) of one or more principals. Permissions
    -- are granted to the principals in this list. To grant permissions to all
    -- principals, specify an asterisk (*).
    addAllowedPrincipals :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the service.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeAllowedPrincipals', 'modifyVpcEndpointServicePermissions_removeAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are revoked for principals in this list.
--
-- 'addAllowedPrincipals', 'modifyVpcEndpointServicePermissions_addAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are granted to the principals in this list. To grant permissions to all
-- principals, specify an asterisk (*).
--
-- 'dryRun', 'modifyVpcEndpointServicePermissions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'modifyVpcEndpointServicePermissions_serviceId' - The ID of the service.
newModifyVpcEndpointServicePermissions ::
  -- | 'serviceId'
  Prelude.Text ->
  ModifyVpcEndpointServicePermissions
newModifyVpcEndpointServicePermissions pServiceId_ =
  ModifyVpcEndpointServicePermissions'
    { removeAllowedPrincipals =
        Prelude.Nothing,
      addAllowedPrincipals = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are revoked for principals in this list.
modifyVpcEndpointServicePermissions_removeAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServicePermissions_removeAllowedPrincipals = Lens.lens (\ModifyVpcEndpointServicePermissions' {removeAllowedPrincipals} -> removeAllowedPrincipals) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {removeAllowedPrincipals = a} :: ModifyVpcEndpointServicePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are granted to the principals in this list. To grant permissions to all
-- principals, specify an asterisk (*).
modifyVpcEndpointServicePermissions_addAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServicePermissions_addAllowedPrincipals = Lens.lens (\ModifyVpcEndpointServicePermissions' {addAllowedPrincipals} -> addAllowedPrincipals) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {addAllowedPrincipals = a} :: ModifyVpcEndpointServicePermissions) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointServicePermissions_dryRun :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePermissions_dryRun = Lens.lens (\ModifyVpcEndpointServicePermissions' {dryRun} -> dryRun) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {dryRun = a} :: ModifyVpcEndpointServicePermissions)

-- | The ID of the service.
modifyVpcEndpointServicePermissions_serviceId :: Lens.Lens' ModifyVpcEndpointServicePermissions Prelude.Text
modifyVpcEndpointServicePermissions_serviceId = Lens.lens (\ModifyVpcEndpointServicePermissions' {serviceId} -> serviceId) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {serviceId = a} :: ModifyVpcEndpointServicePermissions)

instance
  Core.AWSRequest
    ModifyVpcEndpointServicePermissions
  where
  type
    AWSResponse ModifyVpcEndpointServicePermissions =
      ModifyVpcEndpointServicePermissionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointServicePermissionsResponse'
            Prelude.<$> (x Core..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointServicePermissions
  where
  hashWithSalt
    salt'
    ModifyVpcEndpointServicePermissions' {..} =
      salt' `Prelude.hashWithSalt` serviceId
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` addAllowedPrincipals
        `Prelude.hashWithSalt` removeAllowedPrincipals

instance
  Prelude.NFData
    ModifyVpcEndpointServicePermissions
  where
  rnf ModifyVpcEndpointServicePermissions' {..} =
    Prelude.rnf removeAllowedPrincipals
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf addAllowedPrincipals

instance
  Core.ToHeaders
    ModifyVpcEndpointServicePermissions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyVpcEndpointServicePermissions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyVpcEndpointServicePermissions
  where
  toQuery ModifyVpcEndpointServicePermissions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyVpcEndpointServicePermissions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "RemoveAllowedPrincipals"
              Prelude.<$> removeAllowedPrincipals
          ),
        Core.toQuery
          ( Core.toQueryList "AddAllowedPrincipals"
              Prelude.<$> addAllowedPrincipals
          ),
        "DryRun" Core.=: dryRun,
        "ServiceId" Core.=: serviceId
      ]

-- | /See:/ 'newModifyVpcEndpointServicePermissionsResponse' smart constructor.
data ModifyVpcEndpointServicePermissionsResponse = ModifyVpcEndpointServicePermissionsResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServicePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'modifyVpcEndpointServicePermissionsResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyVpcEndpointServicePermissionsResponse_httpStatus' - The response's http status code.
newModifyVpcEndpointServicePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpcEndpointServicePermissionsResponse
newModifyVpcEndpointServicePermissionsResponse
  pHttpStatus_ =
    ModifyVpcEndpointServicePermissionsResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointServicePermissionsResponse_returnValue :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePermissionsResponse_returnValue = Lens.lens (\ModifyVpcEndpointServicePermissionsResponse' {returnValue} -> returnValue) (\s@ModifyVpcEndpointServicePermissionsResponse' {} a -> s {returnValue = a} :: ModifyVpcEndpointServicePermissionsResponse)

-- | The response's http status code.
modifyVpcEndpointServicePermissionsResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse Prelude.Int
modifyVpcEndpointServicePermissionsResponse_httpStatus = Lens.lens (\ModifyVpcEndpointServicePermissionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointServicePermissionsResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointServicePermissionsResponse)

instance
  Prelude.NFData
    ModifyVpcEndpointServicePermissionsResponse
  where
  rnf ModifyVpcEndpointServicePermissionsResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus

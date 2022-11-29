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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the permissions for your VPC endpoint service. You can add or
-- remove permissions for service consumers (IAM users, IAM roles, and
-- Amazon Web Services accounts) to connect to your endpoint service.
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
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_serviceId,

    -- * Destructuring the Response
    ModifyVpcEndpointServicePermissionsResponse (..),
    newModifyVpcEndpointServicePermissionsResponse,

    -- * Response Lenses
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_addedPrincipals,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcEndpointServicePermissions' smart constructor.
data ModifyVpcEndpointServicePermissions = ModifyVpcEndpointServicePermissions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARN) of one or more principals. Permissions
    -- are granted to the principals in this list. To grant permissions to all
    -- principals, specify an asterisk (*).
    addAllowedPrincipals :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARN) of one or more principals. Permissions
    -- are revoked for principals in this list.
    removeAllowedPrincipals :: Prelude.Maybe [Prelude.Text],
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
-- 'dryRun', 'modifyVpcEndpointServicePermissions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'addAllowedPrincipals', 'modifyVpcEndpointServicePermissions_addAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are granted to the principals in this list. To grant permissions to all
-- principals, specify an asterisk (*).
--
-- 'removeAllowedPrincipals', 'modifyVpcEndpointServicePermissions_removeAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are revoked for principals in this list.
--
-- 'serviceId', 'modifyVpcEndpointServicePermissions_serviceId' - The ID of the service.
newModifyVpcEndpointServicePermissions ::
  -- | 'serviceId'
  Prelude.Text ->
  ModifyVpcEndpointServicePermissions
newModifyVpcEndpointServicePermissions pServiceId_ =
  ModifyVpcEndpointServicePermissions'
    { dryRun =
        Prelude.Nothing,
      addAllowedPrincipals = Prelude.Nothing,
      removeAllowedPrincipals =
        Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointServicePermissions_dryRun :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePermissions_dryRun = Lens.lens (\ModifyVpcEndpointServicePermissions' {dryRun} -> dryRun) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {dryRun = a} :: ModifyVpcEndpointServicePermissions)

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are granted to the principals in this list. To grant permissions to all
-- principals, specify an asterisk (*).
modifyVpcEndpointServicePermissions_addAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServicePermissions_addAllowedPrincipals = Lens.lens (\ModifyVpcEndpointServicePermissions' {addAllowedPrincipals} -> addAllowedPrincipals) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {addAllowedPrincipals = a} :: ModifyVpcEndpointServicePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions
-- are revoked for principals in this list.
modifyVpcEndpointServicePermissions_removeAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServicePermissions_removeAllowedPrincipals = Lens.lens (\ModifyVpcEndpointServicePermissions' {removeAllowedPrincipals} -> removeAllowedPrincipals) (\s@ModifyVpcEndpointServicePermissions' {} a -> s {removeAllowedPrincipals = a} :: ModifyVpcEndpointServicePermissions) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointServicePermissionsResponse'
            Prelude.<$> (x Core..@? "return")
              Prelude.<*> ( x Core..@? "addedPrincipalSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointServicePermissions
  where
  hashWithSalt
    _salt
    ModifyVpcEndpointServicePermissions' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` addAllowedPrincipals
        `Prelude.hashWithSalt` removeAllowedPrincipals
        `Prelude.hashWithSalt` serviceId

instance
  Prelude.NFData
    ModifyVpcEndpointServicePermissions
  where
  rnf ModifyVpcEndpointServicePermissions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf addAllowedPrincipals
      `Prelude.seq` Prelude.rnf removeAllowedPrincipals
      `Prelude.seq` Prelude.rnf serviceId

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
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "AddAllowedPrincipals"
              Prelude.<$> addAllowedPrincipals
          ),
        Core.toQuery
          ( Core.toQueryList "RemoveAllowedPrincipals"
              Prelude.<$> removeAllowedPrincipals
          ),
        "ServiceId" Core.=: serviceId
      ]

-- | /See:/ 'newModifyVpcEndpointServicePermissionsResponse' smart constructor.
data ModifyVpcEndpointServicePermissionsResponse = ModifyVpcEndpointServicePermissionsResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | Information about the added principals.
    addedPrincipals :: Prelude.Maybe [AddedPrincipal],
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
-- 'addedPrincipals', 'modifyVpcEndpointServicePermissionsResponse_addedPrincipals' - Information about the added principals.
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
        addedPrincipals =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointServicePermissionsResponse_returnValue :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePermissionsResponse_returnValue = Lens.lens (\ModifyVpcEndpointServicePermissionsResponse' {returnValue} -> returnValue) (\s@ModifyVpcEndpointServicePermissionsResponse' {} a -> s {returnValue = a} :: ModifyVpcEndpointServicePermissionsResponse)

-- | Information about the added principals.
modifyVpcEndpointServicePermissionsResponse_addedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse (Prelude.Maybe [AddedPrincipal])
modifyVpcEndpointServicePermissionsResponse_addedPrincipals = Lens.lens (\ModifyVpcEndpointServicePermissionsResponse' {addedPrincipals} -> addedPrincipals) (\s@ModifyVpcEndpointServicePermissionsResponse' {} a -> s {addedPrincipals = a} :: ModifyVpcEndpointServicePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyVpcEndpointServicePermissionsResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse Prelude.Int
modifyVpcEndpointServicePermissionsResponse_httpStatus = Lens.lens (\ModifyVpcEndpointServicePermissionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointServicePermissionsResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointServicePermissionsResponse)

instance
  Prelude.NFData
    ModifyVpcEndpointServicePermissionsResponse
  where
  rnf ModifyVpcEndpointServicePermissionsResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf addedPrincipals
      `Prelude.seq` Prelude.rnf httpStatus

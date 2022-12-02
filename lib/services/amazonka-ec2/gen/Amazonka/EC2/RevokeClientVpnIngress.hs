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
-- Module      : Amazonka.EC2.RevokeClientVpnIngress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an ingress authorization rule from a Client VPN endpoint.
module Amazonka.EC2.RevokeClientVpnIngress
  ( -- * Creating a Request
    RevokeClientVpnIngress (..),
    newRevokeClientVpnIngress,

    -- * Request Lenses
    revokeClientVpnIngress_revokeAllGroups,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_clientVpnEndpointId,
    revokeClientVpnIngress_targetNetworkCidr,

    -- * Destructuring the Response
    RevokeClientVpnIngressResponse (..),
    newRevokeClientVpnIngressResponse,

    -- * Response Lenses
    revokeClientVpnIngressResponse_status,
    revokeClientVpnIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeClientVpnIngress' smart constructor.
data RevokeClientVpnIngress = RevokeClientVpnIngress'
  { -- | Indicates whether access should be revoked for all clients.
    revokeAllGroups :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Active Directory group for which to revoke access.
    accessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint with which the authorization rule is
    -- associated.
    clientVpnEndpointId :: Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the network for which
    -- access is being removed.
    targetNetworkCidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeClientVpnIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokeAllGroups', 'revokeClientVpnIngress_revokeAllGroups' - Indicates whether access should be revoked for all clients.
--
-- 'dryRun', 'revokeClientVpnIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'accessGroupId', 'revokeClientVpnIngress_accessGroupId' - The ID of the Active Directory group for which to revoke access.
--
-- 'clientVpnEndpointId', 'revokeClientVpnIngress_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
--
-- 'targetNetworkCidr', 'revokeClientVpnIngress_targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which
-- access is being removed.
newRevokeClientVpnIngress ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  -- | 'targetNetworkCidr'
  Prelude.Text ->
  RevokeClientVpnIngress
newRevokeClientVpnIngress
  pClientVpnEndpointId_
  pTargetNetworkCidr_ =
    RevokeClientVpnIngress'
      { revokeAllGroups =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        accessGroupId = Prelude.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        targetNetworkCidr = pTargetNetworkCidr_
      }

-- | Indicates whether access should be revoked for all clients.
revokeClientVpnIngress_revokeAllGroups :: Lens.Lens' RevokeClientVpnIngress (Prelude.Maybe Prelude.Bool)
revokeClientVpnIngress_revokeAllGroups = Lens.lens (\RevokeClientVpnIngress' {revokeAllGroups} -> revokeAllGroups) (\s@RevokeClientVpnIngress' {} a -> s {revokeAllGroups = a} :: RevokeClientVpnIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
revokeClientVpnIngress_dryRun :: Lens.Lens' RevokeClientVpnIngress (Prelude.Maybe Prelude.Bool)
revokeClientVpnIngress_dryRun = Lens.lens (\RevokeClientVpnIngress' {dryRun} -> dryRun) (\s@RevokeClientVpnIngress' {} a -> s {dryRun = a} :: RevokeClientVpnIngress)

-- | The ID of the Active Directory group for which to revoke access.
revokeClientVpnIngress_accessGroupId :: Lens.Lens' RevokeClientVpnIngress (Prelude.Maybe Prelude.Text)
revokeClientVpnIngress_accessGroupId = Lens.lens (\RevokeClientVpnIngress' {accessGroupId} -> accessGroupId) (\s@RevokeClientVpnIngress' {} a -> s {accessGroupId = a} :: RevokeClientVpnIngress)

-- | The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
revokeClientVpnIngress_clientVpnEndpointId :: Lens.Lens' RevokeClientVpnIngress Prelude.Text
revokeClientVpnIngress_clientVpnEndpointId = Lens.lens (\RevokeClientVpnIngress' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@RevokeClientVpnIngress' {} a -> s {clientVpnEndpointId = a} :: RevokeClientVpnIngress)

-- | The IPv4 address range, in CIDR notation, of the network for which
-- access is being removed.
revokeClientVpnIngress_targetNetworkCidr :: Lens.Lens' RevokeClientVpnIngress Prelude.Text
revokeClientVpnIngress_targetNetworkCidr = Lens.lens (\RevokeClientVpnIngress' {targetNetworkCidr} -> targetNetworkCidr) (\s@RevokeClientVpnIngress' {} a -> s {targetNetworkCidr = a} :: RevokeClientVpnIngress)

instance Core.AWSRequest RevokeClientVpnIngress where
  type
    AWSResponse RevokeClientVpnIngress =
      RevokeClientVpnIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeClientVpnIngressResponse'
            Prelude.<$> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeClientVpnIngress where
  hashWithSalt _salt RevokeClientVpnIngress' {..} =
    _salt `Prelude.hashWithSalt` revokeAllGroups
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` accessGroupId
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` targetNetworkCidr

instance Prelude.NFData RevokeClientVpnIngress where
  rnf RevokeClientVpnIngress' {..} =
    Prelude.rnf revokeAllGroups
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf accessGroupId
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf targetNetworkCidr

instance Data.ToHeaders RevokeClientVpnIngress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RevokeClientVpnIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeClientVpnIngress where
  toQuery RevokeClientVpnIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RevokeClientVpnIngress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "RevokeAllGroups" Data.=: revokeAllGroups,
        "DryRun" Data.=: dryRun,
        "AccessGroupId" Data.=: accessGroupId,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId,
        "TargetNetworkCidr" Data.=: targetNetworkCidr
      ]

-- | /See:/ 'newRevokeClientVpnIngressResponse' smart constructor.
data RevokeClientVpnIngressResponse = RevokeClientVpnIngressResponse'
  { -- | The current state of the authorization rule.
    status :: Prelude.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeClientVpnIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'revokeClientVpnIngressResponse_status' - The current state of the authorization rule.
--
-- 'httpStatus', 'revokeClientVpnIngressResponse_httpStatus' - The response's http status code.
newRevokeClientVpnIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeClientVpnIngressResponse
newRevokeClientVpnIngressResponse pHttpStatus_ =
  RevokeClientVpnIngressResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the authorization rule.
revokeClientVpnIngressResponse_status :: Lens.Lens' RevokeClientVpnIngressResponse (Prelude.Maybe ClientVpnAuthorizationRuleStatus)
revokeClientVpnIngressResponse_status = Lens.lens (\RevokeClientVpnIngressResponse' {status} -> status) (\s@RevokeClientVpnIngressResponse' {} a -> s {status = a} :: RevokeClientVpnIngressResponse)

-- | The response's http status code.
revokeClientVpnIngressResponse_httpStatus :: Lens.Lens' RevokeClientVpnIngressResponse Prelude.Int
revokeClientVpnIngressResponse_httpStatus = Lens.lens (\RevokeClientVpnIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeClientVpnIngressResponse' {} a -> s {httpStatus = a} :: RevokeClientVpnIngressResponse)

instance
  Prelude.NFData
    RevokeClientVpnIngressResponse
  where
  rnf RevokeClientVpnIngressResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.EC2.RevokeClientVpnIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an ingress authorization rule from a Client VPN endpoint.
module Network.AWS.EC2.RevokeClientVpnIngress
  ( -- * Creating a Request
    RevokeClientVpnIngress (..),
    newRevokeClientVpnIngress,

    -- * Request Lenses
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_revokeAllGroups,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRevokeClientVpnIngress' smart constructor.
data RevokeClientVpnIngress = RevokeClientVpnIngress'
  { -- | The ID of the Active Directory group for which to revoke access.
    accessGroupId :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether access should be revoked for all clients.
    revokeAllGroups :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint with which the authorization rule is
    -- associated.
    clientVpnEndpointId :: Core.Text,
    -- | The IPv4 address range, in CIDR notation, of the network for which
    -- access is being removed.
    targetNetworkCidr :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeClientVpnIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessGroupId', 'revokeClientVpnIngress_accessGroupId' - The ID of the Active Directory group for which to revoke access.
--
-- 'dryRun', 'revokeClientVpnIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'revokeAllGroups', 'revokeClientVpnIngress_revokeAllGroups' - Indicates whether access should be revoked for all clients.
--
-- 'clientVpnEndpointId', 'revokeClientVpnIngress_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
--
-- 'targetNetworkCidr', 'revokeClientVpnIngress_targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which
-- access is being removed.
newRevokeClientVpnIngress ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  -- | 'targetNetworkCidr'
  Core.Text ->
  RevokeClientVpnIngress
newRevokeClientVpnIngress
  pClientVpnEndpointId_
  pTargetNetworkCidr_ =
    RevokeClientVpnIngress'
      { accessGroupId =
          Core.Nothing,
        dryRun = Core.Nothing,
        revokeAllGroups = Core.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        targetNetworkCidr = pTargetNetworkCidr_
      }

-- | The ID of the Active Directory group for which to revoke access.
revokeClientVpnIngress_accessGroupId :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Text)
revokeClientVpnIngress_accessGroupId = Lens.lens (\RevokeClientVpnIngress' {accessGroupId} -> accessGroupId) (\s@RevokeClientVpnIngress' {} a -> s {accessGroupId = a} :: RevokeClientVpnIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
revokeClientVpnIngress_dryRun :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Bool)
revokeClientVpnIngress_dryRun = Lens.lens (\RevokeClientVpnIngress' {dryRun} -> dryRun) (\s@RevokeClientVpnIngress' {} a -> s {dryRun = a} :: RevokeClientVpnIngress)

-- | Indicates whether access should be revoked for all clients.
revokeClientVpnIngress_revokeAllGroups :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Bool)
revokeClientVpnIngress_revokeAllGroups = Lens.lens (\RevokeClientVpnIngress' {revokeAllGroups} -> revokeAllGroups) (\s@RevokeClientVpnIngress' {} a -> s {revokeAllGroups = a} :: RevokeClientVpnIngress)

-- | The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
revokeClientVpnIngress_clientVpnEndpointId :: Lens.Lens' RevokeClientVpnIngress Core.Text
revokeClientVpnIngress_clientVpnEndpointId = Lens.lens (\RevokeClientVpnIngress' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@RevokeClientVpnIngress' {} a -> s {clientVpnEndpointId = a} :: RevokeClientVpnIngress)

-- | The IPv4 address range, in CIDR notation, of the network for which
-- access is being removed.
revokeClientVpnIngress_targetNetworkCidr :: Lens.Lens' RevokeClientVpnIngress Core.Text
revokeClientVpnIngress_targetNetworkCidr = Lens.lens (\RevokeClientVpnIngress' {targetNetworkCidr} -> targetNetworkCidr) (\s@RevokeClientVpnIngress' {} a -> s {targetNetworkCidr = a} :: RevokeClientVpnIngress)

instance Core.AWSRequest RevokeClientVpnIngress where
  type
    AWSResponse RevokeClientVpnIngress =
      RevokeClientVpnIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeClientVpnIngressResponse'
            Core.<$> (x Core..@? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RevokeClientVpnIngress

instance Core.NFData RevokeClientVpnIngress

instance Core.ToHeaders RevokeClientVpnIngress where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RevokeClientVpnIngress where
  toPath = Core.const "/"

instance Core.ToQuery RevokeClientVpnIngress where
  toQuery RevokeClientVpnIngress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RevokeClientVpnIngress" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "AccessGroupId" Core.=: accessGroupId,
        "DryRun" Core.=: dryRun,
        "RevokeAllGroups" Core.=: revokeAllGroups,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId,
        "TargetNetworkCidr" Core.=: targetNetworkCidr
      ]

-- | /See:/ 'newRevokeClientVpnIngressResponse' smart constructor.
data RevokeClientVpnIngressResponse = RevokeClientVpnIngressResponse'
  { -- | The current state of the authorization rule.
    status :: Core.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RevokeClientVpnIngressResponse
newRevokeClientVpnIngressResponse pHttpStatus_ =
  RevokeClientVpnIngressResponse'
    { status =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the authorization rule.
revokeClientVpnIngressResponse_status :: Lens.Lens' RevokeClientVpnIngressResponse (Core.Maybe ClientVpnAuthorizationRuleStatus)
revokeClientVpnIngressResponse_status = Lens.lens (\RevokeClientVpnIngressResponse' {status} -> status) (\s@RevokeClientVpnIngressResponse' {} a -> s {status = a} :: RevokeClientVpnIngressResponse)

-- | The response's http status code.
revokeClientVpnIngressResponse_httpStatus :: Lens.Lens' RevokeClientVpnIngressResponse Core.Int
revokeClientVpnIngressResponse_httpStatus = Lens.lens (\RevokeClientVpnIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeClientVpnIngressResponse' {} a -> s {httpStatus = a} :: RevokeClientVpnIngressResponse)

instance Core.NFData RevokeClientVpnIngressResponse

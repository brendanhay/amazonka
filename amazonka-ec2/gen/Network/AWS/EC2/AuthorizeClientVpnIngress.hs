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
-- Module      : Network.AWS.EC2.AuthorizeClientVpnIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an ingress authorization rule to a Client VPN endpoint. Ingress
-- authorization rules act as firewall rules that grant access to networks.
-- You must configure ingress authorization rules to enable clients to
-- access resources in AWS or on-premises networks.
module Network.AWS.EC2.AuthorizeClientVpnIngress
  ( -- * Creating a Request
    AuthorizeClientVpnIngress (..),
    newAuthorizeClientVpnIngress,

    -- * Request Lenses
    authorizeClientVpnIngress_accessGroupId,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_clientVpnEndpointId,
    authorizeClientVpnIngress_targetNetworkCidr,

    -- * Destructuring the Response
    AuthorizeClientVpnIngressResponse (..),
    newAuthorizeClientVpnIngressResponse,

    -- * Response Lenses
    authorizeClientVpnIngressResponse_status,
    authorizeClientVpnIngressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAuthorizeClientVpnIngress' smart constructor.
data AuthorizeClientVpnIngress = AuthorizeClientVpnIngress'
  { -- | The ID of the group to grant access to, for example, the Active
    -- Directory group or identity provider (IdP) group. Required if
    -- @AuthorizeAllGroups@ is @false@ or not specified.
    accessGroupId :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether to grant access to all clients. Specify @true@ to
    -- grant all clients who successfully establish a VPN connection access to
    -- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
    authorizeAllGroups :: Core.Maybe Core.Bool,
    -- | A brief description of the authorization rule.
    description :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text,
    -- | The IPv4 address range, in CIDR notation, of the network for which
    -- access is being authorized.
    targetNetworkCidr :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeClientVpnIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessGroupId', 'authorizeClientVpnIngress_accessGroupId' - The ID of the group to grant access to, for example, the Active
-- Directory group or identity provider (IdP) group. Required if
-- @AuthorizeAllGroups@ is @false@ or not specified.
--
-- 'dryRun', 'authorizeClientVpnIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'authorizeAllGroups', 'authorizeClientVpnIngress_authorizeAllGroups' - Indicates whether to grant access to all clients. Specify @true@ to
-- grant all clients who successfully establish a VPN connection access to
-- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
--
-- 'description', 'authorizeClientVpnIngress_description' - A brief description of the authorization rule.
--
-- 'clientToken', 'authorizeClientVpnIngress_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'clientVpnEndpointId', 'authorizeClientVpnIngress_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'targetNetworkCidr', 'authorizeClientVpnIngress_targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which
-- access is being authorized.
newAuthorizeClientVpnIngress ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  -- | 'targetNetworkCidr'
  Core.Text ->
  AuthorizeClientVpnIngress
newAuthorizeClientVpnIngress
  pClientVpnEndpointId_
  pTargetNetworkCidr_ =
    AuthorizeClientVpnIngress'
      { accessGroupId =
          Core.Nothing,
        dryRun = Core.Nothing,
        authorizeAllGroups = Core.Nothing,
        description = Core.Nothing,
        clientToken = Core.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        targetNetworkCidr = pTargetNetworkCidr_
      }

-- | The ID of the group to grant access to, for example, the Active
-- Directory group or identity provider (IdP) group. Required if
-- @AuthorizeAllGroups@ is @false@ or not specified.
authorizeClientVpnIngress_accessGroupId :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
authorizeClientVpnIngress_accessGroupId = Lens.lens (\AuthorizeClientVpnIngress' {accessGroupId} -> accessGroupId) (\s@AuthorizeClientVpnIngress' {} a -> s {accessGroupId = a} :: AuthorizeClientVpnIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeClientVpnIngress_dryRun :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Bool)
authorizeClientVpnIngress_dryRun = Lens.lens (\AuthorizeClientVpnIngress' {dryRun} -> dryRun) (\s@AuthorizeClientVpnIngress' {} a -> s {dryRun = a} :: AuthorizeClientVpnIngress)

-- | Indicates whether to grant access to all clients. Specify @true@ to
-- grant all clients who successfully establish a VPN connection access to
-- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
authorizeClientVpnIngress_authorizeAllGroups :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Bool)
authorizeClientVpnIngress_authorizeAllGroups = Lens.lens (\AuthorizeClientVpnIngress' {authorizeAllGroups} -> authorizeAllGroups) (\s@AuthorizeClientVpnIngress' {} a -> s {authorizeAllGroups = a} :: AuthorizeClientVpnIngress)

-- | A brief description of the authorization rule.
authorizeClientVpnIngress_description :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
authorizeClientVpnIngress_description = Lens.lens (\AuthorizeClientVpnIngress' {description} -> description) (\s@AuthorizeClientVpnIngress' {} a -> s {description = a} :: AuthorizeClientVpnIngress)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
authorizeClientVpnIngress_clientToken :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
authorizeClientVpnIngress_clientToken = Lens.lens (\AuthorizeClientVpnIngress' {clientToken} -> clientToken) (\s@AuthorizeClientVpnIngress' {} a -> s {clientToken = a} :: AuthorizeClientVpnIngress)

-- | The ID of the Client VPN endpoint.
authorizeClientVpnIngress_clientVpnEndpointId :: Lens.Lens' AuthorizeClientVpnIngress Core.Text
authorizeClientVpnIngress_clientVpnEndpointId = Lens.lens (\AuthorizeClientVpnIngress' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@AuthorizeClientVpnIngress' {} a -> s {clientVpnEndpointId = a} :: AuthorizeClientVpnIngress)

-- | The IPv4 address range, in CIDR notation, of the network for which
-- access is being authorized.
authorizeClientVpnIngress_targetNetworkCidr :: Lens.Lens' AuthorizeClientVpnIngress Core.Text
authorizeClientVpnIngress_targetNetworkCidr = Lens.lens (\AuthorizeClientVpnIngress' {targetNetworkCidr} -> targetNetworkCidr) (\s@AuthorizeClientVpnIngress' {} a -> s {targetNetworkCidr = a} :: AuthorizeClientVpnIngress)

instance Core.AWSRequest AuthorizeClientVpnIngress where
  type
    AWSResponse AuthorizeClientVpnIngress =
      AuthorizeClientVpnIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AuthorizeClientVpnIngressResponse'
            Core.<$> (x Core..@? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AuthorizeClientVpnIngress

instance Core.NFData AuthorizeClientVpnIngress

instance Core.ToHeaders AuthorizeClientVpnIngress where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AuthorizeClientVpnIngress where
  toPath = Core.const "/"

instance Core.ToQuery AuthorizeClientVpnIngress where
  toQuery AuthorizeClientVpnIngress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AuthorizeClientVpnIngress" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "AccessGroupId" Core.=: accessGroupId,
        "DryRun" Core.=: dryRun,
        "AuthorizeAllGroups" Core.=: authorizeAllGroups,
        "Description" Core.=: description,
        "ClientToken" Core.=: clientToken,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId,
        "TargetNetworkCidr" Core.=: targetNetworkCidr
      ]

-- | /See:/ 'newAuthorizeClientVpnIngressResponse' smart constructor.
data AuthorizeClientVpnIngressResponse = AuthorizeClientVpnIngressResponse'
  { -- | The current state of the authorization rule.
    status :: Core.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeClientVpnIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'authorizeClientVpnIngressResponse_status' - The current state of the authorization rule.
--
-- 'httpStatus', 'authorizeClientVpnIngressResponse_httpStatus' - The response's http status code.
newAuthorizeClientVpnIngressResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AuthorizeClientVpnIngressResponse
newAuthorizeClientVpnIngressResponse pHttpStatus_ =
  AuthorizeClientVpnIngressResponse'
    { status =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the authorization rule.
authorizeClientVpnIngressResponse_status :: Lens.Lens' AuthorizeClientVpnIngressResponse (Core.Maybe ClientVpnAuthorizationRuleStatus)
authorizeClientVpnIngressResponse_status = Lens.lens (\AuthorizeClientVpnIngressResponse' {status} -> status) (\s@AuthorizeClientVpnIngressResponse' {} a -> s {status = a} :: AuthorizeClientVpnIngressResponse)

-- | The response's http status code.
authorizeClientVpnIngressResponse_httpStatus :: Lens.Lens' AuthorizeClientVpnIngressResponse Core.Int
authorizeClientVpnIngressResponse_httpStatus = Lens.lens (\AuthorizeClientVpnIngressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeClientVpnIngressResponse' {} a -> s {httpStatus = a} :: AuthorizeClientVpnIngressResponse)

instance
  Core.NFData
    AuthorizeClientVpnIngressResponse

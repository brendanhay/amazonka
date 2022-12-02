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
-- Module      : Amazonka.EC2.AuthorizeClientVpnIngress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an ingress authorization rule to a Client VPN endpoint. Ingress
-- authorization rules act as firewall rules that grant access to networks.
-- You must configure ingress authorization rules to enable clients to
-- access resources in Amazon Web Services or on-premises networks.
module Amazonka.EC2.AuthorizeClientVpnIngress
  ( -- * Creating a Request
    AuthorizeClientVpnIngress (..),
    newAuthorizeClientVpnIngress,

    -- * Request Lenses
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_accessGroupId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAuthorizeClientVpnIngress' smart constructor.
data AuthorizeClientVpnIngress = AuthorizeClientVpnIngress'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to grant access to all clients. Specify @true@ to
    -- grant all clients who successfully establish a VPN connection access to
    -- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
    authorizeAllGroups :: Prelude.Maybe Prelude.Bool,
    -- | A brief description of the authorization rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the group to grant access to, for example, the Active
    -- Directory group or identity provider (IdP) group. Required if
    -- @AuthorizeAllGroups@ is @false@ or not specified.
    accessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the network for which
    -- access is being authorized.
    targetNetworkCidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeClientVpnIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'authorizeClientVpnIngress_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'authorizeAllGroups', 'authorizeClientVpnIngress_authorizeAllGroups' - Indicates whether to grant access to all clients. Specify @true@ to
-- grant all clients who successfully establish a VPN connection access to
-- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
--
-- 'description', 'authorizeClientVpnIngress_description' - A brief description of the authorization rule.
--
-- 'dryRun', 'authorizeClientVpnIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'accessGroupId', 'authorizeClientVpnIngress_accessGroupId' - The ID of the group to grant access to, for example, the Active
-- Directory group or identity provider (IdP) group. Required if
-- @AuthorizeAllGroups@ is @false@ or not specified.
--
-- 'clientVpnEndpointId', 'authorizeClientVpnIngress_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'targetNetworkCidr', 'authorizeClientVpnIngress_targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which
-- access is being authorized.
newAuthorizeClientVpnIngress ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  -- | 'targetNetworkCidr'
  Prelude.Text ->
  AuthorizeClientVpnIngress
newAuthorizeClientVpnIngress
  pClientVpnEndpointId_
  pTargetNetworkCidr_ =
    AuthorizeClientVpnIngress'
      { clientToken =
          Prelude.Nothing,
        authorizeAllGroups = Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        accessGroupId = Prelude.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        targetNetworkCidr = pTargetNetworkCidr_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
authorizeClientVpnIngress_clientToken :: Lens.Lens' AuthorizeClientVpnIngress (Prelude.Maybe Prelude.Text)
authorizeClientVpnIngress_clientToken = Lens.lens (\AuthorizeClientVpnIngress' {clientToken} -> clientToken) (\s@AuthorizeClientVpnIngress' {} a -> s {clientToken = a} :: AuthorizeClientVpnIngress)

-- | Indicates whether to grant access to all clients. Specify @true@ to
-- grant all clients who successfully establish a VPN connection access to
-- the network. Must be set to @true@ if @AccessGroupId@ is not specified.
authorizeClientVpnIngress_authorizeAllGroups :: Lens.Lens' AuthorizeClientVpnIngress (Prelude.Maybe Prelude.Bool)
authorizeClientVpnIngress_authorizeAllGroups = Lens.lens (\AuthorizeClientVpnIngress' {authorizeAllGroups} -> authorizeAllGroups) (\s@AuthorizeClientVpnIngress' {} a -> s {authorizeAllGroups = a} :: AuthorizeClientVpnIngress)

-- | A brief description of the authorization rule.
authorizeClientVpnIngress_description :: Lens.Lens' AuthorizeClientVpnIngress (Prelude.Maybe Prelude.Text)
authorizeClientVpnIngress_description = Lens.lens (\AuthorizeClientVpnIngress' {description} -> description) (\s@AuthorizeClientVpnIngress' {} a -> s {description = a} :: AuthorizeClientVpnIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeClientVpnIngress_dryRun :: Lens.Lens' AuthorizeClientVpnIngress (Prelude.Maybe Prelude.Bool)
authorizeClientVpnIngress_dryRun = Lens.lens (\AuthorizeClientVpnIngress' {dryRun} -> dryRun) (\s@AuthorizeClientVpnIngress' {} a -> s {dryRun = a} :: AuthorizeClientVpnIngress)

-- | The ID of the group to grant access to, for example, the Active
-- Directory group or identity provider (IdP) group. Required if
-- @AuthorizeAllGroups@ is @false@ or not specified.
authorizeClientVpnIngress_accessGroupId :: Lens.Lens' AuthorizeClientVpnIngress (Prelude.Maybe Prelude.Text)
authorizeClientVpnIngress_accessGroupId = Lens.lens (\AuthorizeClientVpnIngress' {accessGroupId} -> accessGroupId) (\s@AuthorizeClientVpnIngress' {} a -> s {accessGroupId = a} :: AuthorizeClientVpnIngress)

-- | The ID of the Client VPN endpoint.
authorizeClientVpnIngress_clientVpnEndpointId :: Lens.Lens' AuthorizeClientVpnIngress Prelude.Text
authorizeClientVpnIngress_clientVpnEndpointId = Lens.lens (\AuthorizeClientVpnIngress' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@AuthorizeClientVpnIngress' {} a -> s {clientVpnEndpointId = a} :: AuthorizeClientVpnIngress)

-- | The IPv4 address range, in CIDR notation, of the network for which
-- access is being authorized.
authorizeClientVpnIngress_targetNetworkCidr :: Lens.Lens' AuthorizeClientVpnIngress Prelude.Text
authorizeClientVpnIngress_targetNetworkCidr = Lens.lens (\AuthorizeClientVpnIngress' {targetNetworkCidr} -> targetNetworkCidr) (\s@AuthorizeClientVpnIngress' {} a -> s {targetNetworkCidr = a} :: AuthorizeClientVpnIngress)

instance Core.AWSRequest AuthorizeClientVpnIngress where
  type
    AWSResponse AuthorizeClientVpnIngress =
      AuthorizeClientVpnIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AuthorizeClientVpnIngressResponse'
            Prelude.<$> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AuthorizeClientVpnIngress where
  hashWithSalt _salt AuthorizeClientVpnIngress' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` authorizeAllGroups
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` accessGroupId
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` targetNetworkCidr

instance Prelude.NFData AuthorizeClientVpnIngress where
  rnf AuthorizeClientVpnIngress' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf authorizeAllGroups
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf accessGroupId
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf targetNetworkCidr

instance Data.ToHeaders AuthorizeClientVpnIngress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AuthorizeClientVpnIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeClientVpnIngress where
  toQuery AuthorizeClientVpnIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AuthorizeClientVpnIngress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "AuthorizeAllGroups" Data.=: authorizeAllGroups,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "AccessGroupId" Data.=: accessGroupId,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId,
        "TargetNetworkCidr" Data.=: targetNetworkCidr
      ]

-- | /See:/ 'newAuthorizeClientVpnIngressResponse' smart constructor.
data AuthorizeClientVpnIngressResponse = AuthorizeClientVpnIngressResponse'
  { -- | The current state of the authorization rule.
    status :: Prelude.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AuthorizeClientVpnIngressResponse
newAuthorizeClientVpnIngressResponse pHttpStatus_ =
  AuthorizeClientVpnIngressResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the authorization rule.
authorizeClientVpnIngressResponse_status :: Lens.Lens' AuthorizeClientVpnIngressResponse (Prelude.Maybe ClientVpnAuthorizationRuleStatus)
authorizeClientVpnIngressResponse_status = Lens.lens (\AuthorizeClientVpnIngressResponse' {status} -> status) (\s@AuthorizeClientVpnIngressResponse' {} a -> s {status = a} :: AuthorizeClientVpnIngressResponse)

-- | The response's http status code.
authorizeClientVpnIngressResponse_httpStatus :: Lens.Lens' AuthorizeClientVpnIngressResponse Prelude.Int
authorizeClientVpnIngressResponse_httpStatus = Lens.lens (\AuthorizeClientVpnIngressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeClientVpnIngressResponse' {} a -> s {httpStatus = a} :: AuthorizeClientVpnIngressResponse)

instance
  Prelude.NFData
    AuthorizeClientVpnIngressResponse
  where
  rnf AuthorizeClientVpnIngressResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

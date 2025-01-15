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
-- Module      : Amazonka.NetworkFirewall.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an IAM policy for your rule group or firewall policy.
-- Use this to share rule groups and firewall policies between accounts.
-- This operation works in conjunction with the Amazon Web Services
-- Resource Access Manager (RAM) service to manage resource sharing for
-- Network Firewall.
--
-- Use this operation to create or update a resource policy for your rule
-- group or firewall policy. In the policy, you specify the accounts that
-- you want to share the resource with and the operations that you want the
-- accounts to be able to perform.
--
-- When you add an account in the resource policy, you then run the
-- following Resource Access Manager (RAM) operations to access and accept
-- the shared rule group or firewall policy.
--
-- -   <https://docs.aws.amazon.com/ram/latest/APIReference/API_GetResourceShareInvitations.html GetResourceShareInvitations>
--     - Returns the Amazon Resource Names (ARNs) of the resource share
--     invitations.
--
-- -   <https://docs.aws.amazon.com/ram/latest/APIReference/API_AcceptResourceShareInvitation.html AcceptResourceShareInvitation>
--     - Accepts the share invitation for a specified resource share.
--
-- For additional information about resource sharing using RAM, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/what-is.html Resource Access Manager User Guide>.
module Amazonka.NetworkFirewall.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the account that you want to share
    -- rule groups and firewall policies with.
    resourceArn :: Prelude.Text,
    -- | The IAM policy statement that lists the accounts that you want to share
    -- your rule group or firewall policy with and the operations that you want
    -- the accounts to be able to perform.
    --
    -- For a rule group resource, you can specify the following operations in
    -- the Actions section of the statement:
    --
    -- -   network-firewall:CreateFirewallPolicy
    --
    -- -   network-firewall:UpdateFirewallPolicy
    --
    -- -   network-firewall:ListRuleGroups
    --
    -- For a firewall policy resource, you can specify the following operations
    -- in the Actions section of the statement:
    --
    -- -   network-firewall:CreateFirewall
    --
    -- -   network-firewall:UpdateFirewall
    --
    -- -   network-firewall:AssociateFirewallPolicy
    --
    -- -   network-firewall:ListFirewallPolicies
    --
    -- In the Resource section of the statement, you specify the ARNs for the
    -- rule groups and firewall policies that you want to share with the
    -- account that you specified in @Arn@.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the account that you want to share
-- rule groups and firewall policies with.
--
-- 'policy', 'putResourcePolicy_policy' - The IAM policy statement that lists the accounts that you want to share
-- your rule group or firewall policy with and the operations that you want
-- the accounts to be able to perform.
--
-- For a rule group resource, you can specify the following operations in
-- the Actions section of the statement:
--
-- -   network-firewall:CreateFirewallPolicy
--
-- -   network-firewall:UpdateFirewallPolicy
--
-- -   network-firewall:ListRuleGroups
--
-- For a firewall policy resource, you can specify the following operations
-- in the Actions section of the statement:
--
-- -   network-firewall:CreateFirewall
--
-- -   network-firewall:UpdateFirewall
--
-- -   network-firewall:AssociateFirewallPolicy
--
-- -   network-firewall:ListFirewallPolicies
--
-- In the Resource section of the statement, you specify the ARNs for the
-- rule groups and firewall policies that you want to share with the
-- account that you specified in @Arn@.
newPutResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pResourceArn_ pPolicy_ =
  PutResourcePolicy'
    { resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the account that you want to share
-- rule groups and firewall policies with.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | The IAM policy statement that lists the accounts that you want to share
-- your rule group or firewall policy with and the operations that you want
-- the accounts to be able to perform.
--
-- For a rule group resource, you can specify the following operations in
-- the Actions section of the statement:
--
-- -   network-firewall:CreateFirewallPolicy
--
-- -   network-firewall:UpdateFirewallPolicy
--
-- -   network-firewall:ListRuleGroups
--
-- For a firewall policy resource, you can specify the following operations
-- in the Actions section of the statement:
--
-- -   network-firewall:CreateFirewall
--
-- -   network-firewall:UpdateFirewall
--
-- -   network-firewall:AssociateFirewallPolicy
--
-- -   network-firewall:ListFirewallPolicies
--
-- In the Resource section of the statement, you specify the ARNs for the
-- rule groups and firewall policies that you want to share with the
-- account that you specified in @Arn@.
putResourcePolicy_policy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policy = Lens.lens (\PutResourcePolicy' {policy} -> policy) (\s@PutResourcePolicy' {} a -> s {policy = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf resourceArn `Prelude.seq`
      Prelude.rnf policy

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf httpStatus

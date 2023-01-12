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
-- Module      : Amazonka.NetworkFirewall.DescribeFirewallPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data objects for the specified firewall policy.
module Amazonka.NetworkFirewall.DescribeFirewallPolicy
  ( -- * Creating a Request
    DescribeFirewallPolicy (..),
    newDescribeFirewallPolicy,

    -- * Request Lenses
    describeFirewallPolicy_firewallPolicyArn,
    describeFirewallPolicy_firewallPolicyName,

    -- * Destructuring the Response
    DescribeFirewallPolicyResponse (..),
    newDescribeFirewallPolicyResponse,

    -- * Response Lenses
    describeFirewallPolicyResponse_firewallPolicy,
    describeFirewallPolicyResponse_httpStatus,
    describeFirewallPolicyResponse_updateToken,
    describeFirewallPolicyResponse_firewallPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFirewallPolicy' smart constructor.
data DescribeFirewallPolicy = DescribeFirewallPolicy'
  { -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallPolicyArn', 'describeFirewallPolicy_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallPolicyName', 'describeFirewallPolicy_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDescribeFirewallPolicy ::
  DescribeFirewallPolicy
newDescribeFirewallPolicy =
  DescribeFirewallPolicy'
    { firewallPolicyArn =
        Prelude.Nothing,
      firewallPolicyName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
describeFirewallPolicy_firewallPolicyArn :: Lens.Lens' DescribeFirewallPolicy (Prelude.Maybe Prelude.Text)
describeFirewallPolicy_firewallPolicyArn = Lens.lens (\DescribeFirewallPolicy' {firewallPolicyArn} -> firewallPolicyArn) (\s@DescribeFirewallPolicy' {} a -> s {firewallPolicyArn = a} :: DescribeFirewallPolicy)

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeFirewallPolicy_firewallPolicyName :: Lens.Lens' DescribeFirewallPolicy (Prelude.Maybe Prelude.Text)
describeFirewallPolicy_firewallPolicyName = Lens.lens (\DescribeFirewallPolicy' {firewallPolicyName} -> firewallPolicyName) (\s@DescribeFirewallPolicy' {} a -> s {firewallPolicyName = a} :: DescribeFirewallPolicy)

instance Core.AWSRequest DescribeFirewallPolicy where
  type
    AWSResponse DescribeFirewallPolicy =
      DescribeFirewallPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFirewallPolicyResponse'
            Prelude.<$> (x Data..?> "FirewallPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "FirewallPolicyResponse")
      )

instance Prelude.Hashable DescribeFirewallPolicy where
  hashWithSalt _salt DescribeFirewallPolicy' {..} =
    _salt `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` firewallPolicyName

instance Prelude.NFData DescribeFirewallPolicy where
  rnf DescribeFirewallPolicy' {..} =
    Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf firewallPolicyName

instance Data.ToHeaders DescribeFirewallPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DescribeFirewallPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFirewallPolicy where
  toJSON DescribeFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn,
            ("FirewallPolicyName" Data..=)
              Prelude.<$> firewallPolicyName
          ]
      )

instance Data.ToPath DescribeFirewallPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFirewallPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFirewallPolicyResponse' smart constructor.
data DescribeFirewallPolicyResponse = DescribeFirewallPolicyResponse'
  { -- | The policy for the specified firewall policy.
    firewallPolicy :: Prelude.Maybe FirewallPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the firewall policy. The token marks the state
    -- of the policy resource at the time of the request.
    --
    -- To make changes to the policy, you provide the token in your request.
    -- Network Firewall uses the token to ensure that the policy hasn\'t
    -- changed since you last retrieved it. If it has changed, the operation
    -- fails with an @InvalidTokenException@. If this happens, retrieve the
    -- firewall policy again to get a current copy of it with current token.
    -- Reapply your changes as needed, then try the operation again using the
    -- new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a firewall policy. This, along with the
    -- FirewallPolicy, define the policy. You can retrieve all objects for a
    -- firewall policy by calling DescribeFirewallPolicy.
    firewallPolicyResponse :: FirewallPolicyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallPolicy', 'describeFirewallPolicyResponse_firewallPolicy' - The policy for the specified firewall policy.
--
-- 'httpStatus', 'describeFirewallPolicyResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'describeFirewallPolicyResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
--
-- 'firewallPolicyResponse', 'describeFirewallPolicyResponse_firewallPolicyResponse' - The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
newDescribeFirewallPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'firewallPolicyResponse'
  FirewallPolicyResponse ->
  DescribeFirewallPolicyResponse
newDescribeFirewallPolicyResponse
  pHttpStatus_
  pUpdateToken_
  pFirewallPolicyResponse_ =
    DescribeFirewallPolicyResponse'
      { firewallPolicy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        updateToken = pUpdateToken_,
        firewallPolicyResponse =
          pFirewallPolicyResponse_
      }

-- | The policy for the specified firewall policy.
describeFirewallPolicyResponse_firewallPolicy :: Lens.Lens' DescribeFirewallPolicyResponse (Prelude.Maybe FirewallPolicy)
describeFirewallPolicyResponse_firewallPolicy = Lens.lens (\DescribeFirewallPolicyResponse' {firewallPolicy} -> firewallPolicy) (\s@DescribeFirewallPolicyResponse' {} a -> s {firewallPolicy = a} :: DescribeFirewallPolicyResponse)

-- | The response's http status code.
describeFirewallPolicyResponse_httpStatus :: Lens.Lens' DescribeFirewallPolicyResponse Prelude.Int
describeFirewallPolicyResponse_httpStatus = Lens.lens (\DescribeFirewallPolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeFirewallPolicyResponse' {} a -> s {httpStatus = a} :: DescribeFirewallPolicyResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
describeFirewallPolicyResponse_updateToken :: Lens.Lens' DescribeFirewallPolicyResponse Prelude.Text
describeFirewallPolicyResponse_updateToken = Lens.lens (\DescribeFirewallPolicyResponse' {updateToken} -> updateToken) (\s@DescribeFirewallPolicyResponse' {} a -> s {updateToken = a} :: DescribeFirewallPolicyResponse)

-- | The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
describeFirewallPolicyResponse_firewallPolicyResponse :: Lens.Lens' DescribeFirewallPolicyResponse FirewallPolicyResponse
describeFirewallPolicyResponse_firewallPolicyResponse = Lens.lens (\DescribeFirewallPolicyResponse' {firewallPolicyResponse} -> firewallPolicyResponse) (\s@DescribeFirewallPolicyResponse' {} a -> s {firewallPolicyResponse = a} :: DescribeFirewallPolicyResponse)

instance
  Prelude.NFData
    DescribeFirewallPolicyResponse
  where
  rnf DescribeFirewallPolicyResponse' {..} =
    Prelude.rnf firewallPolicy
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallPolicyResponse

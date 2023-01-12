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
-- Module      : Amazonka.Route53Resolver.GetFirewallRuleGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Identity and Access Management (Amazon Web Services IAM)
-- policy for sharing the specified rule group. You can use the policy to
-- share the rule group using Resource Access Manager (RAM).
module Amazonka.Route53Resolver.GetFirewallRuleGroupPolicy
  ( -- * Creating a Request
    GetFirewallRuleGroupPolicy (..),
    newGetFirewallRuleGroupPolicy,

    -- * Request Lenses
    getFirewallRuleGroupPolicy_arn,

    -- * Destructuring the Response
    GetFirewallRuleGroupPolicyResponse (..),
    newGetFirewallRuleGroupPolicyResponse,

    -- * Response Lenses
    getFirewallRuleGroupPolicyResponse_firewallRuleGroupPolicy,
    getFirewallRuleGroupPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetFirewallRuleGroupPolicy' smart constructor.
data GetFirewallRuleGroupPolicy = GetFirewallRuleGroupPolicy'
  { -- | The ARN (Amazon Resource Name) for the rule group.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallRuleGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getFirewallRuleGroupPolicy_arn' - The ARN (Amazon Resource Name) for the rule group.
newGetFirewallRuleGroupPolicy ::
  -- | 'arn'
  Prelude.Text ->
  GetFirewallRuleGroupPolicy
newGetFirewallRuleGroupPolicy pArn_ =
  GetFirewallRuleGroupPolicy' {arn = pArn_}

-- | The ARN (Amazon Resource Name) for the rule group.
getFirewallRuleGroupPolicy_arn :: Lens.Lens' GetFirewallRuleGroupPolicy Prelude.Text
getFirewallRuleGroupPolicy_arn = Lens.lens (\GetFirewallRuleGroupPolicy' {arn} -> arn) (\s@GetFirewallRuleGroupPolicy' {} a -> s {arn = a} :: GetFirewallRuleGroupPolicy)

instance Core.AWSRequest GetFirewallRuleGroupPolicy where
  type
    AWSResponse GetFirewallRuleGroupPolicy =
      GetFirewallRuleGroupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFirewallRuleGroupPolicyResponse'
            Prelude.<$> (x Data..?> "FirewallRuleGroupPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFirewallRuleGroupPolicy where
  hashWithSalt _salt GetFirewallRuleGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetFirewallRuleGroupPolicy where
  rnf GetFirewallRuleGroupPolicy' {..} = Prelude.rnf arn

instance Data.ToHeaders GetFirewallRuleGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetFirewallRuleGroupPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFirewallRuleGroupPolicy where
  toJSON GetFirewallRuleGroupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath GetFirewallRuleGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFirewallRuleGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFirewallRuleGroupPolicyResponse' smart constructor.
data GetFirewallRuleGroupPolicyResponse = GetFirewallRuleGroupPolicyResponse'
  { -- | The Identity and Access Management (Amazon Web Services IAM) policy for
    -- sharing the specified rule group. You can use the policy to share the
    -- rule group using Resource Access Manager (RAM).
    firewallRuleGroupPolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallRuleGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupPolicy', 'getFirewallRuleGroupPolicyResponse_firewallRuleGroupPolicy' - The Identity and Access Management (Amazon Web Services IAM) policy for
-- sharing the specified rule group. You can use the policy to share the
-- rule group using Resource Access Manager (RAM).
--
-- 'httpStatus', 'getFirewallRuleGroupPolicyResponse_httpStatus' - The response's http status code.
newGetFirewallRuleGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFirewallRuleGroupPolicyResponse
newGetFirewallRuleGroupPolicyResponse pHttpStatus_ =
  GetFirewallRuleGroupPolicyResponse'
    { firewallRuleGroupPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Identity and Access Management (Amazon Web Services IAM) policy for
-- sharing the specified rule group. You can use the policy to share the
-- rule group using Resource Access Manager (RAM).
getFirewallRuleGroupPolicyResponse_firewallRuleGroupPolicy :: Lens.Lens' GetFirewallRuleGroupPolicyResponse (Prelude.Maybe Prelude.Text)
getFirewallRuleGroupPolicyResponse_firewallRuleGroupPolicy = Lens.lens (\GetFirewallRuleGroupPolicyResponse' {firewallRuleGroupPolicy} -> firewallRuleGroupPolicy) (\s@GetFirewallRuleGroupPolicyResponse' {} a -> s {firewallRuleGroupPolicy = a} :: GetFirewallRuleGroupPolicyResponse)

-- | The response's http status code.
getFirewallRuleGroupPolicyResponse_httpStatus :: Lens.Lens' GetFirewallRuleGroupPolicyResponse Prelude.Int
getFirewallRuleGroupPolicyResponse_httpStatus = Lens.lens (\GetFirewallRuleGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@GetFirewallRuleGroupPolicyResponse' {} a -> s {httpStatus = a} :: GetFirewallRuleGroupPolicyResponse)

instance
  Prelude.NFData
    GetFirewallRuleGroupPolicyResponse
  where
  rnf GetFirewallRuleGroupPolicyResponse' {..} =
    Prelude.rnf firewallRuleGroupPolicy
      `Prelude.seq` Prelude.rnf httpStatus

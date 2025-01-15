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
-- Module      : Amazonka.Route53Resolver.PutFirewallRuleGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Identity and Access Management (Amazon Web Services IAM)
-- policy for sharing the rule group. You can use the policy to share the
-- rule group using Resource Access Manager (RAM).
module Amazonka.Route53Resolver.PutFirewallRuleGroupPolicy
  ( -- * Creating a Request
    PutFirewallRuleGroupPolicy (..),
    newPutFirewallRuleGroupPolicy,

    -- * Request Lenses
    putFirewallRuleGroupPolicy_arn,
    putFirewallRuleGroupPolicy_firewallRuleGroupPolicy,

    -- * Destructuring the Response
    PutFirewallRuleGroupPolicyResponse (..),
    newPutFirewallRuleGroupPolicyResponse,

    -- * Response Lenses
    putFirewallRuleGroupPolicyResponse_returnValue,
    putFirewallRuleGroupPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newPutFirewallRuleGroupPolicy' smart constructor.
data PutFirewallRuleGroupPolicy = PutFirewallRuleGroupPolicy'
  { -- | The ARN (Amazon Resource Name) for the rule group that you want to
    -- share.
    arn :: Prelude.Text,
    -- | The Identity and Access Management (Amazon Web Services IAM) policy to
    -- attach to the rule group.
    firewallRuleGroupPolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFirewallRuleGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putFirewallRuleGroupPolicy_arn' - The ARN (Amazon Resource Name) for the rule group that you want to
-- share.
--
-- 'firewallRuleGroupPolicy', 'putFirewallRuleGroupPolicy_firewallRuleGroupPolicy' - The Identity and Access Management (Amazon Web Services IAM) policy to
-- attach to the rule group.
newPutFirewallRuleGroupPolicy ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'firewallRuleGroupPolicy'
  Prelude.Text ->
  PutFirewallRuleGroupPolicy
newPutFirewallRuleGroupPolicy
  pArn_
  pFirewallRuleGroupPolicy_ =
    PutFirewallRuleGroupPolicy'
      { arn = pArn_,
        firewallRuleGroupPolicy =
          pFirewallRuleGroupPolicy_
      }

-- | The ARN (Amazon Resource Name) for the rule group that you want to
-- share.
putFirewallRuleGroupPolicy_arn :: Lens.Lens' PutFirewallRuleGroupPolicy Prelude.Text
putFirewallRuleGroupPolicy_arn = Lens.lens (\PutFirewallRuleGroupPolicy' {arn} -> arn) (\s@PutFirewallRuleGroupPolicy' {} a -> s {arn = a} :: PutFirewallRuleGroupPolicy)

-- | The Identity and Access Management (Amazon Web Services IAM) policy to
-- attach to the rule group.
putFirewallRuleGroupPolicy_firewallRuleGroupPolicy :: Lens.Lens' PutFirewallRuleGroupPolicy Prelude.Text
putFirewallRuleGroupPolicy_firewallRuleGroupPolicy = Lens.lens (\PutFirewallRuleGroupPolicy' {firewallRuleGroupPolicy} -> firewallRuleGroupPolicy) (\s@PutFirewallRuleGroupPolicy' {} a -> s {firewallRuleGroupPolicy = a} :: PutFirewallRuleGroupPolicy)

instance Core.AWSRequest PutFirewallRuleGroupPolicy where
  type
    AWSResponse PutFirewallRuleGroupPolicy =
      PutFirewallRuleGroupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFirewallRuleGroupPolicyResponse'
            Prelude.<$> (x Data..?> "ReturnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutFirewallRuleGroupPolicy where
  hashWithSalt _salt PutFirewallRuleGroupPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` firewallRuleGroupPolicy

instance Prelude.NFData PutFirewallRuleGroupPolicy where
  rnf PutFirewallRuleGroupPolicy' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf firewallRuleGroupPolicy

instance Data.ToHeaders PutFirewallRuleGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.PutFirewallRuleGroupPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutFirewallRuleGroupPolicy where
  toJSON PutFirewallRuleGroupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ( "FirewallRuleGroupPolicy"
                  Data..= firewallRuleGroupPolicy
              )
          ]
      )

instance Data.ToPath PutFirewallRuleGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutFirewallRuleGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFirewallRuleGroupPolicyResponse' smart constructor.
data PutFirewallRuleGroupPolicyResponse = PutFirewallRuleGroupPolicyResponse'
  { returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFirewallRuleGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'putFirewallRuleGroupPolicyResponse_returnValue' -
--
-- 'httpStatus', 'putFirewallRuleGroupPolicyResponse_httpStatus' - The response's http status code.
newPutFirewallRuleGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutFirewallRuleGroupPolicyResponse
newPutFirewallRuleGroupPolicyResponse pHttpStatus_ =
  PutFirewallRuleGroupPolicyResponse'
    { returnValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

putFirewallRuleGroupPolicyResponse_returnValue :: Lens.Lens' PutFirewallRuleGroupPolicyResponse (Prelude.Maybe Prelude.Bool)
putFirewallRuleGroupPolicyResponse_returnValue = Lens.lens (\PutFirewallRuleGroupPolicyResponse' {returnValue} -> returnValue) (\s@PutFirewallRuleGroupPolicyResponse' {} a -> s {returnValue = a} :: PutFirewallRuleGroupPolicyResponse)

-- | The response's http status code.
putFirewallRuleGroupPolicyResponse_httpStatus :: Lens.Lens' PutFirewallRuleGroupPolicyResponse Prelude.Int
putFirewallRuleGroupPolicyResponse_httpStatus = Lens.lens (\PutFirewallRuleGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@PutFirewallRuleGroupPolicyResponse' {} a -> s {httpStatus = a} :: PutFirewallRuleGroupPolicyResponse)

instance
  Prelude.NFData
    PutFirewallRuleGroupPolicyResponse
  where
  rnf PutFirewallRuleGroupPolicyResponse' {..} =
    Prelude.rnf returnValue `Prelude.seq`
      Prelude.rnf httpStatus

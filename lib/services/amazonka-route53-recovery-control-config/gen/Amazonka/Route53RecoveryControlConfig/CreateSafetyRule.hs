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
-- Module      : Amazonka.Route53RecoveryControlConfig.CreateSafetyRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a safety rule in a control panel. Safety rules let you add
-- safeguards around enabling and disabling routing controls, to help
-- prevent unexpected outcomes.
--
-- There are two types of safety rules: assertion rules and gating rules.
--
-- Assertion rule: An assertion rule enforces that, when a routing control
-- state is changed, the criteria set by the rule configuration is met.
-- Otherwise, the change to the routing control is not accepted.
--
-- Gating rule: A gating rule verifies that a set of gating controls
-- evaluates as true, based on a rule configuration that you specify. If
-- the gating rule evaluates to true, Amazon Route 53 Application Recovery
-- Controller allows a set of routing control state changes to run and
-- complete against the set of target controls.
module Amazonka.Route53RecoveryControlConfig.CreateSafetyRule
  ( -- * Creating a Request
    CreateSafetyRule (..),
    newCreateSafetyRule,

    -- * Request Lenses
    createSafetyRule_assertionRule,
    createSafetyRule_clientToken,
    createSafetyRule_gatingRule,

    -- * Destructuring the Response
    CreateSafetyRuleResponse (..),
    newCreateSafetyRuleResponse,

    -- * Response Lenses
    createSafetyRuleResponse_assertionRule,
    createSafetyRuleResponse_gatingRule,
    createSafetyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The request body that you include when you create a safety rule.
--
-- /See:/ 'newCreateSafetyRule' smart constructor.
data CreateSafetyRule = CreateSafetyRule'
  { assertionRule :: Prelude.Maybe NewAssertionRule,
    -- | Unique client idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    gatingRule :: Prelude.Maybe NewGatingRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSafetyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRule', 'createSafetyRule_assertionRule' - Undocumented member.
--
-- 'clientToken', 'createSafetyRule_clientToken' - Unique client idempotency token.
--
-- 'gatingRule', 'createSafetyRule_gatingRule' - Undocumented member.
newCreateSafetyRule ::
  CreateSafetyRule
newCreateSafetyRule =
  CreateSafetyRule'
    { assertionRule = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      gatingRule = Prelude.Nothing
    }

-- | Undocumented member.
createSafetyRule_assertionRule :: Lens.Lens' CreateSafetyRule (Prelude.Maybe NewAssertionRule)
createSafetyRule_assertionRule = Lens.lens (\CreateSafetyRule' {assertionRule} -> assertionRule) (\s@CreateSafetyRule' {} a -> s {assertionRule = a} :: CreateSafetyRule)

-- | Unique client idempotency token.
createSafetyRule_clientToken :: Lens.Lens' CreateSafetyRule (Prelude.Maybe Prelude.Text)
createSafetyRule_clientToken = Lens.lens (\CreateSafetyRule' {clientToken} -> clientToken) (\s@CreateSafetyRule' {} a -> s {clientToken = a} :: CreateSafetyRule)

-- | Undocumented member.
createSafetyRule_gatingRule :: Lens.Lens' CreateSafetyRule (Prelude.Maybe NewGatingRule)
createSafetyRule_gatingRule = Lens.lens (\CreateSafetyRule' {gatingRule} -> gatingRule) (\s@CreateSafetyRule' {} a -> s {gatingRule = a} :: CreateSafetyRule)

instance Core.AWSRequest CreateSafetyRule where
  type
    AWSResponse CreateSafetyRule =
      CreateSafetyRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSafetyRuleResponse'
            Prelude.<$> (x Core..?> "AssertionRule")
            Prelude.<*> (x Core..?> "GatingRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSafetyRule where
  hashWithSalt salt' CreateSafetyRule' {..} =
    salt' `Prelude.hashWithSalt` gatingRule
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assertionRule

instance Prelude.NFData CreateSafetyRule where
  rnf CreateSafetyRule' {..} =
    Prelude.rnf assertionRule
      `Prelude.seq` Prelude.rnf gatingRule
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CreateSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSafetyRule where
  toJSON CreateSafetyRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssertionRule" Core..=) Prelude.<$> assertionRule,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("GatingRule" Core..=) Prelude.<$> gatingRule
          ]
      )

instance Core.ToPath CreateSafetyRule where
  toPath = Prelude.const "/safetyrule"

instance Core.ToQuery CreateSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSafetyRuleResponse' smart constructor.
data CreateSafetyRuleResponse = CreateSafetyRuleResponse'
  { assertionRule :: Prelude.Maybe AssertionRule,
    gatingRule :: Prelude.Maybe GatingRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSafetyRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRule', 'createSafetyRuleResponse_assertionRule' - Undocumented member.
--
-- 'gatingRule', 'createSafetyRuleResponse_gatingRule' - Undocumented member.
--
-- 'httpStatus', 'createSafetyRuleResponse_httpStatus' - The response's http status code.
newCreateSafetyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSafetyRuleResponse
newCreateSafetyRuleResponse pHttpStatus_ =
  CreateSafetyRuleResponse'
    { assertionRule =
        Prelude.Nothing,
      gatingRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSafetyRuleResponse_assertionRule :: Lens.Lens' CreateSafetyRuleResponse (Prelude.Maybe AssertionRule)
createSafetyRuleResponse_assertionRule = Lens.lens (\CreateSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@CreateSafetyRuleResponse' {} a -> s {assertionRule = a} :: CreateSafetyRuleResponse)

-- | Undocumented member.
createSafetyRuleResponse_gatingRule :: Lens.Lens' CreateSafetyRuleResponse (Prelude.Maybe GatingRule)
createSafetyRuleResponse_gatingRule = Lens.lens (\CreateSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@CreateSafetyRuleResponse' {} a -> s {gatingRule = a} :: CreateSafetyRuleResponse)

-- | The response's http status code.
createSafetyRuleResponse_httpStatus :: Lens.Lens' CreateSafetyRuleResponse Prelude.Int
createSafetyRuleResponse_httpStatus = Lens.lens (\CreateSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSafetyRuleResponse' {} a -> s {httpStatus = a} :: CreateSafetyRuleResponse)

instance Prelude.NFData CreateSafetyRuleResponse where
  rnf CreateSafetyRuleResponse' {..} =
    Prelude.rnf assertionRule
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatingRule

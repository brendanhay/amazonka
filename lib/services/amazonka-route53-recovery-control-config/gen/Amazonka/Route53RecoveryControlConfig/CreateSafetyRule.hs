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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a safety rule in a control panel. Safety rules let you add
-- safeguards around changing routing control states, and for enabling and
-- disabling routing controls, to help prevent unexpected outcomes.
--
-- There are two types of safety rules: assertion rules and gating rules.
--
-- Assertion rule: An assertion rule enforces that, when you change a
-- routing control state, that a certain criteria is met. For example, the
-- criteria might be that at least one routing control state is On after
-- the transation so that traffic continues to flow to at least one cell
-- for the application. This ensures that you avoid a fail-open scenario.
--
-- Gating rule: A gating rule lets you configure a gating routing control
-- as an overall \"on\/off\" switch for a group of routing controls. Or,
-- you can configure more complex gating scenarios, for example by
-- configuring multiple gating routing controls.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.safety-rules.html Safety rules>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
module Amazonka.Route53RecoveryControlConfig.CreateSafetyRule
  ( -- * Creating a Request
    CreateSafetyRule (..),
    newCreateSafetyRule,

    -- * Request Lenses
    createSafetyRule_tags,
    createSafetyRule_clientToken,
    createSafetyRule_gatingRule,
    createSafetyRule_assertionRule,

    -- * Destructuring the Response
    CreateSafetyRuleResponse (..),
    newCreateSafetyRuleResponse,

    -- * Response Lenses
    createSafetyRuleResponse_gatingRule,
    createSafetyRuleResponse_assertionRule,
    createSafetyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The request body that you include when you create a safety rule.
--
-- /See:/ 'newCreateSafetyRule' smart constructor.
data CreateSafetyRule = CreateSafetyRule'
  { -- | The tags associated with the safety rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
    -- idempotent API request with an action, specify a client token in the
    -- request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The gating rule requested.
    gatingRule :: Prelude.Maybe NewGatingRule,
    -- | The assertion rule requested.
    assertionRule :: Prelude.Maybe NewAssertionRule
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
-- 'tags', 'createSafetyRule_tags' - The tags associated with the safety rule.
--
-- 'clientToken', 'createSafetyRule_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
--
-- 'gatingRule', 'createSafetyRule_gatingRule' - The gating rule requested.
--
-- 'assertionRule', 'createSafetyRule_assertionRule' - The assertion rule requested.
newCreateSafetyRule ::
  CreateSafetyRule
newCreateSafetyRule =
  CreateSafetyRule'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      gatingRule = Prelude.Nothing,
      assertionRule = Prelude.Nothing
    }

-- | The tags associated with the safety rule.
createSafetyRule_tags :: Lens.Lens' CreateSafetyRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSafetyRule_tags = Lens.lens (\CreateSafetyRule' {tags} -> tags) (\s@CreateSafetyRule' {} a -> s {tags = a} :: CreateSafetyRule) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
createSafetyRule_clientToken :: Lens.Lens' CreateSafetyRule (Prelude.Maybe Prelude.Text)
createSafetyRule_clientToken = Lens.lens (\CreateSafetyRule' {clientToken} -> clientToken) (\s@CreateSafetyRule' {} a -> s {clientToken = a} :: CreateSafetyRule)

-- | The gating rule requested.
createSafetyRule_gatingRule :: Lens.Lens' CreateSafetyRule (Prelude.Maybe NewGatingRule)
createSafetyRule_gatingRule = Lens.lens (\CreateSafetyRule' {gatingRule} -> gatingRule) (\s@CreateSafetyRule' {} a -> s {gatingRule = a} :: CreateSafetyRule)

-- | The assertion rule requested.
createSafetyRule_assertionRule :: Lens.Lens' CreateSafetyRule (Prelude.Maybe NewAssertionRule)
createSafetyRule_assertionRule = Lens.lens (\CreateSafetyRule' {assertionRule} -> assertionRule) (\s@CreateSafetyRule' {} a -> s {assertionRule = a} :: CreateSafetyRule)

instance Core.AWSRequest CreateSafetyRule where
  type
    AWSResponse CreateSafetyRule =
      CreateSafetyRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSafetyRuleResponse'
            Prelude.<$> (x Data..?> "GatingRule")
            Prelude.<*> (x Data..?> "AssertionRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSafetyRule where
  hashWithSalt _salt CreateSafetyRule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` gatingRule
      `Prelude.hashWithSalt` assertionRule

instance Prelude.NFData CreateSafetyRule where
  rnf CreateSafetyRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf gatingRule
      `Prelude.seq` Prelude.rnf assertionRule

instance Data.ToHeaders CreateSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSafetyRule where
  toJSON CreateSafetyRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("GatingRule" Data..=) Prelude.<$> gatingRule,
            ("AssertionRule" Data..=) Prelude.<$> assertionRule
          ]
      )

instance Data.ToPath CreateSafetyRule where
  toPath = Prelude.const "/safetyrule"

instance Data.ToQuery CreateSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSafetyRuleResponse' smart constructor.
data CreateSafetyRuleResponse = CreateSafetyRuleResponse'
  { -- | The gating rule created.
    gatingRule :: Prelude.Maybe GatingRule,
    -- | The assertion rule created.
    assertionRule :: Prelude.Maybe AssertionRule,
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
-- 'gatingRule', 'createSafetyRuleResponse_gatingRule' - The gating rule created.
--
-- 'assertionRule', 'createSafetyRuleResponse_assertionRule' - The assertion rule created.
--
-- 'httpStatus', 'createSafetyRuleResponse_httpStatus' - The response's http status code.
newCreateSafetyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSafetyRuleResponse
newCreateSafetyRuleResponse pHttpStatus_ =
  CreateSafetyRuleResponse'
    { gatingRule =
        Prelude.Nothing,
      assertionRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The gating rule created.
createSafetyRuleResponse_gatingRule :: Lens.Lens' CreateSafetyRuleResponse (Prelude.Maybe GatingRule)
createSafetyRuleResponse_gatingRule = Lens.lens (\CreateSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@CreateSafetyRuleResponse' {} a -> s {gatingRule = a} :: CreateSafetyRuleResponse)

-- | The assertion rule created.
createSafetyRuleResponse_assertionRule :: Lens.Lens' CreateSafetyRuleResponse (Prelude.Maybe AssertionRule)
createSafetyRuleResponse_assertionRule = Lens.lens (\CreateSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@CreateSafetyRuleResponse' {} a -> s {assertionRule = a} :: CreateSafetyRuleResponse)

-- | The response's http status code.
createSafetyRuleResponse_httpStatus :: Lens.Lens' CreateSafetyRuleResponse Prelude.Int
createSafetyRuleResponse_httpStatus = Lens.lens (\CreateSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSafetyRuleResponse' {} a -> s {httpStatus = a} :: CreateSafetyRuleResponse)

instance Prelude.NFData CreateSafetyRuleResponse where
  rnf CreateSafetyRuleResponse' {..} =
    Prelude.rnf gatingRule
      `Prelude.seq` Prelude.rnf assertionRule
      `Prelude.seq` Prelude.rnf httpStatus

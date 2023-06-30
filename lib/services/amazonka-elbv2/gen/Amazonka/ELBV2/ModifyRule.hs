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
-- Module      : Amazonka.ELBV2.ModifyRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified properties of the specified rule. Any properties
-- that you do not specify are unchanged.
--
-- To add an item to a list, remove an item from a list, or update an item
-- in a list, you must provide the entire list. For example, to add an
-- action, specify a list with the current actions plus the new action.
module Amazonka.ELBV2.ModifyRule
  ( -- * Creating a Request
    ModifyRule (..),
    newModifyRule,

    -- * Request Lenses
    modifyRule_actions,
    modifyRule_conditions,
    modifyRule_ruleArn,

    -- * Destructuring the Response
    ModifyRuleResponse (..),
    newModifyRuleResponse,

    -- * Response Lenses
    modifyRuleResponse_rules,
    modifyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyRule' smart constructor.
data ModifyRule = ModifyRule'
  { -- | The actions.
    actions :: Prelude.Maybe [Action],
    -- | The conditions.
    conditions :: Prelude.Maybe [RuleCondition],
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'modifyRule_actions' - The actions.
--
-- 'conditions', 'modifyRule_conditions' - The conditions.
--
-- 'ruleArn', 'modifyRule_ruleArn' - The Amazon Resource Name (ARN) of the rule.
newModifyRule ::
  -- | 'ruleArn'
  Prelude.Text ->
  ModifyRule
newModifyRule pRuleArn_ =
  ModifyRule'
    { actions = Prelude.Nothing,
      conditions = Prelude.Nothing,
      ruleArn = pRuleArn_
    }

-- | The actions.
modifyRule_actions :: Lens.Lens' ModifyRule (Prelude.Maybe [Action])
modifyRule_actions = Lens.lens (\ModifyRule' {actions} -> actions) (\s@ModifyRule' {} a -> s {actions = a} :: ModifyRule) Prelude.. Lens.mapping Lens.coerced

-- | The conditions.
modifyRule_conditions :: Lens.Lens' ModifyRule (Prelude.Maybe [RuleCondition])
modifyRule_conditions = Lens.lens (\ModifyRule' {conditions} -> conditions) (\s@ModifyRule' {} a -> s {conditions = a} :: ModifyRule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the rule.
modifyRule_ruleArn :: Lens.Lens' ModifyRule Prelude.Text
modifyRule_ruleArn = Lens.lens (\ModifyRule' {ruleArn} -> ruleArn) (\s@ModifyRule' {} a -> s {ruleArn = a} :: ModifyRule)

instance Core.AWSRequest ModifyRule where
  type AWSResponse ModifyRule = ModifyRuleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyRuleResult"
      ( \s h x ->
          ModifyRuleResponse'
            Prelude.<$> ( x
                            Data..@? "Rules"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyRule where
  hashWithSalt _salt ModifyRule' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` conditions
      `Prelude.hashWithSalt` ruleArn

instance Prelude.NFData ModifyRule where
  rnf ModifyRule' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf ruleArn

instance Data.ToHeaders ModifyRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyRule where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyRule where
  toQuery ModifyRule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyRule" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "Actions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> actions),
        "Conditions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> conditions),
        "RuleArn" Data.=: ruleArn
      ]

-- | /See:/ 'newModifyRuleResponse' smart constructor.
data ModifyRuleResponse = ModifyRuleResponse'
  { -- | Information about the modified rule.
    rules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'modifyRuleResponse_rules' - Information about the modified rule.
--
-- 'httpStatus', 'modifyRuleResponse_httpStatus' - The response's http status code.
newModifyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyRuleResponse
newModifyRuleResponse pHttpStatus_ =
  ModifyRuleResponse'
    { rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the modified rule.
modifyRuleResponse_rules :: Lens.Lens' ModifyRuleResponse (Prelude.Maybe [Rule])
modifyRuleResponse_rules = Lens.lens (\ModifyRuleResponse' {rules} -> rules) (\s@ModifyRuleResponse' {} a -> s {rules = a} :: ModifyRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyRuleResponse_httpStatus :: Lens.Lens' ModifyRuleResponse Prelude.Int
modifyRuleResponse_httpStatus = Lens.lens (\ModifyRuleResponse' {httpStatus} -> httpStatus) (\s@ModifyRuleResponse' {} a -> s {httpStatus = a} :: ModifyRuleResponse)

instance Prelude.NFData ModifyRuleResponse where
  rnf ModifyRuleResponse' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus

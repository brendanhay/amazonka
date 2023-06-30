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
-- Module      : Amazonka.SES.CreateReceiptRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule.
--
-- For information about setting up receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.CreateReceiptRule
  ( -- * Creating a Request
    CreateReceiptRule (..),
    newCreateReceiptRule,

    -- * Request Lenses
    createReceiptRule_after,
    createReceiptRule_ruleSetName,
    createReceiptRule_rule,

    -- * Destructuring the Response
    CreateReceiptRuleResponse (..),
    newCreateReceiptRuleResponse,

    -- * Response Lenses
    createReceiptRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to create a receipt rule. You use receipt rules to
-- receive email with Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateReceiptRule' smart constructor.
data CreateReceiptRule = CreateReceiptRule'
  { -- | The name of an existing rule after which the new rule will be placed. If
    -- this parameter is null, the new rule will be inserted at the beginning
    -- of the rule list.
    after :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule set that the receipt rule will be added to.
    ruleSetName :: Prelude.Text,
    -- | A data structure that contains the specified rule\'s name, actions,
    -- recipients, domains, enabled status, scan status, and TLS policy.
    rule :: ReceiptRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReceiptRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'after', 'createReceiptRule_after' - The name of an existing rule after which the new rule will be placed. If
-- this parameter is null, the new rule will be inserted at the beginning
-- of the rule list.
--
-- 'ruleSetName', 'createReceiptRule_ruleSetName' - The name of the rule set that the receipt rule will be added to.
--
-- 'rule', 'createReceiptRule_rule' - A data structure that contains the specified rule\'s name, actions,
-- recipients, domains, enabled status, scan status, and TLS policy.
newCreateReceiptRule ::
  -- | 'ruleSetName'
  Prelude.Text ->
  -- | 'rule'
  ReceiptRule ->
  CreateReceiptRule
newCreateReceiptRule pRuleSetName_ pRule_ =
  CreateReceiptRule'
    { after = Prelude.Nothing,
      ruleSetName = pRuleSetName_,
      rule = pRule_
    }

-- | The name of an existing rule after which the new rule will be placed. If
-- this parameter is null, the new rule will be inserted at the beginning
-- of the rule list.
createReceiptRule_after :: Lens.Lens' CreateReceiptRule (Prelude.Maybe Prelude.Text)
createReceiptRule_after = Lens.lens (\CreateReceiptRule' {after} -> after) (\s@CreateReceiptRule' {} a -> s {after = a} :: CreateReceiptRule)

-- | The name of the rule set that the receipt rule will be added to.
createReceiptRule_ruleSetName :: Lens.Lens' CreateReceiptRule Prelude.Text
createReceiptRule_ruleSetName = Lens.lens (\CreateReceiptRule' {ruleSetName} -> ruleSetName) (\s@CreateReceiptRule' {} a -> s {ruleSetName = a} :: CreateReceiptRule)

-- | A data structure that contains the specified rule\'s name, actions,
-- recipients, domains, enabled status, scan status, and TLS policy.
createReceiptRule_rule :: Lens.Lens' CreateReceiptRule ReceiptRule
createReceiptRule_rule = Lens.lens (\CreateReceiptRule' {rule} -> rule) (\s@CreateReceiptRule' {} a -> s {rule = a} :: CreateReceiptRule)

instance Core.AWSRequest CreateReceiptRule where
  type
    AWSResponse CreateReceiptRule =
      CreateReceiptRuleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateReceiptRuleResult"
      ( \s h x ->
          CreateReceiptRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReceiptRule where
  hashWithSalt _salt CreateReceiptRule' {..} =
    _salt
      `Prelude.hashWithSalt` after
      `Prelude.hashWithSalt` ruleSetName
      `Prelude.hashWithSalt` rule

instance Prelude.NFData CreateReceiptRule where
  rnf CreateReceiptRule' {..} =
    Prelude.rnf after
      `Prelude.seq` Prelude.rnf ruleSetName
      `Prelude.seq` Prelude.rnf rule

instance Data.ToHeaders CreateReceiptRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateReceiptRule where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReceiptRule where
  toQuery CreateReceiptRule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateReceiptRule" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "After" Data.=: after,
        "RuleSetName" Data.=: ruleSetName,
        "Rule" Data.=: rule
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateReceiptRuleResponse' smart constructor.
data CreateReceiptRuleResponse = CreateReceiptRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReceiptRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createReceiptRuleResponse_httpStatus' - The response's http status code.
newCreateReceiptRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReceiptRuleResponse
newCreateReceiptRuleResponse pHttpStatus_ =
  CreateReceiptRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createReceiptRuleResponse_httpStatus :: Lens.Lens' CreateReceiptRuleResponse Prelude.Int
createReceiptRuleResponse_httpStatus = Lens.lens (\CreateReceiptRuleResponse' {httpStatus} -> httpStatus) (\s@CreateReceiptRuleResponse' {} a -> s {httpStatus = a} :: CreateReceiptRuleResponse)

instance Prelude.NFData CreateReceiptRuleResponse where
  rnf CreateReceiptRuleResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.SES.DeleteReceiptRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule.
--
-- For information about managing receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteReceiptRule
  ( -- * Creating a Request
    DeleteReceiptRule (..),
    newDeleteReceiptRule,

    -- * Request Lenses
    deleteReceiptRule_ruleSetName,
    deleteReceiptRule_ruleName,

    -- * Destructuring the Response
    DeleteReceiptRuleResponse (..),
    newDeleteReceiptRuleResponse,

    -- * Response Lenses
    deleteReceiptRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete a receipt rule. You use receipt rules to
-- receive email with Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteReceiptRule' smart constructor.
data DeleteReceiptRule = DeleteReceiptRule'
  { -- | The name of the receipt rule set that contains the receipt rule to
    -- delete.
    ruleSetName :: Prelude.Text,
    -- | The name of the receipt rule to delete.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'deleteReceiptRule_ruleSetName' - The name of the receipt rule set that contains the receipt rule to
-- delete.
--
-- 'ruleName', 'deleteReceiptRule_ruleName' - The name of the receipt rule to delete.
newDeleteReceiptRule ::
  -- | 'ruleSetName'
  Prelude.Text ->
  -- | 'ruleName'
  Prelude.Text ->
  DeleteReceiptRule
newDeleteReceiptRule pRuleSetName_ pRuleName_ =
  DeleteReceiptRule'
    { ruleSetName = pRuleSetName_,
      ruleName = pRuleName_
    }

-- | The name of the receipt rule set that contains the receipt rule to
-- delete.
deleteReceiptRule_ruleSetName :: Lens.Lens' DeleteReceiptRule Prelude.Text
deleteReceiptRule_ruleSetName = Lens.lens (\DeleteReceiptRule' {ruleSetName} -> ruleSetName) (\s@DeleteReceiptRule' {} a -> s {ruleSetName = a} :: DeleteReceiptRule)

-- | The name of the receipt rule to delete.
deleteReceiptRule_ruleName :: Lens.Lens' DeleteReceiptRule Prelude.Text
deleteReceiptRule_ruleName = Lens.lens (\DeleteReceiptRule' {ruleName} -> ruleName) (\s@DeleteReceiptRule' {} a -> s {ruleName = a} :: DeleteReceiptRule)

instance Core.AWSRequest DeleteReceiptRule where
  type
    AWSResponse DeleteReceiptRule =
      DeleteReceiptRuleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteReceiptRuleResult"
      ( \s h x ->
          DeleteReceiptRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReceiptRule where
  hashWithSalt _salt DeleteReceiptRule' {..} =
    _salt
      `Prelude.hashWithSalt` ruleSetName
      `Prelude.hashWithSalt` ruleName

instance Prelude.NFData DeleteReceiptRule where
  rnf DeleteReceiptRule' {..} =
    Prelude.rnf ruleSetName
      `Prelude.seq` Prelude.rnf ruleName

instance Data.ToHeaders DeleteReceiptRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteReceiptRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReceiptRule where
  toQuery DeleteReceiptRule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteReceiptRule" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Data.=: ruleSetName,
        "RuleName" Data.=: ruleName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteReceiptRuleResponse' smart constructor.
data DeleteReceiptRuleResponse = DeleteReceiptRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReceiptRuleResponse_httpStatus' - The response's http status code.
newDeleteReceiptRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReceiptRuleResponse
newDeleteReceiptRuleResponse pHttpStatus_ =
  DeleteReceiptRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReceiptRuleResponse_httpStatus :: Lens.Lens' DeleteReceiptRuleResponse Prelude.Int
deleteReceiptRuleResponse_httpStatus = Lens.lens (\DeleteReceiptRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteReceiptRuleResponse' {} a -> s {httpStatus = a} :: DeleteReceiptRuleResponse)

instance Prelude.NFData DeleteReceiptRuleResponse where
  rnf DeleteReceiptRuleResponse' {..} =
    Prelude.rnf httpStatus

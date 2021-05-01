{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.UpdateReceiptRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a receipt rule.
--
-- For information about managing receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateReceiptRule
  ( -- * Creating a Request
    UpdateReceiptRule (..),
    newUpdateReceiptRule,

    -- * Request Lenses
    updateReceiptRule_ruleSetName,
    updateReceiptRule_rule,

    -- * Destructuring the Response
    UpdateReceiptRuleResponse (..),
    newUpdateReceiptRuleResponse,

    -- * Response Lenses
    updateReceiptRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to update a receipt rule. You use receipt rules to
-- receive email with Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newUpdateReceiptRule' smart constructor.
data UpdateReceiptRule = UpdateReceiptRule'
  { -- | The name of the receipt rule set that the receipt rule belongs to.
    ruleSetName :: Prelude.Text,
    -- | A data structure that contains the updated receipt rule information.
    rule :: ReceiptRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateReceiptRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'updateReceiptRule_ruleSetName' - The name of the receipt rule set that the receipt rule belongs to.
--
-- 'rule', 'updateReceiptRule_rule' - A data structure that contains the updated receipt rule information.
newUpdateReceiptRule ::
  -- | 'ruleSetName'
  Prelude.Text ->
  -- | 'rule'
  ReceiptRule ->
  UpdateReceiptRule
newUpdateReceiptRule pRuleSetName_ pRule_ =
  UpdateReceiptRule'
    { ruleSetName = pRuleSetName_,
      rule = pRule_
    }

-- | The name of the receipt rule set that the receipt rule belongs to.
updateReceiptRule_ruleSetName :: Lens.Lens' UpdateReceiptRule Prelude.Text
updateReceiptRule_ruleSetName = Lens.lens (\UpdateReceiptRule' {ruleSetName} -> ruleSetName) (\s@UpdateReceiptRule' {} a -> s {ruleSetName = a} :: UpdateReceiptRule)

-- | A data structure that contains the updated receipt rule information.
updateReceiptRule_rule :: Lens.Lens' UpdateReceiptRule ReceiptRule
updateReceiptRule_rule = Lens.lens (\UpdateReceiptRule' {rule} -> rule) (\s@UpdateReceiptRule' {} a -> s {rule = a} :: UpdateReceiptRule)

instance Prelude.AWSRequest UpdateReceiptRule where
  type Rs UpdateReceiptRule = UpdateReceiptRuleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateReceiptRuleResult"
      ( \s h x ->
          UpdateReceiptRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReceiptRule

instance Prelude.NFData UpdateReceiptRule

instance Prelude.ToHeaders UpdateReceiptRule where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateReceiptRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateReceiptRule where
  toQuery UpdateReceiptRule' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateReceiptRule" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Prelude.=: ruleSetName,
        "Rule" Prelude.=: rule
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newUpdateReceiptRuleResponse' smart constructor.
data UpdateReceiptRuleResponse = UpdateReceiptRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateReceiptRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateReceiptRuleResponse_httpStatus' - The response's http status code.
newUpdateReceiptRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReceiptRuleResponse
newUpdateReceiptRuleResponse pHttpStatus_ =
  UpdateReceiptRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateReceiptRuleResponse_httpStatus :: Lens.Lens' UpdateReceiptRuleResponse Prelude.Int
updateReceiptRuleResponse_httpStatus = Lens.lens (\UpdateReceiptRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateReceiptRuleResponse' {} a -> s {httpStatus = a} :: UpdateReceiptRuleResponse)

instance Prelude.NFData UpdateReceiptRuleResponse

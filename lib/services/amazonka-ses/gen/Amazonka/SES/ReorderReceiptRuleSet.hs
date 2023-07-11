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
-- Module      : Amazonka.SES.ReorderReceiptRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reorders the receipt rules within a receipt rule set.
--
-- All of the rules in the rule set must be represented in this request.
-- That is, this API will return an error if the reorder request doesn\'t
-- explicitly position all of the rules.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.ReorderReceiptRuleSet
  ( -- * Creating a Request
    ReorderReceiptRuleSet (..),
    newReorderReceiptRuleSet,

    -- * Request Lenses
    reorderReceiptRuleSet_ruleSetName,
    reorderReceiptRuleSet_ruleNames,

    -- * Destructuring the Response
    ReorderReceiptRuleSetResponse (..),
    newReorderReceiptRuleSetResponse,

    -- * Response Lenses
    reorderReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to reorder the receipt rules within a receipt rule
-- set. You use receipt rule sets to receive email with Amazon SES. For
-- more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReorderReceiptRuleSet' smart constructor.
data ReorderReceiptRuleSet = ReorderReceiptRuleSet'
  { -- | The name of the receipt rule set to reorder.
    ruleSetName :: Prelude.Text,
    -- | A list of the specified receipt rule set\'s receipt rules in the order
    -- that you want to put them.
    ruleNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReorderReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'reorderReceiptRuleSet_ruleSetName' - The name of the receipt rule set to reorder.
--
-- 'ruleNames', 'reorderReceiptRuleSet_ruleNames' - A list of the specified receipt rule set\'s receipt rules in the order
-- that you want to put them.
newReorderReceiptRuleSet ::
  -- | 'ruleSetName'
  Prelude.Text ->
  ReorderReceiptRuleSet
newReorderReceiptRuleSet pRuleSetName_ =
  ReorderReceiptRuleSet'
    { ruleSetName = pRuleSetName_,
      ruleNames = Prelude.mempty
    }

-- | The name of the receipt rule set to reorder.
reorderReceiptRuleSet_ruleSetName :: Lens.Lens' ReorderReceiptRuleSet Prelude.Text
reorderReceiptRuleSet_ruleSetName = Lens.lens (\ReorderReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@ReorderReceiptRuleSet' {} a -> s {ruleSetName = a} :: ReorderReceiptRuleSet)

-- | A list of the specified receipt rule set\'s receipt rules in the order
-- that you want to put them.
reorderReceiptRuleSet_ruleNames :: Lens.Lens' ReorderReceiptRuleSet [Prelude.Text]
reorderReceiptRuleSet_ruleNames = Lens.lens (\ReorderReceiptRuleSet' {ruleNames} -> ruleNames) (\s@ReorderReceiptRuleSet' {} a -> s {ruleNames = a} :: ReorderReceiptRuleSet) Prelude.. Lens.coerced

instance Core.AWSRequest ReorderReceiptRuleSet where
  type
    AWSResponse ReorderReceiptRuleSet =
      ReorderReceiptRuleSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ReorderReceiptRuleSetResult"
      ( \s h x ->
          ReorderReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReorderReceiptRuleSet where
  hashWithSalt _salt ReorderReceiptRuleSet' {..} =
    _salt
      `Prelude.hashWithSalt` ruleSetName
      `Prelude.hashWithSalt` ruleNames

instance Prelude.NFData ReorderReceiptRuleSet where
  rnf ReorderReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName
      `Prelude.seq` Prelude.rnf ruleNames

instance Data.ToHeaders ReorderReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReorderReceiptRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery ReorderReceiptRuleSet where
  toQuery ReorderReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReorderReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Data.=: ruleSetName,
        "RuleNames"
          Data.=: Data.toQueryList "member" ruleNames
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newReorderReceiptRuleSetResponse' smart constructor.
data ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReorderReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'reorderReceiptRuleSetResponse_httpStatus' - The response's http status code.
newReorderReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReorderReceiptRuleSetResponse
newReorderReceiptRuleSetResponse pHttpStatus_ =
  ReorderReceiptRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
reorderReceiptRuleSetResponse_httpStatus :: Lens.Lens' ReorderReceiptRuleSetResponse Prelude.Int
reorderReceiptRuleSetResponse_httpStatus = Lens.lens (\ReorderReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@ReorderReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: ReorderReceiptRuleSetResponse)

instance Prelude.NFData ReorderReceiptRuleSetResponse where
  rnf ReorderReceiptRuleSetResponse' {..} =
    Prelude.rnf httpStatus

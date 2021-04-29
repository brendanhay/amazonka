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
-- Module      : Network.AWS.SES.ReorderReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.SES.ReorderReceiptRuleSet
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
reorderReceiptRuleSet_ruleNames = Lens.lens (\ReorderReceiptRuleSet' {ruleNames} -> ruleNames) (\s@ReorderReceiptRuleSet' {} a -> s {ruleNames = a} :: ReorderReceiptRuleSet) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest ReorderReceiptRuleSet where
  type
    Rs ReorderReceiptRuleSet =
      ReorderReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ReorderReceiptRuleSetResult"
      ( \s h x ->
          ReorderReceiptRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReorderReceiptRuleSet

instance Prelude.NFData ReorderReceiptRuleSet

instance Prelude.ToHeaders ReorderReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ReorderReceiptRuleSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReorderReceiptRuleSet where
  toQuery ReorderReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ReorderReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Prelude.=: ruleSetName,
        "RuleNames"
          Prelude.=: Prelude.toQueryList "member" ruleNames
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newReorderReceiptRuleSetResponse' smart constructor.
data ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ReorderReceiptRuleSetResponse

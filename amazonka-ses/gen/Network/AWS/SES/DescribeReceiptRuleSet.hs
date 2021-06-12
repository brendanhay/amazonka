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
-- Module      : Network.AWS.SES.DescribeReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule set.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeReceiptRuleSet
  ( -- * Creating a Request
    DescribeReceiptRuleSet (..),
    newDescribeReceiptRuleSet,

    -- * Request Lenses
    describeReceiptRuleSet_ruleSetName,

    -- * Destructuring the Response
    DescribeReceiptRuleSetResponse (..),
    newDescribeReceiptRuleSetResponse,

    -- * Response Lenses
    describeReceiptRuleSetResponse_rules,
    describeReceiptRuleSetResponse_metadata,
    describeReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the details of a receipt rule set. You
-- use receipt rule sets to receive email with Amazon SES. For more
-- information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeReceiptRuleSet' smart constructor.
data DescribeReceiptRuleSet = DescribeReceiptRuleSet'
  { -- | The name of the receipt rule set to describe.
    ruleSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetName', 'describeReceiptRuleSet_ruleSetName' - The name of the receipt rule set to describe.
newDescribeReceiptRuleSet ::
  -- | 'ruleSetName'
  Core.Text ->
  DescribeReceiptRuleSet
newDescribeReceiptRuleSet pRuleSetName_ =
  DescribeReceiptRuleSet'
    { ruleSetName =
        pRuleSetName_
    }

-- | The name of the receipt rule set to describe.
describeReceiptRuleSet_ruleSetName :: Lens.Lens' DescribeReceiptRuleSet Core.Text
describeReceiptRuleSet_ruleSetName = Lens.lens (\DescribeReceiptRuleSet' {ruleSetName} -> ruleSetName) (\s@DescribeReceiptRuleSet' {} a -> s {ruleSetName = a} :: DescribeReceiptRuleSet)

instance Core.AWSRequest DescribeReceiptRuleSet where
  type
    AWSResponse DescribeReceiptRuleSet =
      DescribeReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReceiptRuleSetResult"
      ( \s h x ->
          DescribeReceiptRuleSetResponse'
            Core.<$> ( x Core..@? "Rules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReceiptRuleSet

instance Core.NFData DescribeReceiptRuleSet

instance Core.ToHeaders DescribeReceiptRuleSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeReceiptRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReceiptRuleSet where
  toQuery DescribeReceiptRuleSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeReceiptRuleSet" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | Represents the details of the specified receipt rule set.
--
-- /See:/ 'newDescribeReceiptRuleSetResponse' smart constructor.
data DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse'
  { -- | A list of the receipt rules that belong to the specified receipt rule
    -- set.
    rules :: Core.Maybe [ReceiptRule],
    -- | The metadata for the receipt rule set, which consists of the rule set
    -- name and the timestamp of when the rule set was created.
    metadata :: Core.Maybe ReceiptRuleSetMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'describeReceiptRuleSetResponse_rules' - A list of the receipt rules that belong to the specified receipt rule
-- set.
--
-- 'metadata', 'describeReceiptRuleSetResponse_metadata' - The metadata for the receipt rule set, which consists of the rule set
-- name and the timestamp of when the rule set was created.
--
-- 'httpStatus', 'describeReceiptRuleSetResponse_httpStatus' - The response's http status code.
newDescribeReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReceiptRuleSetResponse
newDescribeReceiptRuleSetResponse pHttpStatus_ =
  DescribeReceiptRuleSetResponse'
    { rules =
        Core.Nothing,
      metadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the receipt rules that belong to the specified receipt rule
-- set.
describeReceiptRuleSetResponse_rules :: Lens.Lens' DescribeReceiptRuleSetResponse (Core.Maybe [ReceiptRule])
describeReceiptRuleSetResponse_rules = Lens.lens (\DescribeReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeReceiptRuleSetResponse) Core.. Lens.mapping Lens._Coerce

-- | The metadata for the receipt rule set, which consists of the rule set
-- name and the timestamp of when the rule set was created.
describeReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeReceiptRuleSetResponse (Core.Maybe ReceiptRuleSetMetadata)
describeReceiptRuleSetResponse_metadata = Lens.lens (\DescribeReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeReceiptRuleSetResponse)

-- | The response's http status code.
describeReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeReceiptRuleSetResponse Core.Int
describeReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeReceiptRuleSetResponse)

instance Core.NFData DescribeReceiptRuleSetResponse

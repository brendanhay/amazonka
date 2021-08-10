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
import qualified Network.AWS.Prelude as Prelude
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
    ruleSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeReceiptRuleSet
newDescribeReceiptRuleSet pRuleSetName_ =
  DescribeReceiptRuleSet'
    { ruleSetName =
        pRuleSetName_
    }

-- | The name of the receipt rule set to describe.
describeReceiptRuleSet_ruleSetName :: Lens.Lens' DescribeReceiptRuleSet Prelude.Text
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
            Prelude.<$> ( x Core..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReceiptRuleSet

instance Prelude.NFData DescribeReceiptRuleSet

instance Core.ToHeaders DescribeReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeReceiptRuleSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReceiptRuleSet where
  toQuery DescribeReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Core.=: ruleSetName
      ]

-- | Represents the details of the specified receipt rule set.
--
-- /See:/ 'newDescribeReceiptRuleSetResponse' smart constructor.
data DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse'
  { -- | A list of the receipt rules that belong to the specified receipt rule
    -- set.
    rules :: Prelude.Maybe [ReceiptRule],
    -- | The metadata for the receipt rule set, which consists of the rule set
    -- name and the timestamp of when the rule set was created.
    metadata :: Prelude.Maybe ReceiptRuleSetMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeReceiptRuleSetResponse
newDescribeReceiptRuleSetResponse pHttpStatus_ =
  DescribeReceiptRuleSetResponse'
    { rules =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the receipt rules that belong to the specified receipt rule
-- set.
describeReceiptRuleSetResponse_rules :: Lens.Lens' DescribeReceiptRuleSetResponse (Prelude.Maybe [ReceiptRule])
describeReceiptRuleSetResponse_rules = Lens.lens (\DescribeReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeReceiptRuleSetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The metadata for the receipt rule set, which consists of the rule set
-- name and the timestamp of when the rule set was created.
describeReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeReceiptRuleSetResponse (Prelude.Maybe ReceiptRuleSetMetadata)
describeReceiptRuleSetResponse_metadata = Lens.lens (\DescribeReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeReceiptRuleSetResponse)

-- | The response's http status code.
describeReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeReceiptRuleSetResponse Prelude.Int
describeReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeReceiptRuleSetResponse)

instance
  Prelude.NFData
    DescribeReceiptRuleSetResponse

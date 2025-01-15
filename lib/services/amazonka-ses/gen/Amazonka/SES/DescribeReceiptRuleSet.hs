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
-- Module      : Amazonka.SES.DescribeReceiptRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule set.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DescribeReceiptRuleSet
  ( -- * Creating a Request
    DescribeReceiptRuleSet (..),
    newDescribeReceiptRuleSet,

    -- * Request Lenses
    describeReceiptRuleSet_ruleSetName,

    -- * Destructuring the Response
    DescribeReceiptRuleSetResponse (..),
    newDescribeReceiptRuleSetResponse,

    -- * Response Lenses
    describeReceiptRuleSetResponse_metadata,
    describeReceiptRuleSetResponse_rules,
    describeReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeReceiptRuleSetResult"
      ( \s h x ->
          DescribeReceiptRuleSetResponse'
            Prelude.<$> (x Data..@? "Metadata")
            Prelude.<*> ( x Data..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReceiptRuleSet where
  hashWithSalt _salt DescribeReceiptRuleSet' {..} =
    _salt `Prelude.hashWithSalt` ruleSetName

instance Prelude.NFData DescribeReceiptRuleSet where
  rnf DescribeReceiptRuleSet' {..} =
    Prelude.rnf ruleSetName

instance Data.ToHeaders DescribeReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeReceiptRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReceiptRuleSet where
  toQuery DescribeReceiptRuleSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeReceiptRuleSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "RuleSetName" Data.=: ruleSetName
      ]

-- | Represents the details of the specified receipt rule set.
--
-- /See:/ 'newDescribeReceiptRuleSetResponse' smart constructor.
data DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse'
  { -- | The metadata for the receipt rule set, which consists of the rule set
    -- name and the timestamp of when the rule set was created.
    metadata :: Prelude.Maybe ReceiptRuleSetMetadata,
    -- | A list of the receipt rules that belong to the specified receipt rule
    -- set.
    rules :: Prelude.Maybe [ReceiptRule],
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
-- 'metadata', 'describeReceiptRuleSetResponse_metadata' - The metadata for the receipt rule set, which consists of the rule set
-- name and the timestamp of when the rule set was created.
--
-- 'rules', 'describeReceiptRuleSetResponse_rules' - A list of the receipt rules that belong to the specified receipt rule
-- set.
--
-- 'httpStatus', 'describeReceiptRuleSetResponse_httpStatus' - The response's http status code.
newDescribeReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReceiptRuleSetResponse
newDescribeReceiptRuleSetResponse pHttpStatus_ =
  DescribeReceiptRuleSetResponse'
    { metadata =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for the receipt rule set, which consists of the rule set
-- name and the timestamp of when the rule set was created.
describeReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeReceiptRuleSetResponse (Prelude.Maybe ReceiptRuleSetMetadata)
describeReceiptRuleSetResponse_metadata = Lens.lens (\DescribeReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeReceiptRuleSetResponse)

-- | A list of the receipt rules that belong to the specified receipt rule
-- set.
describeReceiptRuleSetResponse_rules :: Lens.Lens' DescribeReceiptRuleSetResponse (Prelude.Maybe [ReceiptRule])
describeReceiptRuleSetResponse_rules = Lens.lens (\DescribeReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeReceiptRuleSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeReceiptRuleSetResponse Prelude.Int
describeReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeReceiptRuleSetResponse)

instance
  Prelude.NFData
    DescribeReceiptRuleSetResponse
  where
  rnf DescribeReceiptRuleSetResponse' {..} =
    Prelude.rnf metadata `Prelude.seq`
      Prelude.rnf rules `Prelude.seq`
        Prelude.rnf httpStatus

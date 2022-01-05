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
-- Module      : Amazonka.SES.DescribeActiveReceiptRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata and receipt rules for the receipt rule set that is
-- currently active.
--
-- For information about setting up receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DescribeActiveReceiptRuleSet
  ( -- * Creating a Request
    DescribeActiveReceiptRuleSet (..),
    newDescribeActiveReceiptRuleSet,

    -- * Destructuring the Response
    DescribeActiveReceiptRuleSetResponse (..),
    newDescribeActiveReceiptRuleSetResponse,

    -- * Response Lenses
    describeActiveReceiptRuleSetResponse_rules,
    describeActiveReceiptRuleSetResponse_metadata,
    describeActiveReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to return the metadata and receipt rules for the
-- receipt rule set that is currently active. You use receipt rule sets to
-- receive email with Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeActiveReceiptRuleSet' smart constructor.
data DescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActiveReceiptRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeActiveReceiptRuleSet ::
  DescribeActiveReceiptRuleSet
newDescribeActiveReceiptRuleSet =
  DescribeActiveReceiptRuleSet'

instance Core.AWSRequest DescribeActiveReceiptRuleSet where
  type
    AWSResponse DescribeActiveReceiptRuleSet =
      DescribeActiveReceiptRuleSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeActiveReceiptRuleSetResult"
      ( \s h x ->
          DescribeActiveReceiptRuleSetResponse'
            Prelude.<$> ( x Core..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeActiveReceiptRuleSet
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeActiveReceiptRuleSet where
  rnf _ = ()

instance Core.ToHeaders DescribeActiveReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeActiveReceiptRuleSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeActiveReceiptRuleSet where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Core.=: ( "DescribeActiveReceiptRuleSet" ::
                          Prelude.ByteString
                      ),
            "Version"
              Core.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Represents the metadata and receipt rules for the receipt rule set that
-- is currently active.
--
-- /See:/ 'newDescribeActiveReceiptRuleSetResponse' smart constructor.
data DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse'
  { -- | The receipt rules that belong to the active rule set.
    rules :: Prelude.Maybe [ReceiptRule],
    -- | The metadata for the currently active receipt rule set. The metadata
    -- consists of the rule set name and a timestamp of when the rule set was
    -- created.
    metadata :: Prelude.Maybe ReceiptRuleSetMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActiveReceiptRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'describeActiveReceiptRuleSetResponse_rules' - The receipt rules that belong to the active rule set.
--
-- 'metadata', 'describeActiveReceiptRuleSetResponse_metadata' - The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and a timestamp of when the rule set was
-- created.
--
-- 'httpStatus', 'describeActiveReceiptRuleSetResponse_httpStatus' - The response's http status code.
newDescribeActiveReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActiveReceiptRuleSetResponse
newDescribeActiveReceiptRuleSetResponse pHttpStatus_ =
  DescribeActiveReceiptRuleSetResponse'
    { rules =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The receipt rules that belong to the active rule set.
describeActiveReceiptRuleSetResponse_rules :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Prelude.Maybe [ReceiptRule])
describeActiveReceiptRuleSetResponse_rules = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeActiveReceiptRuleSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and a timestamp of when the rule set was
-- created.
describeActiveReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Prelude.Maybe ReceiptRuleSetMetadata)
describeActiveReceiptRuleSetResponse_metadata = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeActiveReceiptRuleSetResponse)

-- | The response's http status code.
describeActiveReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeActiveReceiptRuleSetResponse Prelude.Int
describeActiveReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeActiveReceiptRuleSetResponse)

instance
  Prelude.NFData
    DescribeActiveReceiptRuleSetResponse
  where
  rnf DescribeActiveReceiptRuleSetResponse' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus

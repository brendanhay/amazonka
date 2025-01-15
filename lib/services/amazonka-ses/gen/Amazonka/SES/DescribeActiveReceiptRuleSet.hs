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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeActiveReceiptRuleSetResponse_metadata,
    describeActiveReceiptRuleSetResponse_rules,
    describeActiveReceiptRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeActiveReceiptRuleSetResult"
      ( \s h x ->
          DescribeActiveReceiptRuleSetResponse'
            Prelude.<$> (x Data..@? "Metadata")
            Prelude.<*> ( x Data..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
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

instance Data.ToHeaders DescribeActiveReceiptRuleSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeActiveReceiptRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeActiveReceiptRuleSet where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DescribeActiveReceiptRuleSet" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Represents the metadata and receipt rules for the receipt rule set that
-- is currently active.
--
-- /See:/ 'newDescribeActiveReceiptRuleSetResponse' smart constructor.
data DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse'
  { -- | The metadata for the currently active receipt rule set. The metadata
    -- consists of the rule set name and a timestamp of when the rule set was
    -- created.
    metadata :: Prelude.Maybe ReceiptRuleSetMetadata,
    -- | The receipt rules that belong to the active rule set.
    rules :: Prelude.Maybe [ReceiptRule],
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
-- 'metadata', 'describeActiveReceiptRuleSetResponse_metadata' - The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and a timestamp of when the rule set was
-- created.
--
-- 'rules', 'describeActiveReceiptRuleSetResponse_rules' - The receipt rules that belong to the active rule set.
--
-- 'httpStatus', 'describeActiveReceiptRuleSetResponse_httpStatus' - The response's http status code.
newDescribeActiveReceiptRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActiveReceiptRuleSetResponse
newDescribeActiveReceiptRuleSetResponse pHttpStatus_ =
  DescribeActiveReceiptRuleSetResponse'
    { metadata =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and a timestamp of when the rule set was
-- created.
describeActiveReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Prelude.Maybe ReceiptRuleSetMetadata)
describeActiveReceiptRuleSetResponse_metadata = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeActiveReceiptRuleSetResponse)

-- | The receipt rules that belong to the active rule set.
describeActiveReceiptRuleSetResponse_rules :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Prelude.Maybe [ReceiptRule])
describeActiveReceiptRuleSetResponse_rules = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeActiveReceiptRuleSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeActiveReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeActiveReceiptRuleSetResponse Prelude.Int
describeActiveReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeActiveReceiptRuleSetResponse)

instance
  Prelude.NFData
    DescribeActiveReceiptRuleSetResponse
  where
  rnf DescribeActiveReceiptRuleSetResponse' {..} =
    Prelude.rnf metadata `Prelude.seq`
      Prelude.rnf rules `Prelude.seq`
        Prelude.rnf httpStatus

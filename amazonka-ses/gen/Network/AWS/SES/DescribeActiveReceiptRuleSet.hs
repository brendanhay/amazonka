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
-- Module      : Network.AWS.SES.DescribeActiveReceiptRuleSet
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
module Network.AWS.SES.DescribeActiveReceiptRuleSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the metadata and receipt rules for the
-- receipt rule set that is currently active. You use receipt rule sets to
-- receive email with Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeActiveReceiptRuleSet' smart constructor.
data DescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
            Core.<$> ( x Core..@? "Rules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeActiveReceiptRuleSet

instance Core.NFData DescribeActiveReceiptRuleSet

instance Core.ToHeaders DescribeActiveReceiptRuleSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeActiveReceiptRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery DescribeActiveReceiptRuleSet where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeActiveReceiptRuleSet" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | Represents the metadata and receipt rules for the receipt rule set that
-- is currently active.
--
-- /See:/ 'newDescribeActiveReceiptRuleSetResponse' smart constructor.
data DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse'
  { -- | The receipt rules that belong to the active rule set.
    rules :: Core.Maybe [ReceiptRule],
    -- | The metadata for the currently active receipt rule set. The metadata
    -- consists of the rule set name and a timestamp of when the rule set was
    -- created.
    metadata :: Core.Maybe ReceiptRuleSetMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeActiveReceiptRuleSetResponse
newDescribeActiveReceiptRuleSetResponse pHttpStatus_ =
  DescribeActiveReceiptRuleSetResponse'
    { rules =
        Core.Nothing,
      metadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The receipt rules that belong to the active rule set.
describeActiveReceiptRuleSetResponse_rules :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Core.Maybe [ReceiptRule])
describeActiveReceiptRuleSetResponse_rules = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {rules} -> rules) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {rules = a} :: DescribeActiveReceiptRuleSetResponse) Core.. Lens.mapping Lens._Coerce

-- | The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and a timestamp of when the rule set was
-- created.
describeActiveReceiptRuleSetResponse_metadata :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Core.Maybe ReceiptRuleSetMetadata)
describeActiveReceiptRuleSetResponse_metadata = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {metadata} -> metadata) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {metadata = a} :: DescribeActiveReceiptRuleSetResponse)

-- | The response's http status code.
describeActiveReceiptRuleSetResponse_httpStatus :: Lens.Lens' DescribeActiveReceiptRuleSetResponse Core.Int
describeActiveReceiptRuleSetResponse_httpStatus = Lens.lens (\DescribeActiveReceiptRuleSetResponse' {httpStatus} -> httpStatus) (\s@DescribeActiveReceiptRuleSetResponse' {} a -> s {httpStatus = a} :: DescribeActiveReceiptRuleSetResponse)

instance
  Core.NFData
    DescribeActiveReceiptRuleSetResponse

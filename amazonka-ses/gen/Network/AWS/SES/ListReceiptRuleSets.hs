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
-- Module      : Network.AWS.SES.ListReceiptRuleSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the receipt rule sets that exist under your AWS account in the
-- current AWS Region. If there are additional receipt rule sets to be
-- retrieved, you will receive a @NextToken@ that you can provide to the
-- next call to @ListReceiptRuleSets@ to retrieve the additional entries.
--
-- For information about managing receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListReceiptRuleSets
  ( -- * Creating a Request
    ListReceiptRuleSets (..),
    newListReceiptRuleSets,

    -- * Request Lenses
    listReceiptRuleSets_nextToken,

    -- * Destructuring the Response
    ListReceiptRuleSetsResponse (..),
    newListReceiptRuleSetsResponse,

    -- * Response Lenses
    listReceiptRuleSetsResponse_ruleSets,
    listReceiptRuleSetsResponse_nextToken,
    listReceiptRuleSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to list the receipt rule sets that exist under your
-- AWS account. You use receipt rule sets to receive email with Amazon SES.
-- For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListReceiptRuleSets' smart constructor.
data ListReceiptRuleSets = ListReceiptRuleSets'
  { -- | A token returned from a previous call to @ListReceiptRuleSets@ to
    -- indicate the position in the receipt rule set list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReceiptRuleSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReceiptRuleSets_nextToken' - A token returned from a previous call to @ListReceiptRuleSets@ to
-- indicate the position in the receipt rule set list.
newListReceiptRuleSets ::
  ListReceiptRuleSets
newListReceiptRuleSets =
  ListReceiptRuleSets' {nextToken = Core.Nothing}

-- | A token returned from a previous call to @ListReceiptRuleSets@ to
-- indicate the position in the receipt rule set list.
listReceiptRuleSets_nextToken :: Lens.Lens' ListReceiptRuleSets (Core.Maybe Core.Text)
listReceiptRuleSets_nextToken = Lens.lens (\ListReceiptRuleSets' {nextToken} -> nextToken) (\s@ListReceiptRuleSets' {} a -> s {nextToken = a} :: ListReceiptRuleSets)

instance Core.AWSPager ListReceiptRuleSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReceiptRuleSetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listReceiptRuleSetsResponse_ruleSets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listReceiptRuleSets_nextToken
          Lens..~ rs
          Lens.^? listReceiptRuleSetsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListReceiptRuleSets where
  type
    AWSResponse ListReceiptRuleSets =
      ListReceiptRuleSetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListReceiptRuleSetsResult"
      ( \s h x ->
          ListReceiptRuleSetsResponse'
            Core.<$> ( x Core..@? "RuleSets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReceiptRuleSets

instance Core.NFData ListReceiptRuleSets

instance Core.ToHeaders ListReceiptRuleSets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListReceiptRuleSets where
  toPath = Core.const "/"

instance Core.ToQuery ListReceiptRuleSets where
  toQuery ListReceiptRuleSets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListReceiptRuleSets" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | A list of receipt rule sets that exist under your AWS account.
--
-- /See:/ 'newListReceiptRuleSetsResponse' smart constructor.
data ListReceiptRuleSetsResponse = ListReceiptRuleSetsResponse'
  { -- | The metadata for the currently active receipt rule set. The metadata
    -- consists of the rule set name and the timestamp of when the rule set was
    -- created.
    ruleSets :: Core.Maybe [ReceiptRuleSetMetadata],
    -- | A token indicating that there are additional receipt rule sets available
    -- to be listed. Pass this token to successive calls of
    -- @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReceiptRuleSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSets', 'listReceiptRuleSetsResponse_ruleSets' - The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and the timestamp of when the rule set was
-- created.
--
-- 'nextToken', 'listReceiptRuleSetsResponse_nextToken' - A token indicating that there are additional receipt rule sets available
-- to be listed. Pass this token to successive calls of
-- @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
--
-- 'httpStatus', 'listReceiptRuleSetsResponse_httpStatus' - The response's http status code.
newListReceiptRuleSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReceiptRuleSetsResponse
newListReceiptRuleSetsResponse pHttpStatus_ =
  ListReceiptRuleSetsResponse'
    { ruleSets =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for the currently active receipt rule set. The metadata
-- consists of the rule set name and the timestamp of when the rule set was
-- created.
listReceiptRuleSetsResponse_ruleSets :: Lens.Lens' ListReceiptRuleSetsResponse (Core.Maybe [ReceiptRuleSetMetadata])
listReceiptRuleSetsResponse_ruleSets = Lens.lens (\ListReceiptRuleSetsResponse' {ruleSets} -> ruleSets) (\s@ListReceiptRuleSetsResponse' {} a -> s {ruleSets = a} :: ListReceiptRuleSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | A token indicating that there are additional receipt rule sets available
-- to be listed. Pass this token to successive calls of
-- @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
listReceiptRuleSetsResponse_nextToken :: Lens.Lens' ListReceiptRuleSetsResponse (Core.Maybe Core.Text)
listReceiptRuleSetsResponse_nextToken = Lens.lens (\ListReceiptRuleSetsResponse' {nextToken} -> nextToken) (\s@ListReceiptRuleSetsResponse' {} a -> s {nextToken = a} :: ListReceiptRuleSetsResponse)

-- | The response's http status code.
listReceiptRuleSetsResponse_httpStatus :: Lens.Lens' ListReceiptRuleSetsResponse Core.Int
listReceiptRuleSetsResponse_httpStatus = Lens.lens (\ListReceiptRuleSetsResponse' {httpStatus} -> httpStatus) (\s@ListReceiptRuleSetsResponse' {} a -> s {httpStatus = a} :: ListReceiptRuleSetsResponse)

instance Core.NFData ListReceiptRuleSetsResponse

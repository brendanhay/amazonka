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
-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns communications and attachments for one or more support cases.
-- Use the @afterTime@ and @beforeTime@ parameters to filter by date. You
-- can use the @caseId@ parameter to restrict the results to a specific
-- case.
--
-- Case data is available for 12 months after creation. If a case was
-- created more than 12 months ago, a request for data might cause an
-- error.
--
-- You can use the @maxResults@ and @nextToken@ parameters to control the
-- pagination of the results. Set @maxResults@ to the number of cases that
-- you want to display on each page, and use @nextToken@ to specify the
-- resumption of pagination.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCommunications
  ( -- * Creating a Request
    DescribeCommunications (..),
    newDescribeCommunications,

    -- * Request Lenses
    describeCommunications_nextToken,
    describeCommunications_maxResults,
    describeCommunications_beforeTime,
    describeCommunications_afterTime,
    describeCommunications_caseId,

    -- * Destructuring the Response
    DescribeCommunicationsResponse (..),
    newDescribeCommunicationsResponse,

    -- * Response Lenses
    describeCommunicationsResponse_nextToken,
    describeCommunicationsResponse_communications,
    describeCommunicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeCommunications' smart constructor.
data DescribeCommunications = DescribeCommunications'
  { -- | A resumption point for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return before paginating.
    maxResults :: Core.Maybe Core.Natural,
    -- | The end date for a filtered date search on support case communications.
    -- Case communications are available for 12 months after creation.
    beforeTime :: Core.Maybe Core.Text,
    -- | The start date for a filtered date search on support case
    -- communications. Case communications are available for 12 months after
    -- creation.
    afterTime :: Core.Maybe Core.Text,
    -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCommunications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCommunications_nextToken' - A resumption point for pagination.
--
-- 'maxResults', 'describeCommunications_maxResults' - The maximum number of results to return before paginating.
--
-- 'beforeTime', 'describeCommunications_beforeTime' - The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
--
-- 'afterTime', 'describeCommunications_afterTime' - The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
--
-- 'caseId', 'describeCommunications_caseId' - The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
newDescribeCommunications ::
  -- | 'caseId'
  Core.Text ->
  DescribeCommunications
newDescribeCommunications pCaseId_ =
  DescribeCommunications'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      beforeTime = Core.Nothing,
      afterTime = Core.Nothing,
      caseId = pCaseId_
    }

-- | A resumption point for pagination.
describeCommunications_nextToken :: Lens.Lens' DescribeCommunications (Core.Maybe Core.Text)
describeCommunications_nextToken = Lens.lens (\DescribeCommunications' {nextToken} -> nextToken) (\s@DescribeCommunications' {} a -> s {nextToken = a} :: DescribeCommunications)

-- | The maximum number of results to return before paginating.
describeCommunications_maxResults :: Lens.Lens' DescribeCommunications (Core.Maybe Core.Natural)
describeCommunications_maxResults = Lens.lens (\DescribeCommunications' {maxResults} -> maxResults) (\s@DescribeCommunications' {} a -> s {maxResults = a} :: DescribeCommunications)

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
describeCommunications_beforeTime :: Lens.Lens' DescribeCommunications (Core.Maybe Core.Text)
describeCommunications_beforeTime = Lens.lens (\DescribeCommunications' {beforeTime} -> beforeTime) (\s@DescribeCommunications' {} a -> s {beforeTime = a} :: DescribeCommunications)

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
describeCommunications_afterTime :: Lens.Lens' DescribeCommunications (Core.Maybe Core.Text)
describeCommunications_afterTime = Lens.lens (\DescribeCommunications' {afterTime} -> afterTime) (\s@DescribeCommunications' {} a -> s {afterTime = a} :: DescribeCommunications)

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
describeCommunications_caseId :: Lens.Lens' DescribeCommunications Core.Text
describeCommunications_caseId = Lens.lens (\DescribeCommunications' {caseId} -> caseId) (\s@DescribeCommunications' {} a -> s {caseId = a} :: DescribeCommunications)

instance Core.AWSPager DescribeCommunications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCommunicationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCommunicationsResponse_communications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCommunications_nextToken
          Lens..~ rs
          Lens.^? describeCommunicationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeCommunications where
  type
    AWSResponse DescribeCommunications =
      DescribeCommunicationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCommunicationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "communications" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCommunications

instance Core.NFData DescribeCommunications

instance Core.ToHeaders DescribeCommunications where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeCommunications" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCommunications where
  toJSON DescribeCommunications' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("beforeTime" Core..=) Core.<$> beforeTime,
            ("afterTime" Core..=) Core.<$> afterTime,
            Core.Just ("caseId" Core..= caseId)
          ]
      )

instance Core.ToPath DescribeCommunications where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCommunications where
  toQuery = Core.const Core.mempty

-- | The communications returned by the DescribeCommunications operation.
--
-- /See:/ 'newDescribeCommunicationsResponse' smart constructor.
data DescribeCommunicationsResponse = DescribeCommunicationsResponse'
  { -- | A resumption point for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The communications for the case.
    communications :: Core.Maybe [Communication],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCommunicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCommunicationsResponse_nextToken' - A resumption point for pagination.
--
-- 'communications', 'describeCommunicationsResponse_communications' - The communications for the case.
--
-- 'httpStatus', 'describeCommunicationsResponse_httpStatus' - The response's http status code.
newDescribeCommunicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCommunicationsResponse
newDescribeCommunicationsResponse pHttpStatus_ =
  DescribeCommunicationsResponse'
    { nextToken =
        Core.Nothing,
      communications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A resumption point for pagination.
describeCommunicationsResponse_nextToken :: Lens.Lens' DescribeCommunicationsResponse (Core.Maybe Core.Text)
describeCommunicationsResponse_nextToken = Lens.lens (\DescribeCommunicationsResponse' {nextToken} -> nextToken) (\s@DescribeCommunicationsResponse' {} a -> s {nextToken = a} :: DescribeCommunicationsResponse)

-- | The communications for the case.
describeCommunicationsResponse_communications :: Lens.Lens' DescribeCommunicationsResponse (Core.Maybe [Communication])
describeCommunicationsResponse_communications = Lens.lens (\DescribeCommunicationsResponse' {communications} -> communications) (\s@DescribeCommunicationsResponse' {} a -> s {communications = a} :: DescribeCommunicationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCommunicationsResponse_httpStatus :: Lens.Lens' DescribeCommunicationsResponse Core.Int
describeCommunicationsResponse_httpStatus = Lens.lens (\DescribeCommunicationsResponse' {httpStatus} -> httpStatus) (\s@DescribeCommunicationsResponse' {} a -> s {httpStatus = a} :: DescribeCommunicationsResponse)

instance Core.NFData DescribeCommunicationsResponse

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
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues in the current region. The response
-- includes a maximum of 1,000 results. If you specify a value for the
-- optional @QueueNamePrefix@ parameter, only queues with a name that
-- begins with the specified value are returned.
--
-- The @listQueues@ methods supports pagination. Set parameter @MaxResults@
-- in the request to specify the maximum number of results to be returned
-- in the response. If you do not set @MaxResults@, the response includes a
-- maximum of 1,000 results. If you set @MaxResults@ and there are
-- additional results to display, the response includes a value for
-- @NextToken@. Use @NextToken@ as a parameter in your next request to
-- @listQueues@ to receive the next page of results.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListQueues
  ( -- * Creating a Request
    ListQueues (..),
    newListQueues,

    -- * Request Lenses
    listQueues_nextToken,
    listQueues_maxResults,
    listQueues_queueNamePrefix,

    -- * Destructuring the Response
    ListQueuesResponse (..),
    newListQueuesResponse,

    -- * Response Lenses
    listQueuesResponse_nextToken,
    listQueuesResponse_queueUrls,
    listQueuesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | Pagination token to request the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to include in the response. Value range is 1
    -- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
    -- the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A string to use for filtering the list results. Only those queues whose
    -- name begins with the specified string are returned.
    --
    -- Queue URLs and names are case-sensitive.
    queueNamePrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueues_nextToken' - Pagination token to request the next set of results.
--
-- 'maxResults', 'listQueues_maxResults' - Maximum number of results to include in the response. Value range is 1
-- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
-- the response.
--
-- 'queueNamePrefix', 'listQueues_queueNamePrefix' - A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
--
-- Queue URLs and names are case-sensitive.
newListQueues ::
  ListQueues
newListQueues =
  ListQueues'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      queueNamePrefix = Prelude.Nothing
    }

-- | Pagination token to request the next set of results.
listQueues_nextToken :: Lens.Lens' ListQueues (Prelude.Maybe Prelude.Text)
listQueues_nextToken = Lens.lens (\ListQueues' {nextToken} -> nextToken) (\s@ListQueues' {} a -> s {nextToken = a} :: ListQueues)

-- | Maximum number of results to include in the response. Value range is 1
-- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
-- the response.
listQueues_maxResults :: Lens.Lens' ListQueues (Prelude.Maybe Prelude.Int)
listQueues_maxResults = Lens.lens (\ListQueues' {maxResults} -> maxResults) (\s@ListQueues' {} a -> s {maxResults = a} :: ListQueues)

-- | A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
--
-- Queue URLs and names are case-sensitive.
listQueues_queueNamePrefix :: Lens.Lens' ListQueues (Prelude.Maybe Prelude.Text)
listQueues_queueNamePrefix = Lens.lens (\ListQueues' {queueNamePrefix} -> queueNamePrefix) (\s@ListQueues' {} a -> s {queueNamePrefix = a} :: ListQueues)

instance Pager.AWSPager ListQueues where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listQueuesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listQueuesResponse_queueUrls Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listQueues_nextToken
          Lens..~ rs
          Lens.^? listQueuesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListQueuesResult"
      ( \s h x ->
          ListQueuesResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> (Prelude.may (Prelude.parseXMLList "QueueUrl") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueues

instance Prelude.NFData ListQueues

instance Prelude.ToHeaders ListQueues where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListQueues where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListQueues" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults,
        "QueueNamePrefix" Prelude.=: queueNamePrefix
      ]

-- | A list of your queues.
--
-- /See:/ 'newListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | Pagination token to include in the next request. Token value is @null@
    -- if there are no additional results to request, or if you did not set
    -- @MaxResults@ in the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults
    -- that you sent in the request.
    queueUrls :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueuesResponse_nextToken' - Pagination token to include in the next request. Token value is @null@
-- if there are no additional results to request, or if you did not set
-- @MaxResults@ in the request.
--
-- 'queueUrls', 'listQueuesResponse_queueUrls' - A list of queue URLs, up to 1,000 entries, or the value of MaxResults
-- that you sent in the request.
--
-- 'httpStatus', 'listQueuesResponse_httpStatus' - The response's http status code.
newListQueuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueuesResponse
newListQueuesResponse pHttpStatus_ =
  ListQueuesResponse'
    { nextToken = Prelude.Nothing,
      queueUrls = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token to include in the next request. Token value is @null@
-- if there are no additional results to request, or if you did not set
-- @MaxResults@ in the request.
listQueuesResponse_nextToken :: Lens.Lens' ListQueuesResponse (Prelude.Maybe Prelude.Text)
listQueuesResponse_nextToken = Lens.lens (\ListQueuesResponse' {nextToken} -> nextToken) (\s@ListQueuesResponse' {} a -> s {nextToken = a} :: ListQueuesResponse)

-- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults
-- that you sent in the request.
listQueuesResponse_queueUrls :: Lens.Lens' ListQueuesResponse (Prelude.Maybe [Prelude.Text])
listQueuesResponse_queueUrls = Lens.lens (\ListQueuesResponse' {queueUrls} -> queueUrls) (\s@ListQueuesResponse' {} a -> s {queueUrls = a} :: ListQueuesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listQueuesResponse_httpStatus :: Lens.Lens' ListQueuesResponse Prelude.Int
listQueuesResponse_httpStatus = Lens.lens (\ListQueuesResponse' {httpStatus} -> httpStatus) (\s@ListQueuesResponse' {} a -> s {httpStatus = a} :: ListQueuesResponse)

instance Prelude.NFData ListQueuesResponse

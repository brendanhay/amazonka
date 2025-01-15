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
-- Module      : Amazonka.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the @RedrivePolicy@ queue
-- attribute configured with a dead-letter queue.
--
-- The @ListDeadLetterSourceQueues@ methods supports pagination. Set
-- parameter @MaxResults@ in the request to specify the maximum number of
-- results to be returned in the response. If you do not set @MaxResults@,
-- the response includes a maximum of 1,000 results. If you set
-- @MaxResults@ and there are additional results to display, the response
-- includes a value for @NextToken@. Use @NextToken@ as a parameter in your
-- next request to @ListDeadLetterSourceQueues@ to receive the next page of
-- results.
--
-- For more information about using dead-letter queues, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SQS.ListDeadLetterSourceQueues
  ( -- * Creating a Request
    ListDeadLetterSourceQueues (..),
    newListDeadLetterSourceQueues,

    -- * Request Lenses
    listDeadLetterSourceQueues_maxResults,
    listDeadLetterSourceQueues_nextToken,
    listDeadLetterSourceQueues_queueUrl,

    -- * Destructuring the Response
    ListDeadLetterSourceQueuesResponse (..),
    newListDeadLetterSourceQueuesResponse,

    -- * Response Lenses
    listDeadLetterSourceQueuesResponse_nextToken,
    listDeadLetterSourceQueuesResponse_httpStatus,
    listDeadLetterSourceQueuesResponse_queueUrls,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newListDeadLetterSourceQueues' smart constructor.
data ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
  { -- | Maximum number of results to include in the response. Value range is 1
    -- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
    -- the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Pagination token to request the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The URL of a dead-letter queue.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeadLetterSourceQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeadLetterSourceQueues_maxResults' - Maximum number of results to include in the response. Value range is 1
-- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
-- the response.
--
-- 'nextToken', 'listDeadLetterSourceQueues_nextToken' - Pagination token to request the next set of results.
--
-- 'queueUrl', 'listDeadLetterSourceQueues_queueUrl' - The URL of a dead-letter queue.
--
-- Queue URLs and names are case-sensitive.
newListDeadLetterSourceQueues ::
  -- | 'queueUrl'
  Prelude.Text ->
  ListDeadLetterSourceQueues
newListDeadLetterSourceQueues pQueueUrl_ =
  ListDeadLetterSourceQueues'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queueUrl = pQueueUrl_
    }

-- | Maximum number of results to include in the response. Value range is 1
-- to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in
-- the response.
listDeadLetterSourceQueues_maxResults :: Lens.Lens' ListDeadLetterSourceQueues (Prelude.Maybe Prelude.Int)
listDeadLetterSourceQueues_maxResults = Lens.lens (\ListDeadLetterSourceQueues' {maxResults} -> maxResults) (\s@ListDeadLetterSourceQueues' {} a -> s {maxResults = a} :: ListDeadLetterSourceQueues)

-- | Pagination token to request the next set of results.
listDeadLetterSourceQueues_nextToken :: Lens.Lens' ListDeadLetterSourceQueues (Prelude.Maybe Prelude.Text)
listDeadLetterSourceQueues_nextToken = Lens.lens (\ListDeadLetterSourceQueues' {nextToken} -> nextToken) (\s@ListDeadLetterSourceQueues' {} a -> s {nextToken = a} :: ListDeadLetterSourceQueues)

-- | The URL of a dead-letter queue.
--
-- Queue URLs and names are case-sensitive.
listDeadLetterSourceQueues_queueUrl :: Lens.Lens' ListDeadLetterSourceQueues Prelude.Text
listDeadLetterSourceQueues_queueUrl = Lens.lens (\ListDeadLetterSourceQueues' {queueUrl} -> queueUrl) (\s@ListDeadLetterSourceQueues' {} a -> s {queueUrl = a} :: ListDeadLetterSourceQueues)

instance Core.AWSPager ListDeadLetterSourceQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeadLetterSourceQueuesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDeadLetterSourceQueuesResponse_queueUrls
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDeadLetterSourceQueues_nextToken
              Lens..~ rs
              Lens.^? listDeadLetterSourceQueuesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDeadLetterSourceQueues where
  type
    AWSResponse ListDeadLetterSourceQueues =
      ListDeadLetterSourceQueuesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListDeadLetterSourceQueuesResult"
      ( \s h x ->
          ListDeadLetterSourceQueuesResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXMLList "QueueUrl" x)
      )

instance Prelude.Hashable ListDeadLetterSourceQueues where
  hashWithSalt _salt ListDeadLetterSourceQueues' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData ListDeadLetterSourceQueues where
  rnf ListDeadLetterSourceQueues' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf queueUrl

instance Data.ToHeaders ListDeadLetterSourceQueues where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDeadLetterSourceQueues where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeadLetterSourceQueues where
  toQuery ListDeadLetterSourceQueues' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListDeadLetterSourceQueues" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "QueueUrl" Data.=: queueUrl
      ]

-- | A list of your dead letter source queues.
--
-- /See:/ 'newListDeadLetterSourceQueuesResponse' smart constructor.
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
  { -- | Pagination token to include in the next request. Token value is @null@
    -- if there are no additional results to request, or if you did not set
    -- @MaxResults@ in the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of source queue URLs that have the @RedrivePolicy@ queue
    -- attribute configured with a dead-letter queue.
    queueUrls :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeadLetterSourceQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeadLetterSourceQueuesResponse_nextToken' - Pagination token to include in the next request. Token value is @null@
-- if there are no additional results to request, or if you did not set
-- @MaxResults@ in the request.
--
-- 'httpStatus', 'listDeadLetterSourceQueuesResponse_httpStatus' - The response's http status code.
--
-- 'queueUrls', 'listDeadLetterSourceQueuesResponse_queueUrls' - A list of source queue URLs that have the @RedrivePolicy@ queue
-- attribute configured with a dead-letter queue.
newListDeadLetterSourceQueuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeadLetterSourceQueuesResponse
newListDeadLetterSourceQueuesResponse pHttpStatus_ =
  ListDeadLetterSourceQueuesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      queueUrls = Prelude.mempty
    }

-- | Pagination token to include in the next request. Token value is @null@
-- if there are no additional results to request, or if you did not set
-- @MaxResults@ in the request.
listDeadLetterSourceQueuesResponse_nextToken :: Lens.Lens' ListDeadLetterSourceQueuesResponse (Prelude.Maybe Prelude.Text)
listDeadLetterSourceQueuesResponse_nextToken = Lens.lens (\ListDeadLetterSourceQueuesResponse' {nextToken} -> nextToken) (\s@ListDeadLetterSourceQueuesResponse' {} a -> s {nextToken = a} :: ListDeadLetterSourceQueuesResponse)

-- | The response's http status code.
listDeadLetterSourceQueuesResponse_httpStatus :: Lens.Lens' ListDeadLetterSourceQueuesResponse Prelude.Int
listDeadLetterSourceQueuesResponse_httpStatus = Lens.lens (\ListDeadLetterSourceQueuesResponse' {httpStatus} -> httpStatus) (\s@ListDeadLetterSourceQueuesResponse' {} a -> s {httpStatus = a} :: ListDeadLetterSourceQueuesResponse)

-- | A list of source queue URLs that have the @RedrivePolicy@ queue
-- attribute configured with a dead-letter queue.
listDeadLetterSourceQueuesResponse_queueUrls :: Lens.Lens' ListDeadLetterSourceQueuesResponse [Prelude.Text]
listDeadLetterSourceQueuesResponse_queueUrls = Lens.lens (\ListDeadLetterSourceQueuesResponse' {queueUrls} -> queueUrls) (\s@ListDeadLetterSourceQueuesResponse' {} a -> s {queueUrls = a} :: ListDeadLetterSourceQueuesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDeadLetterSourceQueuesResponse
  where
  rnf ListDeadLetterSourceQueuesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf queueUrls

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
-- Module      : Amazonka.QuickSight.ListTopics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the topics within an account.
module Amazonka.QuickSight.ListTopics
  ( -- * Creating a Request
    ListTopics (..),
    newListTopics,

    -- * Request Lenses
    listTopics_maxResults,
    listTopics_nextToken,
    listTopics_awsAccountId,

    -- * Destructuring the Response
    ListTopicsResponse (..),
    newListTopicsResponse,

    -- * Response Lenses
    listTopicsResponse_nextToken,
    listTopicsResponse_requestId,
    listTopicsResponse_topicsSummaries,
    listTopicsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTopics' smart constructor.
data ListTopics = ListTopics'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the topics that
    -- you want to list.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTopics_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listTopics_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listTopics_awsAccountId' - The ID of the Amazon Web Services account that contains the topics that
-- you want to list.
newListTopics ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListTopics
newListTopics pAwsAccountId_ =
  ListTopics'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listTopics_maxResults :: Lens.Lens' ListTopics (Prelude.Maybe Prelude.Natural)
listTopics_maxResults = Lens.lens (\ListTopics' {maxResults} -> maxResults) (\s@ListTopics' {} a -> s {maxResults = a} :: ListTopics)

-- | The token for the next set of results, or null if there are no more
-- results.
listTopics_nextToken :: Lens.Lens' ListTopics (Prelude.Maybe Prelude.Text)
listTopics_nextToken = Lens.lens (\ListTopics' {nextToken} -> nextToken) (\s@ListTopics' {} a -> s {nextToken = a} :: ListTopics)

-- | The ID of the Amazon Web Services account that contains the topics that
-- you want to list.
listTopics_awsAccountId :: Lens.Lens' ListTopics Prelude.Text
listTopics_awsAccountId = Lens.lens (\ListTopics' {awsAccountId} -> awsAccountId) (\s@ListTopics' {} a -> s {awsAccountId = a} :: ListTopics)

instance Core.AWSRequest ListTopics where
  type AWSResponse ListTopics = ListTopicsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "TopicsSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopics where
  hashWithSalt _salt ListTopics' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListTopics where
  rnf ListTopics' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListTopics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTopics where
  toPath ListTopics' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/topics"]

instance Data.ToQuery ListTopics where
  toQuery ListTopics' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A list of topic summaries.
    topicsSummaries :: Prelude.Maybe [TopicSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listTopicsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicsSummaries', 'listTopicsResponse_topicsSummaries' - A list of topic summaries.
--
-- 'status', 'listTopicsResponse_status' - The HTTP status of the request.
newListTopicsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListTopicsResponse
newListTopicsResponse pStatus_ =
  ListTopicsResponse'
    { nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicsSummaries = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listTopicsResponse_nextToken :: Lens.Lens' ListTopicsResponse (Prelude.Maybe Prelude.Text)
listTopicsResponse_nextToken = Lens.lens (\ListTopicsResponse' {nextToken} -> nextToken) (\s@ListTopicsResponse' {} a -> s {nextToken = a} :: ListTopicsResponse)

-- | The Amazon Web Services request ID for this operation.
listTopicsResponse_requestId :: Lens.Lens' ListTopicsResponse (Prelude.Maybe Prelude.Text)
listTopicsResponse_requestId = Lens.lens (\ListTopicsResponse' {requestId} -> requestId) (\s@ListTopicsResponse' {} a -> s {requestId = a} :: ListTopicsResponse)

-- | A list of topic summaries.
listTopicsResponse_topicsSummaries :: Lens.Lens' ListTopicsResponse (Prelude.Maybe [TopicSummary])
listTopicsResponse_topicsSummaries = Lens.lens (\ListTopicsResponse' {topicsSummaries} -> topicsSummaries) (\s@ListTopicsResponse' {} a -> s {topicsSummaries = a} :: ListTopicsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listTopicsResponse_status :: Lens.Lens' ListTopicsResponse Prelude.Int
listTopicsResponse_status = Lens.lens (\ListTopicsResponse' {status} -> status) (\s@ListTopicsResponse' {} a -> s {status = a} :: ListTopicsResponse)

instance Prelude.NFData ListTopicsResponse where
  rnf ListTopicsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicsSummaries
      `Prelude.seq` Prelude.rnf status

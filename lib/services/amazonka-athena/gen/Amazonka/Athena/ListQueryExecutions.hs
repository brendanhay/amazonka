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
-- Module      : Amazonka.Athena.ListQueryExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of available query execution IDs for the queries in the
-- specified workgroup. If a workgroup is not specified, returns a list of
-- query execution IDs for the primary workgroup. Requires you to have
-- access to the workgroup in which the queries ran.
--
-- For code samples using the Amazon Web Services SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
--
-- This operation returns paginated results.
module Amazonka.Athena.ListQueryExecutions
  ( -- * Creating a Request
    ListQueryExecutions (..),
    newListQueryExecutions,

    -- * Request Lenses
    listQueryExecutions_maxResults,
    listQueryExecutions_nextToken,
    listQueryExecutions_workGroup,

    -- * Destructuring the Response
    ListQueryExecutionsResponse (..),
    newListQueryExecutionsResponse,

    -- * Response Lenses
    listQueryExecutionsResponse_nextToken,
    listQueryExecutionsResponse_queryExecutionIds,
    listQueryExecutionsResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListQueryExecutions' smart constructor.
data ListQueryExecutions = ListQueryExecutions'
  { -- | The maximum number of query executions to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the @NextToken@ from the response object of the
    -- previous page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup from which queries are being returned. If a
    -- workgroup is not specified, a list of available query execution IDs for
    -- the queries in the primary workgroup is returned.
    workGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueryExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listQueryExecutions_maxResults' - The maximum number of query executions to return in this request.
--
-- 'nextToken', 'listQueryExecutions_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
--
-- 'workGroup', 'listQueryExecutions_workGroup' - The name of the workgroup from which queries are being returned. If a
-- workgroup is not specified, a list of available query execution IDs for
-- the queries in the primary workgroup is returned.
newListQueryExecutions ::
  ListQueryExecutions
newListQueryExecutions =
  ListQueryExecutions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workGroup = Prelude.Nothing
    }

-- | The maximum number of query executions to return in this request.
listQueryExecutions_maxResults :: Lens.Lens' ListQueryExecutions (Prelude.Maybe Prelude.Natural)
listQueryExecutions_maxResults = Lens.lens (\ListQueryExecutions' {maxResults} -> maxResults) (\s@ListQueryExecutions' {} a -> s {maxResults = a} :: ListQueryExecutions)

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
listQueryExecutions_nextToken :: Lens.Lens' ListQueryExecutions (Prelude.Maybe Prelude.Text)
listQueryExecutions_nextToken = Lens.lens (\ListQueryExecutions' {nextToken} -> nextToken) (\s@ListQueryExecutions' {} a -> s {nextToken = a} :: ListQueryExecutions)

-- | The name of the workgroup from which queries are being returned. If a
-- workgroup is not specified, a list of available query execution IDs for
-- the queries in the primary workgroup is returned.
listQueryExecutions_workGroup :: Lens.Lens' ListQueryExecutions (Prelude.Maybe Prelude.Text)
listQueryExecutions_workGroup = Lens.lens (\ListQueryExecutions' {workGroup} -> workGroup) (\s@ListQueryExecutions' {} a -> s {workGroup = a} :: ListQueryExecutions)

instance Core.AWSPager ListQueryExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueryExecutionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listQueryExecutionsResponse_queryExecutionIds
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listQueryExecutions_nextToken
              Lens..~ rs
              Lens.^? listQueryExecutionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListQueryExecutions where
  type
    AWSResponse ListQueryExecutions =
      ListQueryExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueryExecutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "QueryExecutionIds")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueryExecutions where
  hashWithSalt _salt ListQueryExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData ListQueryExecutions where
  rnf ListQueryExecutions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf workGroup

instance Data.ToHeaders ListQueryExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.ListQueryExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListQueryExecutions where
  toJSON ListQueryExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("WorkGroup" Data..=) Prelude.<$> workGroup
          ]
      )

instance Data.ToPath ListQueryExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListQueryExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListQueryExecutionsResponse' smart constructor.
data ListQueryExecutionsResponse = ListQueryExecutionsResponse'
  { -- | A token to be used by the next request if this request is truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique IDs of each query execution as an array of strings.
    queryExecutionIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueryExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueryExecutionsResponse_nextToken' - A token to be used by the next request if this request is truncated.
--
-- 'queryExecutionIds', 'listQueryExecutionsResponse_queryExecutionIds' - The unique IDs of each query execution as an array of strings.
--
-- 'httpStatus', 'listQueryExecutionsResponse_httpStatus' - The response's http status code.
newListQueryExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueryExecutionsResponse
newListQueryExecutionsResponse pHttpStatus_ =
  ListQueryExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      queryExecutionIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to be used by the next request if this request is truncated.
listQueryExecutionsResponse_nextToken :: Lens.Lens' ListQueryExecutionsResponse (Prelude.Maybe Prelude.Text)
listQueryExecutionsResponse_nextToken = Lens.lens (\ListQueryExecutionsResponse' {nextToken} -> nextToken) (\s@ListQueryExecutionsResponse' {} a -> s {nextToken = a} :: ListQueryExecutionsResponse)

-- | The unique IDs of each query execution as an array of strings.
listQueryExecutionsResponse_queryExecutionIds :: Lens.Lens' ListQueryExecutionsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listQueryExecutionsResponse_queryExecutionIds = Lens.lens (\ListQueryExecutionsResponse' {queryExecutionIds} -> queryExecutionIds) (\s@ListQueryExecutionsResponse' {} a -> s {queryExecutionIds = a} :: ListQueryExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listQueryExecutionsResponse_httpStatus :: Lens.Lens' ListQueryExecutionsResponse Prelude.Int
listQueryExecutionsResponse_httpStatus = Lens.lens (\ListQueryExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListQueryExecutionsResponse' {} a -> s {httpStatus = a} :: ListQueryExecutionsResponse)

instance Prelude.NFData ListQueryExecutionsResponse where
  rnf ListQueryExecutionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf queryExecutionIds `Prelude.seq`
        Prelude.rnf httpStatus

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
-- Module      : Amazonka.RedshiftData.ListStatements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List of SQL statements. By default, only finished statements are shown.
-- A token is returned to page through the statement list.
--
-- This operation returns paginated results.
module Amazonka.RedshiftData.ListStatements
  ( -- * Creating a Request
    ListStatements (..),
    newListStatements,

    -- * Request Lenses
    listStatements_roleLevel,
    listStatements_nextToken,
    listStatements_status,
    listStatements_maxResults,
    listStatements_statementName,

    -- * Destructuring the Response
    ListStatementsResponse (..),
    newListStatementsResponse,

    -- * Response Lenses
    listStatementsResponse_nextToken,
    listStatementsResponse_httpStatus,
    listStatementsResponse_statements,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStatements' smart constructor.
data ListStatements = ListStatements'
  { -- | A value that filters which statements to return in the response. If
    -- true, all statements run by the caller\'s IAM role are returned. If
    -- false, only statements run by the caller\'s IAM role in the current IAM
    -- session are returned. The default is true.
    roleLevel :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the SQL statement to list. Status values are defined as
    -- follows:
    --
    -- -   ABORTED - The query run was stopped by the user.
    --
    -- -   ALL - A status value that includes all query statuses. This value
    --     can be used to filter results.
    --
    -- -   FAILED - The query run failed.
    --
    -- -   FINISHED - The query has finished running.
    --
    -- -   PICKED - The query has been chosen to be run.
    --
    -- -   STARTED - The query run has started.
    --
    -- -   SUBMITTED - The query was submitted, but not yet processed.
    status :: Prelude.Maybe StatusString,
    -- | The maximum number of SQL statements to return in the response. If more
    -- SQL statements exist than fit in one response, then @NextToken@ is
    -- returned to page through the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the SQL statement specified as input to
    -- @BatchExecuteStatement@ or @ExecuteStatement@ to identify the query. You
    -- can list multiple statements by providing a prefix that matches the
    -- beginning of the statement name. For example, to list myStatement1,
    -- myStatement2, myStatement3, and so on, then provide the a value of
    -- @myStatement@. Data API does a case-sensitive match of SQL statement
    -- names to the prefix value you provide.
    statementName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStatements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleLevel', 'listStatements_roleLevel' - A value that filters which statements to return in the response. If
-- true, all statements run by the caller\'s IAM role are returned. If
-- false, only statements run by the caller\'s IAM role in the current IAM
-- session are returned. The default is true.
--
-- 'nextToken', 'listStatements_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'status', 'listStatements_status' - The status of the SQL statement to list. Status values are defined as
-- follows:
--
-- -   ABORTED - The query run was stopped by the user.
--
-- -   ALL - A status value that includes all query statuses. This value
--     can be used to filter results.
--
-- -   FAILED - The query run failed.
--
-- -   FINISHED - The query has finished running.
--
-- -   PICKED - The query has been chosen to be run.
--
-- -   STARTED - The query run has started.
--
-- -   SUBMITTED - The query was submitted, but not yet processed.
--
-- 'maxResults', 'listStatements_maxResults' - The maximum number of SQL statements to return in the response. If more
-- SQL statements exist than fit in one response, then @NextToken@ is
-- returned to page through the results.
--
-- 'statementName', 'listStatements_statementName' - The name of the SQL statement specified as input to
-- @BatchExecuteStatement@ or @ExecuteStatement@ to identify the query. You
-- can list multiple statements by providing a prefix that matches the
-- beginning of the statement name. For example, to list myStatement1,
-- myStatement2, myStatement3, and so on, then provide the a value of
-- @myStatement@. Data API does a case-sensitive match of SQL statement
-- names to the prefix value you provide.
newListStatements ::
  ListStatements
newListStatements =
  ListStatements'
    { roleLevel = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statementName = Prelude.Nothing
    }

-- | A value that filters which statements to return in the response. If
-- true, all statements run by the caller\'s IAM role are returned. If
-- false, only statements run by the caller\'s IAM role in the current IAM
-- session are returned. The default is true.
listStatements_roleLevel :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Bool)
listStatements_roleLevel = Lens.lens (\ListStatements' {roleLevel} -> roleLevel) (\s@ListStatements' {} a -> s {roleLevel = a} :: ListStatements)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listStatements_nextToken :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Text)
listStatements_nextToken = Lens.lens (\ListStatements' {nextToken} -> nextToken) (\s@ListStatements' {} a -> s {nextToken = a} :: ListStatements)

-- | The status of the SQL statement to list. Status values are defined as
-- follows:
--
-- -   ABORTED - The query run was stopped by the user.
--
-- -   ALL - A status value that includes all query statuses. This value
--     can be used to filter results.
--
-- -   FAILED - The query run failed.
--
-- -   FINISHED - The query has finished running.
--
-- -   PICKED - The query has been chosen to be run.
--
-- -   STARTED - The query run has started.
--
-- -   SUBMITTED - The query was submitted, but not yet processed.
listStatements_status :: Lens.Lens' ListStatements (Prelude.Maybe StatusString)
listStatements_status = Lens.lens (\ListStatements' {status} -> status) (\s@ListStatements' {} a -> s {status = a} :: ListStatements)

-- | The maximum number of SQL statements to return in the response. If more
-- SQL statements exist than fit in one response, then @NextToken@ is
-- returned to page through the results.
listStatements_maxResults :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Natural)
listStatements_maxResults = Lens.lens (\ListStatements' {maxResults} -> maxResults) (\s@ListStatements' {} a -> s {maxResults = a} :: ListStatements)

-- | The name of the SQL statement specified as input to
-- @BatchExecuteStatement@ or @ExecuteStatement@ to identify the query. You
-- can list multiple statements by providing a prefix that matches the
-- beginning of the statement name. For example, to list myStatement1,
-- myStatement2, myStatement3, and so on, then provide the a value of
-- @myStatement@. Data API does a case-sensitive match of SQL statement
-- names to the prefix value you provide.
listStatements_statementName :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Text)
listStatements_statementName = Lens.lens (\ListStatements' {statementName} -> statementName) (\s@ListStatements' {} a -> s {statementName = a} :: ListStatements)

instance Core.AWSPager ListStatements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStatementsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listStatementsResponse_statements) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStatements_nextToken
          Lens..~ rs
          Lens.^? listStatementsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListStatements where
  type
    AWSResponse ListStatements =
      ListStatementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStatementsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Statements" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListStatements where
  hashWithSalt _salt ListStatements' {..} =
    _salt `Prelude.hashWithSalt` roleLevel
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` statementName

instance Prelude.NFData ListStatements where
  rnf ListStatements' {..} =
    Prelude.rnf roleLevel
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statementName

instance Core.ToHeaders ListStatements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftData.ListStatements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListStatements where
  toJSON ListStatements' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleLevel" Core..=) Prelude.<$> roleLevel,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Status" Core..=) Prelude.<$> status,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StatementName" Core..=) Prelude.<$> statementName
          ]
      )

instance Core.ToPath ListStatements where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStatements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStatementsResponse' smart constructor.
data ListStatementsResponse = ListStatementsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The SQL statements.
    statements :: [StatementData]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStatementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStatementsResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'httpStatus', 'listStatementsResponse_httpStatus' - The response's http status code.
--
-- 'statements', 'listStatementsResponse_statements' - The SQL statements.
newListStatementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStatementsResponse
newListStatementsResponse pHttpStatus_ =
  ListStatementsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      statements = Prelude.mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listStatementsResponse_nextToken :: Lens.Lens' ListStatementsResponse (Prelude.Maybe Prelude.Text)
listStatementsResponse_nextToken = Lens.lens (\ListStatementsResponse' {nextToken} -> nextToken) (\s@ListStatementsResponse' {} a -> s {nextToken = a} :: ListStatementsResponse)

-- | The response's http status code.
listStatementsResponse_httpStatus :: Lens.Lens' ListStatementsResponse Prelude.Int
listStatementsResponse_httpStatus = Lens.lens (\ListStatementsResponse' {httpStatus} -> httpStatus) (\s@ListStatementsResponse' {} a -> s {httpStatus = a} :: ListStatementsResponse)

-- | The SQL statements.
listStatementsResponse_statements :: Lens.Lens' ListStatementsResponse [StatementData]
listStatementsResponse_statements = Lens.lens (\ListStatementsResponse' {statements} -> statements) (\s@ListStatementsResponse' {} a -> s {statements = a} :: ListStatementsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStatementsResponse where
  rnf ListStatementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf statements

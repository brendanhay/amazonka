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
-- Module      : Amazonka.Athena.ListPreparedStatements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the prepared statements in the specified workgroup.
module Amazonka.Athena.ListPreparedStatements
  ( -- * Creating a Request
    ListPreparedStatements (..),
    newListPreparedStatements,

    -- * Request Lenses
    listPreparedStatements_maxResults,
    listPreparedStatements_nextToken,
    listPreparedStatements_workGroup,

    -- * Destructuring the Response
    ListPreparedStatementsResponse (..),
    newListPreparedStatementsResponse,

    -- * Response Lenses
    listPreparedStatementsResponse_nextToken,
    listPreparedStatementsResponse_preparedStatements,
    listPreparedStatementsResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPreparedStatements' smart constructor.
data ListPreparedStatements = ListPreparedStatements'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the @NextToken@ from the response object of the
    -- previous page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The workgroup to list the prepared statements for.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPreparedStatements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPreparedStatements_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listPreparedStatements_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
--
-- 'workGroup', 'listPreparedStatements_workGroup' - The workgroup to list the prepared statements for.
newListPreparedStatements ::
  -- | 'workGroup'
  Prelude.Text ->
  ListPreparedStatements
newListPreparedStatements pWorkGroup_ =
  ListPreparedStatements'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The maximum number of results to return in this request.
listPreparedStatements_maxResults :: Lens.Lens' ListPreparedStatements (Prelude.Maybe Prelude.Natural)
listPreparedStatements_maxResults = Lens.lens (\ListPreparedStatements' {maxResults} -> maxResults) (\s@ListPreparedStatements' {} a -> s {maxResults = a} :: ListPreparedStatements)

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
listPreparedStatements_nextToken :: Lens.Lens' ListPreparedStatements (Prelude.Maybe Prelude.Text)
listPreparedStatements_nextToken = Lens.lens (\ListPreparedStatements' {nextToken} -> nextToken) (\s@ListPreparedStatements' {} a -> s {nextToken = a} :: ListPreparedStatements)

-- | The workgroup to list the prepared statements for.
listPreparedStatements_workGroup :: Lens.Lens' ListPreparedStatements Prelude.Text
listPreparedStatements_workGroup = Lens.lens (\ListPreparedStatements' {workGroup} -> workGroup) (\s@ListPreparedStatements' {} a -> s {workGroup = a} :: ListPreparedStatements)

instance Core.AWSRequest ListPreparedStatements where
  type
    AWSResponse ListPreparedStatements =
      ListPreparedStatementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPreparedStatementsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "PreparedStatements"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPreparedStatements where
  hashWithSalt _salt ListPreparedStatements' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData ListPreparedStatements where
  rnf ListPreparedStatements' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workGroup

instance Data.ToHeaders ListPreparedStatements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.ListPreparedStatements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPreparedStatements where
  toJSON ListPreparedStatements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("WorkGroup" Data..= workGroup)
          ]
      )

instance Data.ToPath ListPreparedStatements where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPreparedStatements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPreparedStatementsResponse' smart constructor.
data ListPreparedStatementsResponse = ListPreparedStatementsResponse'
  { -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the @NextToken@ from the response object of the
    -- previous page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of prepared statements for the workgroup.
    preparedStatements :: Prelude.Maybe [PreparedStatementSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPreparedStatementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPreparedStatementsResponse_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
--
-- 'preparedStatements', 'listPreparedStatementsResponse_preparedStatements' - The list of prepared statements for the workgroup.
--
-- 'httpStatus', 'listPreparedStatementsResponse_httpStatus' - The response's http status code.
newListPreparedStatementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPreparedStatementsResponse
newListPreparedStatementsResponse pHttpStatus_ =
  ListPreparedStatementsResponse'
    { nextToken =
        Prelude.Nothing,
      preparedStatements = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
listPreparedStatementsResponse_nextToken :: Lens.Lens' ListPreparedStatementsResponse (Prelude.Maybe Prelude.Text)
listPreparedStatementsResponse_nextToken = Lens.lens (\ListPreparedStatementsResponse' {nextToken} -> nextToken) (\s@ListPreparedStatementsResponse' {} a -> s {nextToken = a} :: ListPreparedStatementsResponse)

-- | The list of prepared statements for the workgroup.
listPreparedStatementsResponse_preparedStatements :: Lens.Lens' ListPreparedStatementsResponse (Prelude.Maybe [PreparedStatementSummary])
listPreparedStatementsResponse_preparedStatements = Lens.lens (\ListPreparedStatementsResponse' {preparedStatements} -> preparedStatements) (\s@ListPreparedStatementsResponse' {} a -> s {preparedStatements = a} :: ListPreparedStatementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPreparedStatementsResponse_httpStatus :: Lens.Lens' ListPreparedStatementsResponse Prelude.Int
listPreparedStatementsResponse_httpStatus = Lens.lens (\ListPreparedStatementsResponse' {httpStatus} -> httpStatus) (\s@ListPreparedStatementsResponse' {} a -> s {httpStatus = a} :: ListPreparedStatementsResponse)

instance
  Prelude.NFData
    ListPreparedStatementsResponse
  where
  rnf ListPreparedStatementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf preparedStatements
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.ApplicationInsights.ListProblems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the problems with your application.
module Amazonka.ApplicationInsights.ListProblems
  ( -- * Creating a Request
    ListProblems (..),
    newListProblems,

    -- * Request Lenses
    listProblems_componentName,
    listProblems_endTime,
    listProblems_maxResults,
    listProblems_nextToken,
    listProblems_resourceGroupName,
    listProblems_startTime,

    -- * Destructuring the Response
    ListProblemsResponse (..),
    newListProblemsResponse,

    -- * Response Lenses
    listProblemsResponse_nextToken,
    listProblemsResponse_problemList,
    listProblemsResponse_resourceGroupName,
    listProblemsResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProblems' smart constructor.
data ListProblems = ListProblems'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The time when the problem ended, in epoch seconds. If not specified,
    -- problems within the past seven days are returned.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time when the problem was detected, in epoch seconds. If you don\'t
    -- specify a time frame for the request, problems within the past seven
    -- days are returned.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProblems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'listProblems_componentName' - The name of the component.
--
-- 'endTime', 'listProblems_endTime' - The time when the problem ended, in epoch seconds. If not specified,
-- problems within the past seven days are returned.
--
-- 'maxResults', 'listProblems_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'listProblems_nextToken' - The token to request the next page of results.
--
-- 'resourceGroupName', 'listProblems_resourceGroupName' - The name of the resource group.
--
-- 'startTime', 'listProblems_startTime' - The time when the problem was detected, in epoch seconds. If you don\'t
-- specify a time frame for the request, problems within the past seven
-- days are returned.
newListProblems ::
  ListProblems
newListProblems =
  ListProblems'
    { componentName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The name of the component.
listProblems_componentName :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Text)
listProblems_componentName = Lens.lens (\ListProblems' {componentName} -> componentName) (\s@ListProblems' {} a -> s {componentName = a} :: ListProblems)

-- | The time when the problem ended, in epoch seconds. If not specified,
-- problems within the past seven days are returned.
listProblems_endTime :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.UTCTime)
listProblems_endTime = Lens.lens (\ListProblems' {endTime} -> endTime) (\s@ListProblems' {} a -> s {endTime = a} :: ListProblems) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listProblems_maxResults :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Natural)
listProblems_maxResults = Lens.lens (\ListProblems' {maxResults} -> maxResults) (\s@ListProblems' {} a -> s {maxResults = a} :: ListProblems)

-- | The token to request the next page of results.
listProblems_nextToken :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Text)
listProblems_nextToken = Lens.lens (\ListProblems' {nextToken} -> nextToken) (\s@ListProblems' {} a -> s {nextToken = a} :: ListProblems)

-- | The name of the resource group.
listProblems_resourceGroupName :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Text)
listProblems_resourceGroupName = Lens.lens (\ListProblems' {resourceGroupName} -> resourceGroupName) (\s@ListProblems' {} a -> s {resourceGroupName = a} :: ListProblems)

-- | The time when the problem was detected, in epoch seconds. If you don\'t
-- specify a time frame for the request, problems within the past seven
-- days are returned.
listProblems_startTime :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.UTCTime)
listProblems_startTime = Lens.lens (\ListProblems' {startTime} -> startTime) (\s@ListProblems' {} a -> s {startTime = a} :: ListProblems) Prelude.. Lens.mapping Data._Time

instance Core.AWSRequest ListProblems where
  type AWSResponse ListProblems = ListProblemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProblemsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ProblemList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ResourceGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProblems where
  hashWithSalt _salt ListProblems' {..} =
    _salt
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListProblems where
  rnf ListProblems' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders ListProblems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.ListProblems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProblems where
  toJSON ListProblems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComponentName" Data..=) Prelude.<$> componentName,
            ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceGroupName" Data..=)
              Prelude.<$> resourceGroupName,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )

instance Data.ToPath ListProblems where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProblems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProblemsResponse' smart constructor.
data ListProblemsResponse = ListProblemsResponse'
  { -- | The token used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of problems.
    problemList :: Prelude.Maybe [Problem],
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProblemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProblemsResponse_nextToken' - The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'problemList', 'listProblemsResponse_problemList' - The list of problems.
--
-- 'resourceGroupName', 'listProblemsResponse_resourceGroupName' - The name of the resource group.
--
-- 'httpStatus', 'listProblemsResponse_httpStatus' - The response's http status code.
newListProblemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProblemsResponse
newListProblemsResponse pHttpStatus_ =
  ListProblemsResponse'
    { nextToken = Prelude.Nothing,
      problemList = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listProblemsResponse_nextToken :: Lens.Lens' ListProblemsResponse (Prelude.Maybe Prelude.Text)
listProblemsResponse_nextToken = Lens.lens (\ListProblemsResponse' {nextToken} -> nextToken) (\s@ListProblemsResponse' {} a -> s {nextToken = a} :: ListProblemsResponse)

-- | The list of problems.
listProblemsResponse_problemList :: Lens.Lens' ListProblemsResponse (Prelude.Maybe [Problem])
listProblemsResponse_problemList = Lens.lens (\ListProblemsResponse' {problemList} -> problemList) (\s@ListProblemsResponse' {} a -> s {problemList = a} :: ListProblemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the resource group.
listProblemsResponse_resourceGroupName :: Lens.Lens' ListProblemsResponse (Prelude.Maybe Prelude.Text)
listProblemsResponse_resourceGroupName = Lens.lens (\ListProblemsResponse' {resourceGroupName} -> resourceGroupName) (\s@ListProblemsResponse' {} a -> s {resourceGroupName = a} :: ListProblemsResponse)

-- | The response's http status code.
listProblemsResponse_httpStatus :: Lens.Lens' ListProblemsResponse Prelude.Int
listProblemsResponse_httpStatus = Lens.lens (\ListProblemsResponse' {httpStatus} -> httpStatus) (\s@ListProblemsResponse' {} a -> s {httpStatus = a} :: ListProblemsResponse)

instance Prelude.NFData ListProblemsResponse where
  rnf ListProblemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf problemList
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf httpStatus

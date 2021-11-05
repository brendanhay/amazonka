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
-- Module      : Network.AWS.ApplicationInsights.ListProblems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the problems with your application.
module Network.AWS.ApplicationInsights.ListProblems
  ( -- * Creating a Request
    ListProblems (..),
    newListProblems,

    -- * Request Lenses
    listProblems_resourceGroupName,
    listProblems_startTime,
    listProblems_nextToken,
    listProblems_endTime,
    listProblems_maxResults,

    -- * Destructuring the Response
    ListProblemsResponse (..),
    newListProblemsResponse,

    -- * Response Lenses
    listProblemsResponse_nextToken,
    listProblemsResponse_problemList,
    listProblemsResponse_httpStatus,
  )
where

import Network.AWS.ApplicationInsights.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProblems' smart constructor.
data ListProblems = ListProblems'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time when the problem was detected, in epoch seconds. If you don\'t
    -- specify a time frame for the request, problems within the past seven
    -- days are returned.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time when the problem ended, in epoch seconds. If not specified,
    -- problems within the past seven days are returned.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'resourceGroupName', 'listProblems_resourceGroupName' - The name of the resource group.
--
-- 'startTime', 'listProblems_startTime' - The time when the problem was detected, in epoch seconds. If you don\'t
-- specify a time frame for the request, problems within the past seven
-- days are returned.
--
-- 'nextToken', 'listProblems_nextToken' - The token to request the next page of results.
--
-- 'endTime', 'listProblems_endTime' - The time when the problem ended, in epoch seconds. If not specified,
-- problems within the past seven days are returned.
--
-- 'maxResults', 'listProblems_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
newListProblems ::
  ListProblems
newListProblems =
  ListProblems'
    { resourceGroupName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name of the resource group.
listProblems_resourceGroupName :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Text)
listProblems_resourceGroupName = Lens.lens (\ListProblems' {resourceGroupName} -> resourceGroupName) (\s@ListProblems' {} a -> s {resourceGroupName = a} :: ListProblems)

-- | The time when the problem was detected, in epoch seconds. If you don\'t
-- specify a time frame for the request, problems within the past seven
-- days are returned.
listProblems_startTime :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.UTCTime)
listProblems_startTime = Lens.lens (\ListProblems' {startTime} -> startTime) (\s@ListProblems' {} a -> s {startTime = a} :: ListProblems) Prelude.. Lens.mapping Core._Time

-- | The token to request the next page of results.
listProblems_nextToken :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Text)
listProblems_nextToken = Lens.lens (\ListProblems' {nextToken} -> nextToken) (\s@ListProblems' {} a -> s {nextToken = a} :: ListProblems)

-- | The time when the problem ended, in epoch seconds. If not specified,
-- problems within the past seven days are returned.
listProblems_endTime :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.UTCTime)
listProblems_endTime = Lens.lens (\ListProblems' {endTime} -> endTime) (\s@ListProblems' {} a -> s {endTime = a} :: ListProblems) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listProblems_maxResults :: Lens.Lens' ListProblems (Prelude.Maybe Prelude.Natural)
listProblems_maxResults = Lens.lens (\ListProblems' {maxResults} -> maxResults) (\s@ListProblems' {} a -> s {maxResults = a} :: ListProblems)

instance Core.AWSRequest ListProblems where
  type AWSResponse ListProblems = ListProblemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProblemsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ProblemList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProblems

instance Prelude.NFData ListProblems

instance Core.ToHeaders ListProblems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "EC2WindowsBarleyService.ListProblems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProblems where
  toJSON ListProblems' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceGroupName" Core..=)
              Prelude.<$> resourceGroupName,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListProblems where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProblems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProblemsResponse' smart constructor.
data ListProblemsResponse = ListProblemsResponse'
  { -- | The token used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of problems.
    problemList :: Prelude.Maybe [Problem],
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
-- 'httpStatus', 'listProblemsResponse_httpStatus' - The response's http status code.
newListProblemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProblemsResponse
newListProblemsResponse pHttpStatus_ =
  ListProblemsResponse'
    { nextToken = Prelude.Nothing,
      problemList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listProblemsResponse_nextToken :: Lens.Lens' ListProblemsResponse (Prelude.Maybe Prelude.Text)
listProblemsResponse_nextToken = Lens.lens (\ListProblemsResponse' {nextToken} -> nextToken) (\s@ListProblemsResponse' {} a -> s {nextToken = a} :: ListProblemsResponse)

-- | The list of problems.
listProblemsResponse_problemList :: Lens.Lens' ListProblemsResponse (Prelude.Maybe [Problem])
listProblemsResponse_problemList = Lens.lens (\ListProblemsResponse' {problemList} -> problemList) (\s@ListProblemsResponse' {} a -> s {problemList = a} :: ListProblemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProblemsResponse_httpStatus :: Lens.Lens' ListProblemsResponse Prelude.Int
listProblemsResponse_httpStatus = Lens.lens (\ListProblemsResponse' {httpStatus} -> httpStatus) (\s@ListProblemsResponse' {} a -> s {httpStatus = a} :: ListProblemsResponse)

instance Prelude.NFData ListProblemsResponse

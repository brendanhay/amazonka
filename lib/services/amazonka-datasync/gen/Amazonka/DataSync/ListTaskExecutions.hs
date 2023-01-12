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
-- Module      : Amazonka.DataSync.ListTaskExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of executed tasks.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListTaskExecutions
  ( -- * Creating a Request
    ListTaskExecutions (..),
    newListTaskExecutions,

    -- * Request Lenses
    listTaskExecutions_maxResults,
    listTaskExecutions_nextToken,
    listTaskExecutions_taskArn,

    -- * Destructuring the Response
    ListTaskExecutionsResponse (..),
    newListTaskExecutionsResponse,

    -- * Response Lenses
    listTaskExecutionsResponse_nextToken,
    listTaskExecutionsResponse_taskExecutions,
    listTaskExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListTaskExecutions
--
-- /See:/ 'newListTaskExecutions' smart constructor.
data ListTaskExecutions = ListTaskExecutions'
  { -- | The maximum number of executed tasks to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque string that indicates the position at which to begin the next
    -- list of the executed tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task whose tasks you want to list.
    taskArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTaskExecutions_maxResults' - The maximum number of executed tasks to list.
--
-- 'nextToken', 'listTaskExecutions_nextToken' - An opaque string that indicates the position at which to begin the next
-- list of the executed tasks.
--
-- 'taskArn', 'listTaskExecutions_taskArn' - The Amazon Resource Name (ARN) of the task whose tasks you want to list.
newListTaskExecutions ::
  ListTaskExecutions
newListTaskExecutions =
  ListTaskExecutions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      taskArn = Prelude.Nothing
    }

-- | The maximum number of executed tasks to list.
listTaskExecutions_maxResults :: Lens.Lens' ListTaskExecutions (Prelude.Maybe Prelude.Natural)
listTaskExecutions_maxResults = Lens.lens (\ListTaskExecutions' {maxResults} -> maxResults) (\s@ListTaskExecutions' {} a -> s {maxResults = a} :: ListTaskExecutions)

-- | An opaque string that indicates the position at which to begin the next
-- list of the executed tasks.
listTaskExecutions_nextToken :: Lens.Lens' ListTaskExecutions (Prelude.Maybe Prelude.Text)
listTaskExecutions_nextToken = Lens.lens (\ListTaskExecutions' {nextToken} -> nextToken) (\s@ListTaskExecutions' {} a -> s {nextToken = a} :: ListTaskExecutions)

-- | The Amazon Resource Name (ARN) of the task whose tasks you want to list.
listTaskExecutions_taskArn :: Lens.Lens' ListTaskExecutions (Prelude.Maybe Prelude.Text)
listTaskExecutions_taskArn = Lens.lens (\ListTaskExecutions' {taskArn} -> taskArn) (\s@ListTaskExecutions' {} a -> s {taskArn = a} :: ListTaskExecutions)

instance Core.AWSPager ListTaskExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTaskExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTaskExecutionsResponse_taskExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTaskExecutions_nextToken
          Lens..~ rs
          Lens.^? listTaskExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTaskExecutions where
  type
    AWSResponse ListTaskExecutions =
      ListTaskExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskExecutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TaskExecutions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTaskExecutions where
  hashWithSalt _salt ListTaskExecutions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` taskArn

instance Prelude.NFData ListTaskExecutions where
  rnf ListTaskExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskArn

instance Data.ToHeaders ListTaskExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.ListTaskExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTaskExecutions where
  toJSON ListTaskExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TaskArn" Data..=) Prelude.<$> taskArn
          ]
      )

instance Data.ToPath ListTaskExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTaskExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | ListTaskExecutionsResponse
--
-- /See:/ 'newListTaskExecutionsResponse' smart constructor.
data ListTaskExecutionsResponse = ListTaskExecutionsResponse'
  { -- | An opaque string that indicates the position at which to begin returning
    -- the next list of executed tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of executed tasks.
    taskExecutions :: Prelude.Maybe [TaskExecutionListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTaskExecutionsResponse_nextToken' - An opaque string that indicates the position at which to begin returning
-- the next list of executed tasks.
--
-- 'taskExecutions', 'listTaskExecutionsResponse_taskExecutions' - A list of executed tasks.
--
-- 'httpStatus', 'listTaskExecutionsResponse_httpStatus' - The response's http status code.
newListTaskExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTaskExecutionsResponse
newListTaskExecutionsResponse pHttpStatus_ =
  ListTaskExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      taskExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string that indicates the position at which to begin returning
-- the next list of executed tasks.
listTaskExecutionsResponse_nextToken :: Lens.Lens' ListTaskExecutionsResponse (Prelude.Maybe Prelude.Text)
listTaskExecutionsResponse_nextToken = Lens.lens (\ListTaskExecutionsResponse' {nextToken} -> nextToken) (\s@ListTaskExecutionsResponse' {} a -> s {nextToken = a} :: ListTaskExecutionsResponse)

-- | A list of executed tasks.
listTaskExecutionsResponse_taskExecutions :: Lens.Lens' ListTaskExecutionsResponse (Prelude.Maybe [TaskExecutionListEntry])
listTaskExecutionsResponse_taskExecutions = Lens.lens (\ListTaskExecutionsResponse' {taskExecutions} -> taskExecutions) (\s@ListTaskExecutionsResponse' {} a -> s {taskExecutions = a} :: ListTaskExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTaskExecutionsResponse_httpStatus :: Lens.Lens' ListTaskExecutionsResponse Prelude.Int
listTaskExecutionsResponse_httpStatus = Lens.lens (\ListTaskExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListTaskExecutionsResponse' {} a -> s {httpStatus = a} :: ListTaskExecutionsResponse)

instance Prelude.NFData ListTaskExecutionsResponse where
  rnf ListTaskExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskExecutions
      `Prelude.seq` Prelude.rnf httpStatus

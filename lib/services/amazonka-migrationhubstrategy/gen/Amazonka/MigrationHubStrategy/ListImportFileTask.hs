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
-- Module      : Amazonka.MigrationHubStrategy.ListImportFileTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all the imports performed.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubStrategy.ListImportFileTask
  ( -- * Creating a Request
    ListImportFileTask (..),
    newListImportFileTask,

    -- * Request Lenses
    listImportFileTask_maxResults,
    listImportFileTask_nextToken,

    -- * Destructuring the Response
    ListImportFileTaskResponse (..),
    newListImportFileTaskResponse,

    -- * Response Lenses
    listImportFileTaskResponse_nextToken,
    listImportFileTaskResponse_taskInfos,
    listImportFileTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImportFileTask' smart constructor.
data ListImportFileTask = ListImportFileTask'
  { -- | The total number of items to return. The maximum value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token from a previous call that you use to retrieve the next set of
    -- results. For example, if a previous call to this action returned 100
    -- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
    -- results along with a token. You then use the returned token to retrieve
    -- the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportFileTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listImportFileTask_maxResults' - The total number of items to return. The maximum value is 100.
--
-- 'nextToken', 'listImportFileTask_nextToken' - The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
newListImportFileTask ::
  ListImportFileTask
newListImportFileTask =
  ListImportFileTask'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The total number of items to return. The maximum value is 100.
listImportFileTask_maxResults :: Lens.Lens' ListImportFileTask (Prelude.Maybe Prelude.Int)
listImportFileTask_maxResults = Lens.lens (\ListImportFileTask' {maxResults} -> maxResults) (\s@ListImportFileTask' {} a -> s {maxResults = a} :: ListImportFileTask)

-- | The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
listImportFileTask_nextToken :: Lens.Lens' ListImportFileTask (Prelude.Maybe Prelude.Text)
listImportFileTask_nextToken = Lens.lens (\ListImportFileTask' {nextToken} -> nextToken) (\s@ListImportFileTask' {} a -> s {nextToken = a} :: ListImportFileTask)

instance Core.AWSPager ListImportFileTask where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImportFileTaskResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImportFileTaskResponse_taskInfos
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listImportFileTask_nextToken
          Lens..~ rs
          Lens.^? listImportFileTaskResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListImportFileTask where
  type
    AWSResponse ListImportFileTask =
      ListImportFileTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportFileTaskResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "taskInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImportFileTask where
  hashWithSalt _salt ListImportFileTask' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListImportFileTask where
  rnf ListImportFileTask' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListImportFileTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListImportFileTask where
  toPath = Prelude.const "/list-import-file-task"

instance Data.ToQuery ListImportFileTask where
  toQuery ListImportFileTask' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListImportFileTaskResponse' smart constructor.
data ListImportFileTaskResponse = ListImportFileTaskResponse'
  { -- | The token you use to retrieve the next set of results, or null if there
    -- are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists information about the files you import.
    taskInfos :: Prelude.Maybe [ImportFileTaskInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportFileTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImportFileTaskResponse_nextToken' - The token you use to retrieve the next set of results, or null if there
-- are no more results.
--
-- 'taskInfos', 'listImportFileTaskResponse_taskInfos' - Lists information about the files you import.
--
-- 'httpStatus', 'listImportFileTaskResponse_httpStatus' - The response's http status code.
newListImportFileTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportFileTaskResponse
newListImportFileTaskResponse pHttpStatus_ =
  ListImportFileTaskResponse'
    { nextToken =
        Prelude.Nothing,
      taskInfos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token you use to retrieve the next set of results, or null if there
-- are no more results.
listImportFileTaskResponse_nextToken :: Lens.Lens' ListImportFileTaskResponse (Prelude.Maybe Prelude.Text)
listImportFileTaskResponse_nextToken = Lens.lens (\ListImportFileTaskResponse' {nextToken} -> nextToken) (\s@ListImportFileTaskResponse' {} a -> s {nextToken = a} :: ListImportFileTaskResponse)

-- | Lists information about the files you import.
listImportFileTaskResponse_taskInfos :: Lens.Lens' ListImportFileTaskResponse (Prelude.Maybe [ImportFileTaskInformation])
listImportFileTaskResponse_taskInfos = Lens.lens (\ListImportFileTaskResponse' {taskInfos} -> taskInfos) (\s@ListImportFileTaskResponse' {} a -> s {taskInfos = a} :: ListImportFileTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImportFileTaskResponse_httpStatus :: Lens.Lens' ListImportFileTaskResponse Prelude.Int
listImportFileTaskResponse_httpStatus = Lens.lens (\ListImportFileTaskResponse' {httpStatus} -> httpStatus) (\s@ListImportFileTaskResponse' {} a -> s {httpStatus = a} :: ListImportFileTaskResponse)

instance Prelude.NFData ListImportFileTaskResponse where
  rnf ListImportFileTaskResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskInfos
      `Prelude.seq` Prelude.rnf httpStatus

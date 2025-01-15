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
-- Module      : Amazonka.M2.ListDataSetImportHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data set imports for the specified application.
--
-- This operation returns paginated results.
module Amazonka.M2.ListDataSetImportHistory
  ( -- * Creating a Request
    ListDataSetImportHistory (..),
    newListDataSetImportHistory,

    -- * Request Lenses
    listDataSetImportHistory_maxResults,
    listDataSetImportHistory_nextToken,
    listDataSetImportHistory_applicationId,

    -- * Destructuring the Response
    ListDataSetImportHistoryResponse (..),
    newListDataSetImportHistoryResponse,

    -- * Response Lenses
    listDataSetImportHistoryResponse_nextToken,
    listDataSetImportHistoryResponse_httpStatus,
    listDataSetImportHistoryResponse_dataSetImportTasks,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSetImportHistory' smart constructor.
data ListDataSetImportHistory = ListDataSetImportHistory'
  { -- | The maximum number of objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token returned from a previous call to this operation. This
    -- specifies the next item to return. To return to the beginning of the
    -- list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetImportHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataSetImportHistory_maxResults' - The maximum number of objects to return.
--
-- 'nextToken', 'listDataSetImportHistory_nextToken' - A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
--
-- 'applicationId', 'listDataSetImportHistory_applicationId' - The unique identifier of the application.
newListDataSetImportHistory ::
  -- | 'applicationId'
  Prelude.Text ->
  ListDataSetImportHistory
newListDataSetImportHistory pApplicationId_ =
  ListDataSetImportHistory'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of objects to return.
listDataSetImportHistory_maxResults :: Lens.Lens' ListDataSetImportHistory (Prelude.Maybe Prelude.Natural)
listDataSetImportHistory_maxResults = Lens.lens (\ListDataSetImportHistory' {maxResults} -> maxResults) (\s@ListDataSetImportHistory' {} a -> s {maxResults = a} :: ListDataSetImportHistory)

-- | A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
listDataSetImportHistory_nextToken :: Lens.Lens' ListDataSetImportHistory (Prelude.Maybe Prelude.Text)
listDataSetImportHistory_nextToken = Lens.lens (\ListDataSetImportHistory' {nextToken} -> nextToken) (\s@ListDataSetImportHistory' {} a -> s {nextToken = a} :: ListDataSetImportHistory)

-- | The unique identifier of the application.
listDataSetImportHistory_applicationId :: Lens.Lens' ListDataSetImportHistory Prelude.Text
listDataSetImportHistory_applicationId = Lens.lens (\ListDataSetImportHistory' {applicationId} -> applicationId) (\s@ListDataSetImportHistory' {} a -> s {applicationId = a} :: ListDataSetImportHistory)

instance Core.AWSPager ListDataSetImportHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSetImportHistoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDataSetImportHistoryResponse_dataSetImportTasks
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDataSetImportHistory_nextToken
              Lens..~ rs
              Lens.^? listDataSetImportHistoryResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDataSetImportHistory where
  type
    AWSResponse ListDataSetImportHistory =
      ListDataSetImportHistoryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSetImportHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "dataSetImportTasks"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDataSetImportHistory where
  hashWithSalt _salt ListDataSetImportHistory' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListDataSetImportHistory where
  rnf ListDataSetImportHistory' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf applicationId

instance Data.ToHeaders ListDataSetImportHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataSetImportHistory where
  toPath ListDataSetImportHistory' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/dataset-import-tasks"
      ]

instance Data.ToQuery ListDataSetImportHistory where
  toQuery ListDataSetImportHistory' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDataSetImportHistoryResponse' smart constructor.
data ListDataSetImportHistoryResponse = ListDataSetImportHistoryResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to this operation to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The data set import tasks.
    dataSetImportTasks :: [DataSetImportTask]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetImportHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSetImportHistoryResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listDataSetImportHistoryResponse_httpStatus' - The response's http status code.
--
-- 'dataSetImportTasks', 'listDataSetImportHistoryResponse_dataSetImportTasks' - The data set import tasks.
newListDataSetImportHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataSetImportHistoryResponse
newListDataSetImportHistoryResponse pHttpStatus_ =
  ListDataSetImportHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      dataSetImportTasks = Prelude.mempty
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
listDataSetImportHistoryResponse_nextToken :: Lens.Lens' ListDataSetImportHistoryResponse (Prelude.Maybe Prelude.Text)
listDataSetImportHistoryResponse_nextToken = Lens.lens (\ListDataSetImportHistoryResponse' {nextToken} -> nextToken) (\s@ListDataSetImportHistoryResponse' {} a -> s {nextToken = a} :: ListDataSetImportHistoryResponse)

-- | The response's http status code.
listDataSetImportHistoryResponse_httpStatus :: Lens.Lens' ListDataSetImportHistoryResponse Prelude.Int
listDataSetImportHistoryResponse_httpStatus = Lens.lens (\ListDataSetImportHistoryResponse' {httpStatus} -> httpStatus) (\s@ListDataSetImportHistoryResponse' {} a -> s {httpStatus = a} :: ListDataSetImportHistoryResponse)

-- | The data set import tasks.
listDataSetImportHistoryResponse_dataSetImportTasks :: Lens.Lens' ListDataSetImportHistoryResponse [DataSetImportTask]
listDataSetImportHistoryResponse_dataSetImportTasks = Lens.lens (\ListDataSetImportHistoryResponse' {dataSetImportTasks} -> dataSetImportTasks) (\s@ListDataSetImportHistoryResponse' {} a -> s {dataSetImportTasks = a} :: ListDataSetImportHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDataSetImportHistoryResponse
  where
  rnf ListDataSetImportHistoryResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf dataSetImportTasks

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
-- Module      : Amazonka.Translate.ListParallelData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of your parallel data resources in Amazon Translate.
module Amazonka.Translate.ListParallelData
  ( -- * Creating a Request
    ListParallelData (..),
    newListParallelData,

    -- * Request Lenses
    listParallelData_nextToken,
    listParallelData_maxResults,

    -- * Destructuring the Response
    ListParallelDataResponse (..),
    newListParallelDataResponse,

    -- * Response Lenses
    listParallelDataResponse_nextToken,
    listParallelDataResponse_parallelDataPropertiesList,
    listParallelDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newListParallelData' smart constructor.
data ListParallelData = ListParallelData'
  { -- | A string that specifies the next page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of parallel data resources returned for each request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listParallelData_nextToken' - A string that specifies the next page of results to return in a
-- paginated response.
--
-- 'maxResults', 'listParallelData_maxResults' - The maximum number of parallel data resources returned for each request.
newListParallelData ::
  ListParallelData
newListParallelData =
  ListParallelData'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A string that specifies the next page of results to return in a
-- paginated response.
listParallelData_nextToken :: Lens.Lens' ListParallelData (Prelude.Maybe Prelude.Text)
listParallelData_nextToken = Lens.lens (\ListParallelData' {nextToken} -> nextToken) (\s@ListParallelData' {} a -> s {nextToken = a} :: ListParallelData)

-- | The maximum number of parallel data resources returned for each request.
listParallelData_maxResults :: Lens.Lens' ListParallelData (Prelude.Maybe Prelude.Natural)
listParallelData_maxResults = Lens.lens (\ListParallelData' {maxResults} -> maxResults) (\s@ListParallelData' {} a -> s {maxResults = a} :: ListParallelData)

instance Core.AWSRequest ListParallelData where
  type
    AWSResponse ListParallelData =
      ListParallelDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListParallelDataResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ParallelDataPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListParallelData where
  hashWithSalt _salt ListParallelData' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListParallelData where
  rnf ListParallelData' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListParallelData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ListParallelData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListParallelData where
  toJSON ListParallelData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListParallelData where
  toPath = Prelude.const "/"

instance Core.ToQuery ListParallelData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListParallelDataResponse' smart constructor.
data ListParallelDataResponse = ListParallelDataResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The properties of the parallel data resources returned by this request.
    parallelDataPropertiesList :: Prelude.Maybe [ParallelDataProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listParallelDataResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'parallelDataPropertiesList', 'listParallelDataResponse_parallelDataPropertiesList' - The properties of the parallel data resources returned by this request.
--
-- 'httpStatus', 'listParallelDataResponse_httpStatus' - The response's http status code.
newListParallelDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListParallelDataResponse
newListParallelDataResponse pHttpStatus_ =
  ListParallelDataResponse'
    { nextToken =
        Prelude.Nothing,
      parallelDataPropertiesList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listParallelDataResponse_nextToken :: Lens.Lens' ListParallelDataResponse (Prelude.Maybe Prelude.Text)
listParallelDataResponse_nextToken = Lens.lens (\ListParallelDataResponse' {nextToken} -> nextToken) (\s@ListParallelDataResponse' {} a -> s {nextToken = a} :: ListParallelDataResponse)

-- | The properties of the parallel data resources returned by this request.
listParallelDataResponse_parallelDataPropertiesList :: Lens.Lens' ListParallelDataResponse (Prelude.Maybe [ParallelDataProperties])
listParallelDataResponse_parallelDataPropertiesList = Lens.lens (\ListParallelDataResponse' {parallelDataPropertiesList} -> parallelDataPropertiesList) (\s@ListParallelDataResponse' {} a -> s {parallelDataPropertiesList = a} :: ListParallelDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listParallelDataResponse_httpStatus :: Lens.Lens' ListParallelDataResponse Prelude.Int
listParallelDataResponse_httpStatus = Lens.lens (\ListParallelDataResponse' {httpStatus} -> httpStatus) (\s@ListParallelDataResponse' {} a -> s {httpStatus = a} :: ListParallelDataResponse)

instance Prelude.NFData ListParallelDataResponse where
  rnf ListParallelDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parallelDataPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus

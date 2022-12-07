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
-- Module      : Amazonka.ComprehendMedical.ListSNOMEDCTInferenceJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of InferSNOMEDCT jobs a user has submitted.
module Amazonka.ComprehendMedical.ListSNOMEDCTInferenceJobs
  ( -- * Creating a Request
    ListSNOMEDCTInferenceJobs (..),
    newListSNOMEDCTInferenceJobs,

    -- * Request Lenses
    listSNOMEDCTInferenceJobs_nextToken,
    listSNOMEDCTInferenceJobs_filter,
    listSNOMEDCTInferenceJobs_maxResults,

    -- * Destructuring the Response
    ListSNOMEDCTInferenceJobsResponse (..),
    newListSNOMEDCTInferenceJobsResponse,

    -- * Response Lenses
    listSNOMEDCTInferenceJobsResponse_nextToken,
    listSNOMEDCTInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listSNOMEDCTInferenceJobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSNOMEDCTInferenceJobs' smart constructor.
data ListSNOMEDCTInferenceJobs = ListSNOMEDCTInferenceJobs'
  { -- | Identifies the next page of InferSNOMEDCT results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSNOMEDCTInferenceJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSNOMEDCTInferenceJobs_nextToken' - Identifies the next page of InferSNOMEDCT results to return.
--
-- 'filter'', 'listSNOMEDCTInferenceJobs_filter' - Undocumented member.
--
-- 'maxResults', 'listSNOMEDCTInferenceJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListSNOMEDCTInferenceJobs ::
  ListSNOMEDCTInferenceJobs
newListSNOMEDCTInferenceJobs =
  ListSNOMEDCTInferenceJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of InferSNOMEDCT results to return.
listSNOMEDCTInferenceJobs_nextToken :: Lens.Lens' ListSNOMEDCTInferenceJobs (Prelude.Maybe Prelude.Text)
listSNOMEDCTInferenceJobs_nextToken = Lens.lens (\ListSNOMEDCTInferenceJobs' {nextToken} -> nextToken) (\s@ListSNOMEDCTInferenceJobs' {} a -> s {nextToken = a} :: ListSNOMEDCTInferenceJobs)

-- | Undocumented member.
listSNOMEDCTInferenceJobs_filter :: Lens.Lens' ListSNOMEDCTInferenceJobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listSNOMEDCTInferenceJobs_filter = Lens.lens (\ListSNOMEDCTInferenceJobs' {filter'} -> filter') (\s@ListSNOMEDCTInferenceJobs' {} a -> s {filter' = a} :: ListSNOMEDCTInferenceJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listSNOMEDCTInferenceJobs_maxResults :: Lens.Lens' ListSNOMEDCTInferenceJobs (Prelude.Maybe Prelude.Natural)
listSNOMEDCTInferenceJobs_maxResults = Lens.lens (\ListSNOMEDCTInferenceJobs' {maxResults} -> maxResults) (\s@ListSNOMEDCTInferenceJobs' {} a -> s {maxResults = a} :: ListSNOMEDCTInferenceJobs)

instance Core.AWSRequest ListSNOMEDCTInferenceJobs where
  type
    AWSResponse ListSNOMEDCTInferenceJobs =
      ListSNOMEDCTInferenceJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSNOMEDCTInferenceJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSNOMEDCTInferenceJobs where
  hashWithSalt _salt ListSNOMEDCTInferenceJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSNOMEDCTInferenceJobs where
  rnf ListSNOMEDCTInferenceJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListSNOMEDCTInferenceJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.ListSNOMEDCTInferenceJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSNOMEDCTInferenceJobs where
  toJSON ListSNOMEDCTInferenceJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListSNOMEDCTInferenceJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSNOMEDCTInferenceJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSNOMEDCTInferenceJobsResponse' smart constructor.
data ListSNOMEDCTInferenceJobsResponse = ListSNOMEDCTInferenceJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSNOMEDCTInferenceJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSNOMEDCTInferenceJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'comprehendMedicalAsyncJobPropertiesList', 'listSNOMEDCTInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listSNOMEDCTInferenceJobsResponse_httpStatus' - The response's http status code.
newListSNOMEDCTInferenceJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSNOMEDCTInferenceJobsResponse
newListSNOMEDCTInferenceJobsResponse pHttpStatus_ =
  ListSNOMEDCTInferenceJobsResponse'
    { nextToken =
        Prelude.Nothing,
      comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listSNOMEDCTInferenceJobsResponse_nextToken :: Lens.Lens' ListSNOMEDCTInferenceJobsResponse (Prelude.Maybe Prelude.Text)
listSNOMEDCTInferenceJobsResponse_nextToken = Lens.lens (\ListSNOMEDCTInferenceJobsResponse' {nextToken} -> nextToken) (\s@ListSNOMEDCTInferenceJobsResponse' {} a -> s {nextToken = a} :: ListSNOMEDCTInferenceJobsResponse)

-- | A list containing the properties of each job that is returned.
listSNOMEDCTInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListSNOMEDCTInferenceJobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listSNOMEDCTInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListSNOMEDCTInferenceJobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListSNOMEDCTInferenceJobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListSNOMEDCTInferenceJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSNOMEDCTInferenceJobsResponse_httpStatus :: Lens.Lens' ListSNOMEDCTInferenceJobsResponse Prelude.Int
listSNOMEDCTInferenceJobsResponse_httpStatus = Lens.lens (\ListSNOMEDCTInferenceJobsResponse' {httpStatus} -> httpStatus) (\s@ListSNOMEDCTInferenceJobsResponse' {} a -> s {httpStatus = a} :: ListSNOMEDCTInferenceJobsResponse)

instance
  Prelude.NFData
    ListSNOMEDCTInferenceJobsResponse
  where
  rnf ListSNOMEDCTInferenceJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.ComprehendMedical.ListEntitiesDetectionV2Jobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of medical entity detection jobs that you have submitted.
module Amazonka.ComprehendMedical.ListEntitiesDetectionV2Jobs
  ( -- * Creating a Request
    ListEntitiesDetectionV2Jobs (..),
    newListEntitiesDetectionV2Jobs,

    -- * Request Lenses
    listEntitiesDetectionV2Jobs_nextToken,
    listEntitiesDetectionV2Jobs_filter,
    listEntitiesDetectionV2Jobs_maxResults,

    -- * Destructuring the Response
    ListEntitiesDetectionV2JobsResponse (..),
    newListEntitiesDetectionV2JobsResponse,

    -- * Response Lenses
    listEntitiesDetectionV2JobsResponse_nextToken,
    listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listEntitiesDetectionV2JobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitiesDetectionV2Jobs' smart constructor.
data ListEntitiesDetectionV2Jobs = ListEntitiesDetectionV2Jobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs based on their
    -- names, status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesDetectionV2Jobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntitiesDetectionV2Jobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listEntitiesDetectionV2Jobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listEntitiesDetectionV2Jobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListEntitiesDetectionV2Jobs ::
  ListEntitiesDetectionV2Jobs
newListEntitiesDetectionV2Jobs =
  ListEntitiesDetectionV2Jobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listEntitiesDetectionV2Jobs_nextToken :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe Prelude.Text)
listEntitiesDetectionV2Jobs_nextToken = Lens.lens (\ListEntitiesDetectionV2Jobs' {nextToken} -> nextToken) (\s@ListEntitiesDetectionV2Jobs' {} a -> s {nextToken = a} :: ListEntitiesDetectionV2Jobs)

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listEntitiesDetectionV2Jobs_filter :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listEntitiesDetectionV2Jobs_filter = Lens.lens (\ListEntitiesDetectionV2Jobs' {filter'} -> filter') (\s@ListEntitiesDetectionV2Jobs' {} a -> s {filter' = a} :: ListEntitiesDetectionV2Jobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEntitiesDetectionV2Jobs_maxResults :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe Prelude.Natural)
listEntitiesDetectionV2Jobs_maxResults = Lens.lens (\ListEntitiesDetectionV2Jobs' {maxResults} -> maxResults) (\s@ListEntitiesDetectionV2Jobs' {} a -> s {maxResults = a} :: ListEntitiesDetectionV2Jobs)

instance Core.AWSRequest ListEntitiesDetectionV2Jobs where
  type
    AWSResponse ListEntitiesDetectionV2Jobs =
      ListEntitiesDetectionV2JobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesDetectionV2JobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitiesDetectionV2Jobs where
  hashWithSalt _salt ListEntitiesDetectionV2Jobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListEntitiesDetectionV2Jobs where
  rnf ListEntitiesDetectionV2Jobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListEntitiesDetectionV2Jobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.ListEntitiesDetectionV2Jobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEntitiesDetectionV2Jobs where
  toJSON ListEntitiesDetectionV2Jobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListEntitiesDetectionV2Jobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEntitiesDetectionV2Jobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesDetectionV2JobsResponse' smart constructor.
data ListEntitiesDetectionV2JobsResponse = ListEntitiesDetectionV2JobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesDetectionV2JobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntitiesDetectionV2JobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'comprehendMedicalAsyncJobPropertiesList', 'listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job returned.
--
-- 'httpStatus', 'listEntitiesDetectionV2JobsResponse_httpStatus' - The response's http status code.
newListEntitiesDetectionV2JobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesDetectionV2JobsResponse
newListEntitiesDetectionV2JobsResponse pHttpStatus_ =
  ListEntitiesDetectionV2JobsResponse'
    { nextToken =
        Prelude.Nothing,
      comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listEntitiesDetectionV2JobsResponse_nextToken :: Lens.Lens' ListEntitiesDetectionV2JobsResponse (Prelude.Maybe Prelude.Text)
listEntitiesDetectionV2JobsResponse_nextToken = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {nextToken} -> nextToken) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {nextToken = a} :: ListEntitiesDetectionV2JobsResponse)

-- | A list containing the properties of each job returned.
listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListEntitiesDetectionV2JobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListEntitiesDetectionV2JobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEntitiesDetectionV2JobsResponse_httpStatus :: Lens.Lens' ListEntitiesDetectionV2JobsResponse Prelude.Int
listEntitiesDetectionV2JobsResponse_httpStatus = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {httpStatus = a} :: ListEntitiesDetectionV2JobsResponse)

instance
  Prelude.NFData
    ListEntitiesDetectionV2JobsResponse
  where
  rnf ListEntitiesDetectionV2JobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus

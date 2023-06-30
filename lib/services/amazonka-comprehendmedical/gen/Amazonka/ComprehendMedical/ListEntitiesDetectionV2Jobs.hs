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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listEntitiesDetectionV2Jobs_filter,
    listEntitiesDetectionV2Jobs_maxResults,
    listEntitiesDetectionV2Jobs_nextToken,

    -- * Destructuring the Response
    ListEntitiesDetectionV2JobsResponse (..),
    newListEntitiesDetectionV2JobsResponse,

    -- * Response Lenses
    listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listEntitiesDetectionV2JobsResponse_nextToken,
    listEntitiesDetectionV2JobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitiesDetectionV2Jobs' smart constructor.
data ListEntitiesDetectionV2Jobs = ListEntitiesDetectionV2Jobs'
  { -- | Filters the jobs that are returned. You can filter jobs based on their
    -- names, status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'listEntitiesDetectionV2Jobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listEntitiesDetectionV2Jobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listEntitiesDetectionV2Jobs_nextToken' - Identifies the next page of results to return.
newListEntitiesDetectionV2Jobs ::
  ListEntitiesDetectionV2Jobs
newListEntitiesDetectionV2Jobs =
  ListEntitiesDetectionV2Jobs'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listEntitiesDetectionV2Jobs_filter :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listEntitiesDetectionV2Jobs_filter = Lens.lens (\ListEntitiesDetectionV2Jobs' {filter'} -> filter') (\s@ListEntitiesDetectionV2Jobs' {} a -> s {filter' = a} :: ListEntitiesDetectionV2Jobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEntitiesDetectionV2Jobs_maxResults :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe Prelude.Natural)
listEntitiesDetectionV2Jobs_maxResults = Lens.lens (\ListEntitiesDetectionV2Jobs' {maxResults} -> maxResults) (\s@ListEntitiesDetectionV2Jobs' {} a -> s {maxResults = a} :: ListEntitiesDetectionV2Jobs)

-- | Identifies the next page of results to return.
listEntitiesDetectionV2Jobs_nextToken :: Lens.Lens' ListEntitiesDetectionV2Jobs (Prelude.Maybe Prelude.Text)
listEntitiesDetectionV2Jobs_nextToken = Lens.lens (\ListEntitiesDetectionV2Jobs' {nextToken} -> nextToken) (\s@ListEntitiesDetectionV2Jobs' {} a -> s {nextToken = a} :: ListEntitiesDetectionV2Jobs)

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
            Prelude.<$> ( x
                            Data..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitiesDetectionV2Jobs where
  hashWithSalt _salt ListEntitiesDetectionV2Jobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEntitiesDetectionV2Jobs where
  rnf ListEntitiesDetectionV2Jobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEntitiesDetectionV2Jobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.ListEntitiesDetectionV2Jobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntitiesDetectionV2Jobs where
  toJSON ListEntitiesDetectionV2Jobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEntitiesDetectionV2Jobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntitiesDetectionV2Jobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesDetectionV2JobsResponse' smart constructor.
data ListEntitiesDetectionV2JobsResponse = ListEntitiesDetectionV2JobsResponse'
  { -- | A list containing the properties of each job returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'comprehendMedicalAsyncJobPropertiesList', 'listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job returned.
--
-- 'nextToken', 'listEntitiesDetectionV2JobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listEntitiesDetectionV2JobsResponse_httpStatus' - The response's http status code.
newListEntitiesDetectionV2JobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesDetectionV2JobsResponse
newListEntitiesDetectionV2JobsResponse pHttpStatus_ =
  ListEntitiesDetectionV2JobsResponse'
    { comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job returned.
listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListEntitiesDetectionV2JobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListEntitiesDetectionV2JobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listEntitiesDetectionV2JobsResponse_nextToken :: Lens.Lens' ListEntitiesDetectionV2JobsResponse (Prelude.Maybe Prelude.Text)
listEntitiesDetectionV2JobsResponse_nextToken = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {nextToken} -> nextToken) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {nextToken = a} :: ListEntitiesDetectionV2JobsResponse)

-- | The response's http status code.
listEntitiesDetectionV2JobsResponse_httpStatus :: Lens.Lens' ListEntitiesDetectionV2JobsResponse Prelude.Int
listEntitiesDetectionV2JobsResponse_httpStatus = Lens.lens (\ListEntitiesDetectionV2JobsResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesDetectionV2JobsResponse' {} a -> s {httpStatus = a} :: ListEntitiesDetectionV2JobsResponse)

instance
  Prelude.NFData
    ListEntitiesDetectionV2JobsResponse
  where
  rnf ListEntitiesDetectionV2JobsResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

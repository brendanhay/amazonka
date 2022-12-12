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
-- Module      : Amazonka.Translate.ListTextTranslationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch translation jobs that you have submitted.
module Amazonka.Translate.ListTextTranslationJobs
  ( -- * Creating a Request
    ListTextTranslationJobs (..),
    newListTextTranslationJobs,

    -- * Request Lenses
    listTextTranslationJobs_filter,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobs_nextToken,

    -- * Destructuring the Response
    ListTextTranslationJobsResponse (..),
    newListTextTranslationJobsResponse,

    -- * Response Lenses
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newListTextTranslationJobs' smart constructor.
data ListTextTranslationJobs = ListTextTranslationJobs'
  { -- | The parameters that specify which batch translation jobs to retrieve.
    -- Filters include job name, job status, and submission time. You can only
    -- set one filter at a time.
    filter' :: Prelude.Maybe TextTranslationJobFilter,
    -- | The maximum number of results to return in each page. The default value
    -- is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTextTranslationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listTextTranslationJobs_filter' - The parameters that specify which batch translation jobs to retrieve.
-- Filters include job name, job status, and submission time. You can only
-- set one filter at a time.
--
-- 'maxResults', 'listTextTranslationJobs_maxResults' - The maximum number of results to return in each page. The default value
-- is 100.
--
-- 'nextToken', 'listTextTranslationJobs_nextToken' - The token to request the next page of results.
newListTextTranslationJobs ::
  ListTextTranslationJobs
newListTextTranslationJobs =
  ListTextTranslationJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The parameters that specify which batch translation jobs to retrieve.
-- Filters include job name, job status, and submission time. You can only
-- set one filter at a time.
listTextTranslationJobs_filter :: Lens.Lens' ListTextTranslationJobs (Prelude.Maybe TextTranslationJobFilter)
listTextTranslationJobs_filter = Lens.lens (\ListTextTranslationJobs' {filter'} -> filter') (\s@ListTextTranslationJobs' {} a -> s {filter' = a} :: ListTextTranslationJobs)

-- | The maximum number of results to return in each page. The default value
-- is 100.
listTextTranslationJobs_maxResults :: Lens.Lens' ListTextTranslationJobs (Prelude.Maybe Prelude.Natural)
listTextTranslationJobs_maxResults = Lens.lens (\ListTextTranslationJobs' {maxResults} -> maxResults) (\s@ListTextTranslationJobs' {} a -> s {maxResults = a} :: ListTextTranslationJobs)

-- | The token to request the next page of results.
listTextTranslationJobs_nextToken :: Lens.Lens' ListTextTranslationJobs (Prelude.Maybe Prelude.Text)
listTextTranslationJobs_nextToken = Lens.lens (\ListTextTranslationJobs' {nextToken} -> nextToken) (\s@ListTextTranslationJobs' {} a -> s {nextToken = a} :: ListTextTranslationJobs)

instance Core.AWSRequest ListTextTranslationJobs where
  type
    AWSResponse ListTextTranslationJobs =
      ListTextTranslationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTextTranslationJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "TextTranslationJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTextTranslationJobs where
  hashWithSalt _salt ListTextTranslationJobs' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTextTranslationJobs where
  rnf ListTextTranslationJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListTextTranslationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.ListTextTranslationJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTextTranslationJobs where
  toJSON ListTextTranslationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTextTranslationJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTextTranslationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTextTranslationJobsResponse' smart constructor.
data ListTextTranslationJobsResponse = ListTextTranslationJobsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    textTranslationJobPropertiesList :: Prelude.Maybe [TextTranslationJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTextTranslationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTextTranslationJobsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'textTranslationJobPropertiesList', 'listTextTranslationJobsResponse_textTranslationJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listTextTranslationJobsResponse_httpStatus' - The response's http status code.
newListTextTranslationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTextTranslationJobsResponse
newListTextTranslationJobsResponse pHttpStatus_ =
  ListTextTranslationJobsResponse'
    { nextToken =
        Prelude.Nothing,
      textTranslationJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listTextTranslationJobsResponse_nextToken :: Lens.Lens' ListTextTranslationJobsResponse (Prelude.Maybe Prelude.Text)
listTextTranslationJobsResponse_nextToken = Lens.lens (\ListTextTranslationJobsResponse' {nextToken} -> nextToken) (\s@ListTextTranslationJobsResponse' {} a -> s {nextToken = a} :: ListTextTranslationJobsResponse)

-- | A list containing the properties of each job that is returned.
listTextTranslationJobsResponse_textTranslationJobPropertiesList :: Lens.Lens' ListTextTranslationJobsResponse (Prelude.Maybe [TextTranslationJobProperties])
listTextTranslationJobsResponse_textTranslationJobPropertiesList = Lens.lens (\ListTextTranslationJobsResponse' {textTranslationJobPropertiesList} -> textTranslationJobPropertiesList) (\s@ListTextTranslationJobsResponse' {} a -> s {textTranslationJobPropertiesList = a} :: ListTextTranslationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTextTranslationJobsResponse_httpStatus :: Lens.Lens' ListTextTranslationJobsResponse Prelude.Int
listTextTranslationJobsResponse_httpStatus = Lens.lens (\ListTextTranslationJobsResponse' {httpStatus} -> httpStatus) (\s@ListTextTranslationJobsResponse' {} a -> s {httpStatus = a} :: ListTextTranslationJobsResponse)

instance
  Prelude.NFData
    ListTextTranslationJobsResponse
  where
  rnf ListTextTranslationJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf textTranslationJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus

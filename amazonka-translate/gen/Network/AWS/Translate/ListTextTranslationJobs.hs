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
-- Module      : Network.AWS.Translate.ListTextTranslationJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch translation jobs that you have submitted.
module Network.AWS.Translate.ListTextTranslationJobs
  ( -- * Creating a Request
    ListTextTranslationJobs (..),
    newListTextTranslationJobs,

    -- * Request Lenses
    listTextTranslationJobs_nextToken,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobs_filter,

    -- * Destructuring the Response
    ListTextTranslationJobsResponse (..),
    newListTextTranslationJobsResponse,

    -- * Response Lenses
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newListTextTranslationJobs' smart constructor.
data ListTextTranslationJobs = ListTextTranslationJobs'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default value
    -- is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameters that specify which batch translation jobs to retrieve.
    -- Filters include job name, job status, and submission time. You can only
    -- set one filter at a time.
    filter' :: Core.Maybe TextTranslationJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTextTranslationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTextTranslationJobs_nextToken' - The token to request the next page of results.
--
-- 'maxResults', 'listTextTranslationJobs_maxResults' - The maximum number of results to return in each page. The default value
-- is 100.
--
-- 'filter'', 'listTextTranslationJobs_filter' - The parameters that specify which batch translation jobs to retrieve.
-- Filters include job name, job status, and submission time. You can only
-- set one filter at a time.
newListTextTranslationJobs ::
  ListTextTranslationJobs
newListTextTranslationJobs =
  ListTextTranslationJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | The token to request the next page of results.
listTextTranslationJobs_nextToken :: Lens.Lens' ListTextTranslationJobs (Core.Maybe Core.Text)
listTextTranslationJobs_nextToken = Lens.lens (\ListTextTranslationJobs' {nextToken} -> nextToken) (\s@ListTextTranslationJobs' {} a -> s {nextToken = a} :: ListTextTranslationJobs)

-- | The maximum number of results to return in each page. The default value
-- is 100.
listTextTranslationJobs_maxResults :: Lens.Lens' ListTextTranslationJobs (Core.Maybe Core.Natural)
listTextTranslationJobs_maxResults = Lens.lens (\ListTextTranslationJobs' {maxResults} -> maxResults) (\s@ListTextTranslationJobs' {} a -> s {maxResults = a} :: ListTextTranslationJobs)

-- | The parameters that specify which batch translation jobs to retrieve.
-- Filters include job name, job status, and submission time. You can only
-- set one filter at a time.
listTextTranslationJobs_filter :: Lens.Lens' ListTextTranslationJobs (Core.Maybe TextTranslationJobFilter)
listTextTranslationJobs_filter = Lens.lens (\ListTextTranslationJobs' {filter'} -> filter') (\s@ListTextTranslationJobs' {} a -> s {filter' = a} :: ListTextTranslationJobs)

instance Core.AWSRequest ListTextTranslationJobs where
  type
    AWSResponse ListTextTranslationJobs =
      ListTextTranslationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTextTranslationJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TextTranslationJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTextTranslationJobs

instance Core.NFData ListTextTranslationJobs

instance Core.ToHeaders ListTextTranslationJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ListTextTranslationJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTextTranslationJobs where
  toJSON ListTextTranslationJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListTextTranslationJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListTextTranslationJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTextTranslationJobsResponse' smart constructor.
data ListTextTranslationJobsResponse = ListTextTranslationJobsResponse'
  { -- | The token to use to retreive the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list containing the properties of each job that is returned.
    textTranslationJobPropertiesList :: Core.Maybe [TextTranslationJobProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTextTranslationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTextTranslationJobsResponse_nextToken' - The token to use to retreive the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'textTranslationJobPropertiesList', 'listTextTranslationJobsResponse_textTranslationJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listTextTranslationJobsResponse_httpStatus' - The response's http status code.
newListTextTranslationJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTextTranslationJobsResponse
newListTextTranslationJobsResponse pHttpStatus_ =
  ListTextTranslationJobsResponse'
    { nextToken =
        Core.Nothing,
      textTranslationJobPropertiesList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retreive the next page of results. This value is
-- @null@ when there are no more results to return.
listTextTranslationJobsResponse_nextToken :: Lens.Lens' ListTextTranslationJobsResponse (Core.Maybe Core.Text)
listTextTranslationJobsResponse_nextToken = Lens.lens (\ListTextTranslationJobsResponse' {nextToken} -> nextToken) (\s@ListTextTranslationJobsResponse' {} a -> s {nextToken = a} :: ListTextTranslationJobsResponse)

-- | A list containing the properties of each job that is returned.
listTextTranslationJobsResponse_textTranslationJobPropertiesList :: Lens.Lens' ListTextTranslationJobsResponse (Core.Maybe [TextTranslationJobProperties])
listTextTranslationJobsResponse_textTranslationJobPropertiesList = Lens.lens (\ListTextTranslationJobsResponse' {textTranslationJobPropertiesList} -> textTranslationJobPropertiesList) (\s@ListTextTranslationJobsResponse' {} a -> s {textTranslationJobPropertiesList = a} :: ListTextTranslationJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTextTranslationJobsResponse_httpStatus :: Lens.Lens' ListTextTranslationJobsResponse Core.Int
listTextTranslationJobsResponse_httpStatus = Lens.lens (\ListTextTranslationJobsResponse' {httpStatus} -> httpStatus) (\s@ListTextTranslationJobsResponse' {} a -> s {httpStatus = a} :: ListTextTranslationJobsResponse)

instance Core.NFData ListTextTranslationJobsResponse

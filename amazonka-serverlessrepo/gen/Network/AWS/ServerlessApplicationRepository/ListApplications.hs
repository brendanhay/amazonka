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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists applications owned by the requester.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_nextToken,
    listApplications_maxItems,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | A token to specify where to start paginating.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplications_nextToken' - A token to specify where to start paginating.
--
-- 'maxItems', 'listApplications_maxItems' - The total number of items to return.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications'
    { nextToken = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | A token to specify where to start paginating.
listApplications_nextToken :: Lens.Lens' ListApplications (Core.Maybe Core.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

-- | The total number of items to return.
listApplications_maxItems :: Lens.Lens' ListApplications (Core.Maybe Core.Natural)
listApplications_maxItems = Lens.lens (\ListApplications' {maxItems} -> maxItems) (\s@ListApplications' {} a -> s {maxItems = a} :: ListApplications)

instance Core.AWSPager ListApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_applications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplications_nextToken
          Lens..~ rs
          Lens.^? listApplicationsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListApplications where
  type
    AWSResponse ListApplications =
      ListApplicationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "applications" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplications

instance Core.NFData ListApplications

instance Core.ToHeaders ListApplications where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListApplications where
  toPath = Core.const "/applications"

instance Core.ToQuery ListApplications where
  toQuery ListApplications' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of application summaries.
    applications :: Core.Maybe [ApplicationSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - The token to request the next page of results.
--
-- 'applications', 'listApplicationsResponse_applications' - An array of application summaries.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { nextToken = Core.Nothing,
      applications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Core.Maybe Core.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | An array of application summaries.
listApplicationsResponse_applications :: Lens.Lens' ListApplicationsResponse (Core.Maybe [ApplicationSummary])
listApplicationsResponse_applications = Lens.lens (\ListApplicationsResponse' {applications} -> applications) (\s@ListApplicationsResponse' {} a -> s {applications = a} :: ListApplicationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Core.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

instance Core.NFData ListApplicationsResponse

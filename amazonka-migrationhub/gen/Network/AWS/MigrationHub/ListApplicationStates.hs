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
-- Module      : Network.AWS.MigrationHub.ListApplicationStates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the migration statuses for your applications. If you use the
-- optional @ApplicationIds@ parameter, only the migration statuses for
-- those applications will be returned.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListApplicationStates
  ( -- * Creating a Request
    ListApplicationStates (..),
    newListApplicationStates,

    -- * Request Lenses
    listApplicationStates_nextToken,
    listApplicationStates_maxResults,
    listApplicationStates_applicationIds,

    -- * Destructuring the Response
    ListApplicationStatesResponse (..),
    newListApplicationStatesResponse,

    -- * Response Lenses
    listApplicationStatesResponse_applicationStateList,
    listApplicationStatesResponse_nextToken,
    listApplicationStatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of results to be returned per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The configurationIds from the Application Discovery Service that
    -- uniquely identifies your applications.
    applicationIds :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationStates_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'maxResults', 'listApplicationStates_maxResults' - Maximum number of results to be returned per page.
--
-- 'applicationIds', 'listApplicationStates_applicationIds' - The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
newListApplicationStates ::
  ListApplicationStates
newListApplicationStates =
  ListApplicationStates'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      applicationIds = Core.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStates_nextToken :: Lens.Lens' ListApplicationStates (Core.Maybe Core.Text)
listApplicationStates_nextToken = Lens.lens (\ListApplicationStates' {nextToken} -> nextToken) (\s@ListApplicationStates' {} a -> s {nextToken = a} :: ListApplicationStates)

-- | Maximum number of results to be returned per page.
listApplicationStates_maxResults :: Lens.Lens' ListApplicationStates (Core.Maybe Core.Natural)
listApplicationStates_maxResults = Lens.lens (\ListApplicationStates' {maxResults} -> maxResults) (\s@ListApplicationStates' {} a -> s {maxResults = a} :: ListApplicationStates)

-- | The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
listApplicationStates_applicationIds :: Lens.Lens' ListApplicationStates (Core.Maybe (Core.NonEmpty Core.Text))
listApplicationStates_applicationIds = Lens.lens (\ListApplicationStates' {applicationIds} -> applicationIds) (\s@ListApplicationStates' {} a -> s {applicationIds = a} :: ListApplicationStates) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListApplicationStates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationStatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationStatesResponse_applicationStateList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplicationStates_nextToken
          Lens..~ rs
          Lens.^? listApplicationStatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListApplicationStates where
  type
    AWSResponse ListApplicationStates =
      ListApplicationStatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationStatesResponse'
            Core.<$> ( x Core..?> "ApplicationStateList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplicationStates

instance Core.NFData ListApplicationStates

instance Core.ToHeaders ListApplicationStates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListApplicationStates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApplicationStates where
  toJSON ListApplicationStates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ApplicationIds" Core..=) Core.<$> applicationIds
          ]
      )

instance Core.ToPath ListApplicationStates where
  toPath = Core.const "/"

instance Core.ToQuery ListApplicationStates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { -- | A list of Applications that exist in Application Discovery Service.
    applicationStateList :: Core.Maybe [ApplicationState],
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationStateList', 'listApplicationStatesResponse_applicationStateList' - A list of Applications that exist in Application Discovery Service.
--
-- 'nextToken', 'listApplicationStatesResponse_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'httpStatus', 'listApplicationStatesResponse_httpStatus' - The response's http status code.
newListApplicationStatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApplicationStatesResponse
newListApplicationStatesResponse pHttpStatus_ =
  ListApplicationStatesResponse'
    { applicationStateList =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Applications that exist in Application Discovery Service.
listApplicationStatesResponse_applicationStateList :: Lens.Lens' ListApplicationStatesResponse (Core.Maybe [ApplicationState])
listApplicationStatesResponse_applicationStateList = Lens.lens (\ListApplicationStatesResponse' {applicationStateList} -> applicationStateList) (\s@ListApplicationStatesResponse' {} a -> s {applicationStateList = a} :: ListApplicationStatesResponse) Core.. Lens.mapping Lens._Coerce

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStatesResponse_nextToken :: Lens.Lens' ListApplicationStatesResponse (Core.Maybe Core.Text)
listApplicationStatesResponse_nextToken = Lens.lens (\ListApplicationStatesResponse' {nextToken} -> nextToken) (\s@ListApplicationStatesResponse' {} a -> s {nextToken = a} :: ListApplicationStatesResponse)

-- | The response's http status code.
listApplicationStatesResponse_httpStatus :: Lens.Lens' ListApplicationStatesResponse Core.Int
listApplicationStatesResponse_httpStatus = Lens.lens (\ListApplicationStatesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationStatesResponse' {} a -> s {httpStatus = a} :: ListApplicationStatesResponse)

instance Core.NFData ListApplicationStatesResponse

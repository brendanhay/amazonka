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
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the routing profiles for the
-- specified Amazon Connect instance.
--
-- For more information about routing profiles, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html Routing Profiles>
-- and
-- <https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html Create a Routing Profile>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
  ( -- * Creating a Request
    ListRoutingProfiles (..),
    newListRoutingProfiles,

    -- * Request Lenses
    listRoutingProfiles_nextToken,
    listRoutingProfiles_maxResults,
    listRoutingProfiles_instanceId,

    -- * Destructuring the Response
    ListRoutingProfilesResponse (..),
    newListRoutingProfilesResponse,

    -- * Response Lenses
    listRoutingProfilesResponse_nextToken,
    listRoutingProfilesResponse_routingProfileSummaryList,
    listRoutingProfilesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoutingProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listRoutingProfiles_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listRoutingProfiles_instanceId' - The identifier of the Amazon Connect instance.
newListRoutingProfiles ::
  -- | 'instanceId'
  Core.Text ->
  ListRoutingProfiles
newListRoutingProfiles pInstanceId_ =
  ListRoutingProfiles'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRoutingProfiles_nextToken :: Lens.Lens' ListRoutingProfiles (Core.Maybe Core.Text)
listRoutingProfiles_nextToken = Lens.lens (\ListRoutingProfiles' {nextToken} -> nextToken) (\s@ListRoutingProfiles' {} a -> s {nextToken = a} :: ListRoutingProfiles)

-- | The maximum number of results to return per page.
listRoutingProfiles_maxResults :: Lens.Lens' ListRoutingProfiles (Core.Maybe Core.Natural)
listRoutingProfiles_maxResults = Lens.lens (\ListRoutingProfiles' {maxResults} -> maxResults) (\s@ListRoutingProfiles' {} a -> s {maxResults = a} :: ListRoutingProfiles)

-- | The identifier of the Amazon Connect instance.
listRoutingProfiles_instanceId :: Lens.Lens' ListRoutingProfiles Core.Text
listRoutingProfiles_instanceId = Lens.lens (\ListRoutingProfiles' {instanceId} -> instanceId) (\s@ListRoutingProfiles' {} a -> s {instanceId = a} :: ListRoutingProfiles)

instance Core.AWSPager ListRoutingProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoutingProfilesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRoutingProfilesResponse_routingProfileSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRoutingProfiles_nextToken
          Lens..~ rs
          Lens.^? listRoutingProfilesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListRoutingProfiles where
  type
    AWSResponse ListRoutingProfiles =
      ListRoutingProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingProfilesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "RoutingProfileSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRoutingProfiles

instance Core.NFData ListRoutingProfiles

instance Core.ToHeaders ListRoutingProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListRoutingProfiles where
  toPath ListRoutingProfiles' {..} =
    Core.mconcat
      ["/routing-profiles-summary/", Core.toBS instanceId]

instance Core.ToQuery ListRoutingProfiles where
  toQuery ListRoutingProfiles' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the routing profiles.
    routingProfileSummaryList :: Core.Maybe [RoutingProfileSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoutingProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfilesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'routingProfileSummaryList', 'listRoutingProfilesResponse_routingProfileSummaryList' - Information about the routing profiles.
--
-- 'httpStatus', 'listRoutingProfilesResponse_httpStatus' - The response's http status code.
newListRoutingProfilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRoutingProfilesResponse
newListRoutingProfilesResponse pHttpStatus_ =
  ListRoutingProfilesResponse'
    { nextToken =
        Core.Nothing,
      routingProfileSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRoutingProfilesResponse_nextToken :: Lens.Lens' ListRoutingProfilesResponse (Core.Maybe Core.Text)
listRoutingProfilesResponse_nextToken = Lens.lens (\ListRoutingProfilesResponse' {nextToken} -> nextToken) (\s@ListRoutingProfilesResponse' {} a -> s {nextToken = a} :: ListRoutingProfilesResponse)

-- | Information about the routing profiles.
listRoutingProfilesResponse_routingProfileSummaryList :: Lens.Lens' ListRoutingProfilesResponse (Core.Maybe [RoutingProfileSummary])
listRoutingProfilesResponse_routingProfileSummaryList = Lens.lens (\ListRoutingProfilesResponse' {routingProfileSummaryList} -> routingProfileSummaryList) (\s@ListRoutingProfilesResponse' {} a -> s {routingProfileSummaryList = a} :: ListRoutingProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRoutingProfilesResponse_httpStatus :: Lens.Lens' ListRoutingProfilesResponse Core.Int
listRoutingProfilesResponse_httpStatus = Lens.lens (\ListRoutingProfilesResponse' {httpStatus} -> httpStatus) (\s@ListRoutingProfilesResponse' {} a -> s {httpStatus = a} :: ListRoutingProfilesResponse)

instance Core.NFData ListRoutingProfilesResponse

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
-- Module      : Network.AWS.WorkMail.ListResourceDelegates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the delegates associated with a resource. Users and groups can be
-- resource delegates and answer requests on behalf of the resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResourceDelegates
  ( -- * Creating a Request
    ListResourceDelegates (..),
    newListResourceDelegates,

    -- * Request Lenses
    listResourceDelegates_nextToken,
    listResourceDelegates_maxResults,
    listResourceDelegates_organizationId,
    listResourceDelegates_resourceId,

    -- * Destructuring the Response
    ListResourceDelegatesResponse (..),
    newListResourceDelegatesResponse,

    -- * Response Lenses
    listResourceDelegatesResponse_nextToken,
    listResourceDelegatesResponse_delegates,
    listResourceDelegatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListResourceDelegates' smart constructor.
data ListResourceDelegates = ListResourceDelegates'
  { -- | The token used to paginate through the delegates associated with a
    -- resource.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of maximum results in a page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier for the organization that contains the resource for which
    -- delegates are listed.
    organizationId :: Core.Text,
    -- | The identifier for the resource whose delegates are listed.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDelegates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDelegates_nextToken' - The token used to paginate through the delegates associated with a
-- resource.
--
-- 'maxResults', 'listResourceDelegates_maxResults' - The number of maximum results in a page.
--
-- 'organizationId', 'listResourceDelegates_organizationId' - The identifier for the organization that contains the resource for which
-- delegates are listed.
--
-- 'resourceId', 'listResourceDelegates_resourceId' - The identifier for the resource whose delegates are listed.
newListResourceDelegates ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  ListResourceDelegates
newListResourceDelegates
  pOrganizationId_
  pResourceId_ =
    ListResourceDelegates'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        organizationId = pOrganizationId_,
        resourceId = pResourceId_
      }

-- | The token used to paginate through the delegates associated with a
-- resource.
listResourceDelegates_nextToken :: Lens.Lens' ListResourceDelegates (Core.Maybe Core.Text)
listResourceDelegates_nextToken = Lens.lens (\ListResourceDelegates' {nextToken} -> nextToken) (\s@ListResourceDelegates' {} a -> s {nextToken = a} :: ListResourceDelegates)

-- | The number of maximum results in a page.
listResourceDelegates_maxResults :: Lens.Lens' ListResourceDelegates (Core.Maybe Core.Natural)
listResourceDelegates_maxResults = Lens.lens (\ListResourceDelegates' {maxResults} -> maxResults) (\s@ListResourceDelegates' {} a -> s {maxResults = a} :: ListResourceDelegates)

-- | The identifier for the organization that contains the resource for which
-- delegates are listed.
listResourceDelegates_organizationId :: Lens.Lens' ListResourceDelegates Core.Text
listResourceDelegates_organizationId = Lens.lens (\ListResourceDelegates' {organizationId} -> organizationId) (\s@ListResourceDelegates' {} a -> s {organizationId = a} :: ListResourceDelegates)

-- | The identifier for the resource whose delegates are listed.
listResourceDelegates_resourceId :: Lens.Lens' ListResourceDelegates Core.Text
listResourceDelegates_resourceId = Lens.lens (\ListResourceDelegates' {resourceId} -> resourceId) (\s@ListResourceDelegates' {} a -> s {resourceId = a} :: ListResourceDelegates)

instance Core.AWSPager ListResourceDelegates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDelegatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDelegatesResponse_delegates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourceDelegates_nextToken
          Lens..~ rs
          Lens.^? listResourceDelegatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListResourceDelegates where
  type
    AWSResponse ListResourceDelegates =
      ListResourceDelegatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDelegatesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Delegates" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourceDelegates

instance Core.NFData ListResourceDelegates

instance Core.ToHeaders ListResourceDelegates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListResourceDelegates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResourceDelegates where
  toJSON ListResourceDelegates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath ListResourceDelegates where
  toPath = Core.const "/"

instance Core.ToQuery ListResourceDelegates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourceDelegatesResponse' smart constructor.
data ListResourceDelegatesResponse = ListResourceDelegatesResponse'
  { -- | The token used to paginate through the delegates associated with a
    -- resource. While results are still available, it has an associated value.
    -- When the last page is reached, the token is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | One page of the resource\'s delegates.
    delegates :: Core.Maybe [Delegate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDelegatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDelegatesResponse_nextToken' - The token used to paginate through the delegates associated with a
-- resource. While results are still available, it has an associated value.
-- When the last page is reached, the token is empty.
--
-- 'delegates', 'listResourceDelegatesResponse_delegates' - One page of the resource\'s delegates.
--
-- 'httpStatus', 'listResourceDelegatesResponse_httpStatus' - The response's http status code.
newListResourceDelegatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourceDelegatesResponse
newListResourceDelegatesResponse pHttpStatus_ =
  ListResourceDelegatesResponse'
    { nextToken =
        Core.Nothing,
      delegates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate through the delegates associated with a
-- resource. While results are still available, it has an associated value.
-- When the last page is reached, the token is empty.
listResourceDelegatesResponse_nextToken :: Lens.Lens' ListResourceDelegatesResponse (Core.Maybe Core.Text)
listResourceDelegatesResponse_nextToken = Lens.lens (\ListResourceDelegatesResponse' {nextToken} -> nextToken) (\s@ListResourceDelegatesResponse' {} a -> s {nextToken = a} :: ListResourceDelegatesResponse)

-- | One page of the resource\'s delegates.
listResourceDelegatesResponse_delegates :: Lens.Lens' ListResourceDelegatesResponse (Core.Maybe [Delegate])
listResourceDelegatesResponse_delegates = Lens.lens (\ListResourceDelegatesResponse' {delegates} -> delegates) (\s@ListResourceDelegatesResponse' {} a -> s {delegates = a} :: ListResourceDelegatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceDelegatesResponse_httpStatus :: Lens.Lens' ListResourceDelegatesResponse Core.Int
listResourceDelegatesResponse_httpStatus = Lens.lens (\ListResourceDelegatesResponse' {httpStatus} -> httpStatus) (\s@ListResourceDelegatesResponse' {} a -> s {httpStatus = a} :: ListResourceDelegatesResponse)

instance Core.NFData ListResourceDelegatesResponse

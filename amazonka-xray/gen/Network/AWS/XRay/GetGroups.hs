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
-- Module      : Network.AWS.XRay.GetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active group details.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetGroups
  ( -- * Creating a Request
    GetGroups (..),
    newGetGroups,

    -- * Request Lenses
    getGroups_nextToken,

    -- * Destructuring the Response
    GetGroupsResponse (..),
    newGetGroupsResponse,

    -- * Response Lenses
    getGroupsResponse_groups,
    getGroupsResponse_nextToken,
    getGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetGroups' smart constructor.
data GetGroups = GetGroups'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getGroups_nextToken' - Pagination token.
newGetGroups ::
  GetGroups
newGetGroups = GetGroups' {nextToken = Core.Nothing}

-- | Pagination token.
getGroups_nextToken :: Lens.Lens' GetGroups (Core.Maybe Core.Text)
getGroups_nextToken = Lens.lens (\GetGroups' {nextToken} -> nextToken) (\s@GetGroups' {} a -> s {nextToken = a} :: GetGroups)

instance Core.AWSPager GetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getGroupsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getGroupsResponse_groups Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getGroups_nextToken
          Lens..~ rs
          Lens.^? getGroupsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetGroups where
  type AWSResponse GetGroups = GetGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupsResponse'
            Core.<$> (x Core..?> "Groups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGroups

instance Core.NFData GetGroups

instance Core.ToHeaders GetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetGroups where
  toJSON GetGroups' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath GetGroups where
  toPath = Core.const "/Groups"

instance Core.ToQuery GetGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { -- | The collection of all active groups.
    groups :: Core.Maybe [GroupSummary],
    -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'getGroupsResponse_groups' - The collection of all active groups.
--
-- 'nextToken', 'getGroupsResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'getGroupsResponse_httpStatus' - The response's http status code.
newGetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGroupsResponse
newGetGroupsResponse pHttpStatus_ =
  GetGroupsResponse'
    { groups = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of all active groups.
getGroupsResponse_groups :: Lens.Lens' GetGroupsResponse (Core.Maybe [GroupSummary])
getGroupsResponse_groups = Lens.lens (\GetGroupsResponse' {groups} -> groups) (\s@GetGroupsResponse' {} a -> s {groups = a} :: GetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | Pagination token.
getGroupsResponse_nextToken :: Lens.Lens' GetGroupsResponse (Core.Maybe Core.Text)
getGroupsResponse_nextToken = Lens.lens (\GetGroupsResponse' {nextToken} -> nextToken) (\s@GetGroupsResponse' {} a -> s {nextToken = a} :: GetGroupsResponse)

-- | The response's http status code.
getGroupsResponse_httpStatus :: Lens.Lens' GetGroupsResponse Core.Int
getGroupsResponse_httpStatus = Lens.lens (\GetGroupsResponse' {httpStatus} -> httpStatus) (\s@GetGroupsResponse' {} a -> s {httpStatus = a} :: GetGroupsResponse)

instance Core.NFData GetGroupsResponse

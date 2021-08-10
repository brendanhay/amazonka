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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetGroups' smart constructor.
data GetGroups = GetGroups'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newGetGroups =
  GetGroups' {nextToken = Prelude.Nothing}

-- | Pagination token.
getGroups_nextToken :: Lens.Lens' GetGroups (Prelude.Maybe Prelude.Text)
getGroups_nextToken = Lens.lens (\GetGroups' {nextToken} -> nextToken) (\s@GetGroups' {} a -> s {nextToken = a} :: GetGroups)

instance Core.AWSPager GetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getGroupsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getGroupsResponse_groups Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getGroups_nextToken
          Lens..~ rs
          Lens.^? getGroupsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetGroups where
  type AWSResponse GetGroups = GetGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupsResponse'
            Prelude.<$> (x Core..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGroups

instance Prelude.NFData GetGroups

instance Core.ToHeaders GetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetGroups where
  toJSON GetGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [("NextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath GetGroups where
  toPath = Prelude.const "/Groups"

instance Core.ToQuery GetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { -- | The collection of all active groups.
    groups :: Prelude.Maybe [GroupSummary],
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetGroupsResponse
newGetGroupsResponse pHttpStatus_ =
  GetGroupsResponse'
    { groups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of all active groups.
getGroupsResponse_groups :: Lens.Lens' GetGroupsResponse (Prelude.Maybe [GroupSummary])
getGroupsResponse_groups = Lens.lens (\GetGroupsResponse' {groups} -> groups) (\s@GetGroupsResponse' {} a -> s {groups = a} :: GetGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Pagination token.
getGroupsResponse_nextToken :: Lens.Lens' GetGroupsResponse (Prelude.Maybe Prelude.Text)
getGroupsResponse_nextToken = Lens.lens (\GetGroupsResponse' {nextToken} -> nextToken) (\s@GetGroupsResponse' {} a -> s {nextToken = a} :: GetGroupsResponse)

-- | The response's http status code.
getGroupsResponse_httpStatus :: Lens.Lens' GetGroupsResponse Prelude.Int
getGroupsResponse_httpStatus = Lens.lens (\GetGroupsResponse' {httpStatus} -> httpStatus) (\s@GetGroupsResponse' {} a -> s {httpStatus = a} :: GetGroupsResponse)

instance Prelude.NFData GetGroupsResponse

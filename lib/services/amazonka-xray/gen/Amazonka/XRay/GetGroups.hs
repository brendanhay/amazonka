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
-- Module      : Amazonka.XRay.GetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active group details.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetGroups
  ( -- * Creating a Request
    GetGroups (..),
    newGetGroups,

    -- * Request Lenses
    getGroups_nextToken,

    -- * Destructuring the Response
    GetGroupsResponse (..),
    newGetGroupsResponse,

    -- * Response Lenses
    getGroupsResponse_nextToken,
    getGroupsResponse_groups,
    getGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGroups where
  hashWithSalt _salt GetGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetGroups where
  rnf GetGroups' {..} = Prelude.rnf nextToken

instance Data.ToHeaders GetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetGroups where
  toJSON GetGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath GetGroups where
  toPath = Prelude.const "/Groups"

instance Data.ToQuery GetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of all active groups.
    groups :: Prelude.Maybe [GroupSummary],
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
-- 'nextToken', 'getGroupsResponse_nextToken' - Pagination token.
--
-- 'groups', 'getGroupsResponse_groups' - The collection of all active groups.
--
-- 'httpStatus', 'getGroupsResponse_httpStatus' - The response's http status code.
newGetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupsResponse
newGetGroupsResponse pHttpStatus_ =
  GetGroupsResponse'
    { nextToken = Prelude.Nothing,
      groups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getGroupsResponse_nextToken :: Lens.Lens' GetGroupsResponse (Prelude.Maybe Prelude.Text)
getGroupsResponse_nextToken = Lens.lens (\GetGroupsResponse' {nextToken} -> nextToken) (\s@GetGroupsResponse' {} a -> s {nextToken = a} :: GetGroupsResponse)

-- | The collection of all active groups.
getGroupsResponse_groups :: Lens.Lens' GetGroupsResponse (Prelude.Maybe [GroupSummary])
getGroupsResponse_groups = Lens.lens (\GetGroupsResponse' {groups} -> groups) (\s@GetGroupsResponse' {} a -> s {groups = a} :: GetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getGroupsResponse_httpStatus :: Lens.Lens' GetGroupsResponse Prelude.Int
getGroupsResponse_httpStatus = Lens.lens (\GetGroupsResponse' {httpStatus} -> httpStatus) (\s@GetGroupsResponse' {} a -> s {httpStatus = a} :: GetGroupsResponse)

instance Prelude.NFData GetGroupsResponse where
  rnf GetGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf httpStatus

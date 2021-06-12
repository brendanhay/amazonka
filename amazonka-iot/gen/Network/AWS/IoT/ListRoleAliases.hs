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
-- Module      : Network.AWS.IoT.ListRoleAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the role aliases registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListRoleAliases
  ( -- * Creating a Request
    ListRoleAliases (..),
    newListRoleAliases,

    -- * Request Lenses
    listRoleAliases_pageSize,
    listRoleAliases_ascendingOrder,
    listRoleAliases_marker,

    -- * Destructuring the Response
    ListRoleAliasesResponse (..),
    newListRoleAliasesResponse,

    -- * Response Lenses
    listRoleAliasesResponse_roleAliases,
    listRoleAliasesResponse_nextMarker,
    listRoleAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { -- | The maximum number of results to return at one time.
    pageSize :: Core.Maybe Core.Natural,
    -- | Return the list of role aliases in ascending alphabetical order.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | A marker used to get the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoleAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listRoleAliases_pageSize' - The maximum number of results to return at one time.
--
-- 'ascendingOrder', 'listRoleAliases_ascendingOrder' - Return the list of role aliases in ascending alphabetical order.
--
-- 'marker', 'listRoleAliases_marker' - A marker used to get the next set of results.
newListRoleAliases ::
  ListRoleAliases
newListRoleAliases =
  ListRoleAliases'
    { pageSize = Core.Nothing,
      ascendingOrder = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of results to return at one time.
listRoleAliases_pageSize :: Lens.Lens' ListRoleAliases (Core.Maybe Core.Natural)
listRoleAliases_pageSize = Lens.lens (\ListRoleAliases' {pageSize} -> pageSize) (\s@ListRoleAliases' {} a -> s {pageSize = a} :: ListRoleAliases)

-- | Return the list of role aliases in ascending alphabetical order.
listRoleAliases_ascendingOrder :: Lens.Lens' ListRoleAliases (Core.Maybe Core.Bool)
listRoleAliases_ascendingOrder = Lens.lens (\ListRoleAliases' {ascendingOrder} -> ascendingOrder) (\s@ListRoleAliases' {} a -> s {ascendingOrder = a} :: ListRoleAliases)

-- | A marker used to get the next set of results.
listRoleAliases_marker :: Lens.Lens' ListRoleAliases (Core.Maybe Core.Text)
listRoleAliases_marker = Lens.lens (\ListRoleAliases' {marker} -> marker) (\s@ListRoleAliases' {} a -> s {marker = a} :: ListRoleAliases)

instance Core.AWSPager ListRoleAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoleAliasesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRoleAliasesResponse_roleAliases
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRoleAliases_marker
          Lens..~ rs
          Lens.^? listRoleAliasesResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListRoleAliases where
  type
    AWSResponse ListRoleAliases =
      ListRoleAliasesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoleAliasesResponse'
            Core.<$> (x Core..?> "roleAliases" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRoleAliases

instance Core.NFData ListRoleAliases

instance Core.ToHeaders ListRoleAliases where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListRoleAliases where
  toPath = Core.const "/role-aliases"

instance Core.ToQuery ListRoleAliases where
  toQuery ListRoleAliases' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListRoleAliasesResponse' smart constructor.
data ListRoleAliasesResponse = ListRoleAliasesResponse'
  { -- | The role aliases.
    roleAliases :: Core.Maybe [Core.Text],
    -- | A marker used to get the next set of results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoleAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAliases', 'listRoleAliasesResponse_roleAliases' - The role aliases.
--
-- 'nextMarker', 'listRoleAliasesResponse_nextMarker' - A marker used to get the next set of results.
--
-- 'httpStatus', 'listRoleAliasesResponse_httpStatus' - The response's http status code.
newListRoleAliasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRoleAliasesResponse
newListRoleAliasesResponse pHttpStatus_ =
  ListRoleAliasesResponse'
    { roleAliases =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role aliases.
listRoleAliasesResponse_roleAliases :: Lens.Lens' ListRoleAliasesResponse (Core.Maybe [Core.Text])
listRoleAliasesResponse_roleAliases = Lens.lens (\ListRoleAliasesResponse' {roleAliases} -> roleAliases) (\s@ListRoleAliasesResponse' {} a -> s {roleAliases = a} :: ListRoleAliasesResponse) Core.. Lens.mapping Lens._Coerce

-- | A marker used to get the next set of results.
listRoleAliasesResponse_nextMarker :: Lens.Lens' ListRoleAliasesResponse (Core.Maybe Core.Text)
listRoleAliasesResponse_nextMarker = Lens.lens (\ListRoleAliasesResponse' {nextMarker} -> nextMarker) (\s@ListRoleAliasesResponse' {} a -> s {nextMarker = a} :: ListRoleAliasesResponse)

-- | The response's http status code.
listRoleAliasesResponse_httpStatus :: Lens.Lens' ListRoleAliasesResponse Core.Int
listRoleAliasesResponse_httpStatus = Lens.lens (\ListRoleAliasesResponse' {httpStatus} -> httpStatus) (\s@ListRoleAliasesResponse' {} a -> s {httpStatus = a} :: ListRoleAliasesResponse)

instance Core.NFData ListRoleAliasesResponse

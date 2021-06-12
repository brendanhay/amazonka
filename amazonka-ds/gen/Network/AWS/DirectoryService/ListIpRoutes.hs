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
-- Module      : Network.AWS.DirectoryService.ListIpRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the address blocks that you have added to a directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListIpRoutes
  ( -- * Creating a Request
    ListIpRoutes (..),
    newListIpRoutes,

    -- * Request Lenses
    listIpRoutes_nextToken,
    listIpRoutes_limit,
    listIpRoutes_directoryId,

    -- * Destructuring the Response
    ListIpRoutesResponse (..),
    newListIpRoutesResponse,

    -- * Response Lenses
    listIpRoutesResponse_nextToken,
    listIpRoutesResponse_ipRoutesInfo,
    listIpRoutesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIpRoutes' smart constructor.
data ListIpRoutes = ListIpRoutes'
  { -- | The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
    -- Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of items to return. If this value is zero, the maximum
    -- number of items is specified by the limitations of the operation.
    limit :: Core.Maybe Core.Natural,
    -- | Identifier (ID) of the directory for which you want to retrieve the IP
    -- addresses.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIpRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIpRoutes_nextToken' - The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
-- Pass null if this is the first call.
--
-- 'limit', 'listIpRoutes_limit' - Maximum number of items to return. If this value is zero, the maximum
-- number of items is specified by the limitations of the operation.
--
-- 'directoryId', 'listIpRoutes_directoryId' - Identifier (ID) of the directory for which you want to retrieve the IP
-- addresses.
newListIpRoutes ::
  -- | 'directoryId'
  Core.Text ->
  ListIpRoutes
newListIpRoutes pDirectoryId_ =
  ListIpRoutes'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
-- Pass null if this is the first call.
listIpRoutes_nextToken :: Lens.Lens' ListIpRoutes (Core.Maybe Core.Text)
listIpRoutes_nextToken = Lens.lens (\ListIpRoutes' {nextToken} -> nextToken) (\s@ListIpRoutes' {} a -> s {nextToken = a} :: ListIpRoutes)

-- | Maximum number of items to return. If this value is zero, the maximum
-- number of items is specified by the limitations of the operation.
listIpRoutes_limit :: Lens.Lens' ListIpRoutes (Core.Maybe Core.Natural)
listIpRoutes_limit = Lens.lens (\ListIpRoutes' {limit} -> limit) (\s@ListIpRoutes' {} a -> s {limit = a} :: ListIpRoutes)

-- | Identifier (ID) of the directory for which you want to retrieve the IP
-- addresses.
listIpRoutes_directoryId :: Lens.Lens' ListIpRoutes Core.Text
listIpRoutes_directoryId = Lens.lens (\ListIpRoutes' {directoryId} -> directoryId) (\s@ListIpRoutes' {} a -> s {directoryId = a} :: ListIpRoutes)

instance Core.AWSPager ListIpRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIpRoutesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listIpRoutesResponse_ipRoutesInfo Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIpRoutes_nextToken
          Lens..~ rs
          Lens.^? listIpRoutesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListIpRoutes where
  type AWSResponse ListIpRoutes = ListIpRoutesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIpRoutesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "IpRoutesInfo" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIpRoutes

instance Core.NFData ListIpRoutes

instance Core.ToHeaders ListIpRoutes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ListIpRoutes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListIpRoutes where
  toJSON ListIpRoutes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath ListIpRoutes where
  toPath = Core.const "/"

instance Core.ToQuery ListIpRoutes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListIpRoutesResponse' smart constructor.
data ListIpRoutesResponse = ListIpRoutesResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
    -- the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of IpRoutes.
    ipRoutesInfo :: Core.Maybe [IpRouteInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIpRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIpRoutesResponse_nextToken' - If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
-- the next set of items.
--
-- 'ipRoutesInfo', 'listIpRoutesResponse_ipRoutesInfo' - A list of IpRoutes.
--
-- 'httpStatus', 'listIpRoutesResponse_httpStatus' - The response's http status code.
newListIpRoutesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIpRoutesResponse
newListIpRoutesResponse pHttpStatus_ =
  ListIpRoutesResponse'
    { nextToken = Core.Nothing,
      ipRoutesInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
-- the next set of items.
listIpRoutesResponse_nextToken :: Lens.Lens' ListIpRoutesResponse (Core.Maybe Core.Text)
listIpRoutesResponse_nextToken = Lens.lens (\ListIpRoutesResponse' {nextToken} -> nextToken) (\s@ListIpRoutesResponse' {} a -> s {nextToken = a} :: ListIpRoutesResponse)

-- | A list of IpRoutes.
listIpRoutesResponse_ipRoutesInfo :: Lens.Lens' ListIpRoutesResponse (Core.Maybe [IpRouteInfo])
listIpRoutesResponse_ipRoutesInfo = Lens.lens (\ListIpRoutesResponse' {ipRoutesInfo} -> ipRoutesInfo) (\s@ListIpRoutesResponse' {} a -> s {ipRoutesInfo = a} :: ListIpRoutesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIpRoutesResponse_httpStatus :: Lens.Lens' ListIpRoutesResponse Core.Int
listIpRoutesResponse_httpStatus = Lens.lens (\ListIpRoutesResponse' {httpStatus} -> httpStatus) (\s@ListIpRoutesResponse' {} a -> s {httpStatus = a} :: ListIpRoutesResponse)

instance Core.NFData ListIpRoutesResponse

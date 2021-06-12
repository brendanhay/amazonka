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
-- Module      : Network.AWS.StorageGateway.ListTapePools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists custom tape pools. You specify custom tape pools to list by
-- specifying one or more custom tape pool Amazon Resource Names (ARNs). If
-- you don\'t specify a custom tape pool ARN, the operation lists all
-- custom tape pools.
--
-- This operation supports pagination. You can optionally specify the
-- @Limit@ parameter in the body to limit the number of tape pools in the
-- response. If the number of tape pools returned in the response is
-- truncated, the response includes a @Marker@ element that you can use in
-- your subsequent request to retrieve the next set of tape pools.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapePools
  ( -- * Creating a Request
    ListTapePools (..),
    newListTapePools,

    -- * Request Lenses
    listTapePools_limit,
    listTapePools_poolARNs,
    listTapePools_marker,

    -- * Destructuring the Response
    ListTapePoolsResponse (..),
    newListTapePoolsResponse,

    -- * Response Lenses
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newListTapePools' smart constructor.
data ListTapePools = ListTapePools'
  { -- | An optional number limit for the tape pools in the list returned by this
    -- call.
    limit :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
    -- to list. If you don\'t specify a custom tape pool ARN, the response
    -- lists all custom tape pools.
    poolARNs :: Core.Maybe [Core.Text],
    -- | A string that indicates the position at which to begin the returned list
    -- of tape pools.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTapePools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listTapePools_limit' - An optional number limit for the tape pools in the list returned by this
-- call.
--
-- 'poolARNs', 'listTapePools_poolARNs' - The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
--
-- 'marker', 'listTapePools_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools.
newListTapePools ::
  ListTapePools
newListTapePools =
  ListTapePools'
    { limit = Core.Nothing,
      poolARNs = Core.Nothing,
      marker = Core.Nothing
    }

-- | An optional number limit for the tape pools in the list returned by this
-- call.
listTapePools_limit :: Lens.Lens' ListTapePools (Core.Maybe Core.Natural)
listTapePools_limit = Lens.lens (\ListTapePools' {limit} -> limit) (\s@ListTapePools' {} a -> s {limit = a} :: ListTapePools)

-- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
listTapePools_poolARNs :: Lens.Lens' ListTapePools (Core.Maybe [Core.Text])
listTapePools_poolARNs = Lens.lens (\ListTapePools' {poolARNs} -> poolARNs) (\s@ListTapePools' {} a -> s {poolARNs = a} :: ListTapePools) Core.. Lens.mapping Lens._Coerce

-- | A string that indicates the position at which to begin the returned list
-- of tape pools.
listTapePools_marker :: Lens.Lens' ListTapePools (Core.Maybe Core.Text)
listTapePools_marker = Lens.lens (\ListTapePools' {marker} -> marker) (\s@ListTapePools' {} a -> s {marker = a} :: ListTapePools)

instance Core.AWSPager ListTapePools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTapePoolsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTapePoolsResponse_poolInfos Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTapePools_marker
          Lens..~ rs
          Lens.^? listTapePoolsResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListTapePools where
  type
    AWSResponse ListTapePools =
      ListTapePoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapePoolsResponse'
            Core.<$> (x Core..?> "PoolInfos" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTapePools

instance Core.NFData ListTapePools

instance Core.ToHeaders ListTapePools where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListTapePools" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTapePools where
  toJSON ListTapePools' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("PoolARNs" Core..=) Core.<$> poolARNs,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListTapePools where
  toPath = Core.const "/"

instance Core.ToQuery ListTapePools where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTapePoolsResponse' smart constructor.
data ListTapePoolsResponse = ListTapePoolsResponse'
  { -- | An array of @PoolInfo@ objects, where each object describes a single
    -- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
    -- an empty array.
    poolInfos :: Core.Maybe [PoolInfo],
    -- | A string that indicates the position at which to begin the returned list
    -- of tape pools. Use the marker in your next request to continue
    -- pagination of tape pools. If there are no more tape pools to list, this
    -- element does not appear in the response body.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTapePoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolInfos', 'listTapePoolsResponse_poolInfos' - An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
--
-- 'marker', 'listTapePoolsResponse_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
--
-- 'httpStatus', 'listTapePoolsResponse_httpStatus' - The response's http status code.
newListTapePoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTapePoolsResponse
newListTapePoolsResponse pHttpStatus_ =
  ListTapePoolsResponse'
    { poolInfos = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
listTapePoolsResponse_poolInfos :: Lens.Lens' ListTapePoolsResponse (Core.Maybe [PoolInfo])
listTapePoolsResponse_poolInfos = Lens.lens (\ListTapePoolsResponse' {poolInfos} -> poolInfos) (\s@ListTapePoolsResponse' {} a -> s {poolInfos = a} :: ListTapePoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
listTapePoolsResponse_marker :: Lens.Lens' ListTapePoolsResponse (Core.Maybe Core.Text)
listTapePoolsResponse_marker = Lens.lens (\ListTapePoolsResponse' {marker} -> marker) (\s@ListTapePoolsResponse' {} a -> s {marker = a} :: ListTapePoolsResponse)

-- | The response's http status code.
listTapePoolsResponse_httpStatus :: Lens.Lens' ListTapePoolsResponse Core.Int
listTapePoolsResponse_httpStatus = Lens.lens (\ListTapePoolsResponse' {httpStatus} -> httpStatus) (\s@ListTapePoolsResponse' {} a -> s {httpStatus = a} :: ListTapePoolsResponse)

instance Core.NFData ListTapePoolsResponse

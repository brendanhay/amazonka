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
-- Module      : Network.AWS.StorageGateway.ListFileShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the file shares for a specific file gateway, or the list
-- of file shares that belong to the calling user account. This operation
-- is only supported for file gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListFileShares
  ( -- * Creating a Request
    ListFileShares (..),
    newListFileShares,

    -- * Request Lenses
    listFileShares_limit,
    listFileShares_gatewayARN,
    listFileShares_marker,

    -- * Destructuring the Response
    ListFileSharesResponse (..),
    newListFileSharesResponse,

    -- * Response Lenses
    listFileSharesResponse_nextMarker,
    listFileSharesResponse_fileShareInfoList,
    listFileSharesResponse_marker,
    listFileSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | ListFileShareInput
--
-- /See:/ 'newListFileShares' smart constructor.
data ListFileShares = ListFileShares'
  { -- | The maximum number of file shares to return in the response. The value
    -- must be an integer with a value greater than zero. Optional.
    limit :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the gateway whose file shares you want
    -- to list. If this field is not present, all file shares under your
    -- account are listed.
    gatewayARN :: Core.Maybe Core.Text,
    -- | Opaque pagination token returned from a previous ListFileShares
    -- operation. If present, @Marker@ specifies where to continue the list
    -- from after a previous call to ListFileShares. Optional.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFileShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listFileShares_limit' - The maximum number of file shares to return in the response. The value
-- must be an integer with a value greater than zero. Optional.
--
-- 'gatewayARN', 'listFileShares_gatewayARN' - The Amazon Resource Name (ARN) of the gateway whose file shares you want
-- to list. If this field is not present, all file shares under your
-- account are listed.
--
-- 'marker', 'listFileShares_marker' - Opaque pagination token returned from a previous ListFileShares
-- operation. If present, @Marker@ specifies where to continue the list
-- from after a previous call to ListFileShares. Optional.
newListFileShares ::
  ListFileShares
newListFileShares =
  ListFileShares'
    { limit = Core.Nothing,
      gatewayARN = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of file shares to return in the response. The value
-- must be an integer with a value greater than zero. Optional.
listFileShares_limit :: Lens.Lens' ListFileShares (Core.Maybe Core.Natural)
listFileShares_limit = Lens.lens (\ListFileShares' {limit} -> limit) (\s@ListFileShares' {} a -> s {limit = a} :: ListFileShares)

-- | The Amazon Resource Name (ARN) of the gateway whose file shares you want
-- to list. If this field is not present, all file shares under your
-- account are listed.
listFileShares_gatewayARN :: Lens.Lens' ListFileShares (Core.Maybe Core.Text)
listFileShares_gatewayARN = Lens.lens (\ListFileShares' {gatewayARN} -> gatewayARN) (\s@ListFileShares' {} a -> s {gatewayARN = a} :: ListFileShares)

-- | Opaque pagination token returned from a previous ListFileShares
-- operation. If present, @Marker@ specifies where to continue the list
-- from after a previous call to ListFileShares. Optional.
listFileShares_marker :: Lens.Lens' ListFileShares (Core.Maybe Core.Text)
listFileShares_marker = Lens.lens (\ListFileShares' {marker} -> marker) (\s@ListFileShares' {} a -> s {marker = a} :: ListFileShares)

instance Core.AWSPager ListFileShares where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFileSharesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFileSharesResponse_fileShareInfoList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFileShares_marker
          Lens..~ rs
          Lens.^? listFileSharesResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListFileShares where
  type
    AWSResponse ListFileShares =
      ListFileSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFileSharesResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "FileShareInfoList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFileShares

instance Core.NFData ListFileShares

instance Core.ToHeaders ListFileShares where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListFileShares" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListFileShares where
  toJSON ListFileShares' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("GatewayARN" Core..=) Core.<$> gatewayARN,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListFileShares where
  toPath = Core.const "/"

instance Core.ToQuery ListFileShares where
  toQuery = Core.const Core.mempty

-- | ListFileShareOutput
--
-- /See:/ 'newListFileSharesResponse' smart constructor.
data ListFileSharesResponse = ListFileSharesResponse'
  { -- | If a value is present, there are more file shares to return. In a
    -- subsequent request, use @NextMarker@ as the value for @Marker@ to
    -- retrieve the next set of file shares.
    nextMarker :: Core.Maybe Core.Text,
    -- | An array of information about the file gateway\'s file shares.
    fileShareInfoList :: Core.Maybe [FileShareInfo],
    -- | If the request includes @Marker@, the response returns that value in
    -- this field.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFileSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listFileSharesResponse_nextMarker' - If a value is present, there are more file shares to return. In a
-- subsequent request, use @NextMarker@ as the value for @Marker@ to
-- retrieve the next set of file shares.
--
-- 'fileShareInfoList', 'listFileSharesResponse_fileShareInfoList' - An array of information about the file gateway\'s file shares.
--
-- 'marker', 'listFileSharesResponse_marker' - If the request includes @Marker@, the response returns that value in
-- this field.
--
-- 'httpStatus', 'listFileSharesResponse_httpStatus' - The response's http status code.
newListFileSharesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFileSharesResponse
newListFileSharesResponse pHttpStatus_ =
  ListFileSharesResponse'
    { nextMarker = Core.Nothing,
      fileShareInfoList = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a value is present, there are more file shares to return. In a
-- subsequent request, use @NextMarker@ as the value for @Marker@ to
-- retrieve the next set of file shares.
listFileSharesResponse_nextMarker :: Lens.Lens' ListFileSharesResponse (Core.Maybe Core.Text)
listFileSharesResponse_nextMarker = Lens.lens (\ListFileSharesResponse' {nextMarker} -> nextMarker) (\s@ListFileSharesResponse' {} a -> s {nextMarker = a} :: ListFileSharesResponse)

-- | An array of information about the file gateway\'s file shares.
listFileSharesResponse_fileShareInfoList :: Lens.Lens' ListFileSharesResponse (Core.Maybe [FileShareInfo])
listFileSharesResponse_fileShareInfoList = Lens.lens (\ListFileSharesResponse' {fileShareInfoList} -> fileShareInfoList) (\s@ListFileSharesResponse' {} a -> s {fileShareInfoList = a} :: ListFileSharesResponse) Core.. Lens.mapping Lens._Coerce

-- | If the request includes @Marker@, the response returns that value in
-- this field.
listFileSharesResponse_marker :: Lens.Lens' ListFileSharesResponse (Core.Maybe Core.Text)
listFileSharesResponse_marker = Lens.lens (\ListFileSharesResponse' {marker} -> marker) (\s@ListFileSharesResponse' {} a -> s {marker = a} :: ListFileSharesResponse)

-- | The response's http status code.
listFileSharesResponse_httpStatus :: Lens.Lens' ListFileSharesResponse Core.Int
listFileSharesResponse_httpStatus = Lens.lens (\ListFileSharesResponse' {httpStatus} -> httpStatus) (\s@ListFileSharesResponse' {} a -> s {httpStatus = a} :: ListFileSharesResponse)

instance Core.NFData ListFileSharesResponse

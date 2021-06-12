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
-- Module      : Network.AWS.StorageGateway.ListTapes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists virtual tapes in your virtual tape library (VTL) and your virtual
-- tape shelf (VTS). You specify the tapes to list by specifying one or
-- more tape Amazon Resource Names (ARNs). If you don\'t specify a tape
-- ARN, the operation lists all virtual tapes in both your VTL and VTS.
--
-- This operation supports pagination. By default, the operation returns a
-- maximum of up to 100 tapes. You can optionally specify the @Limit@
-- parameter in the body to limit the number of tapes in the response. If
-- the number of tapes returned in the response is truncated, the response
-- includes a @Marker@ element that you can use in your subsequent request
-- to retrieve the next set of tapes. This operation is only supported in
-- the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapes
  ( -- * Creating a Request
    ListTapes (..),
    newListTapes,

    -- * Request Lenses
    listTapes_tapeARNs,
    listTapes_limit,
    listTapes_marker,

    -- * Destructuring the Response
    ListTapesResponse (..),
    newListTapesResponse,

    -- * Response Lenses
    listTapesResponse_tapeInfos,
    listTapesResponse_marker,
    listTapesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
-- -   ListTapesInput$Limit
--
-- -   ListTapesInput$Marker
--
-- -   ListTapesInput$TapeARNs
--
-- /See:/ 'newListTapes' smart constructor.
data ListTapes = ListTapes'
  { tapeARNs :: Core.Maybe [Core.Text],
    -- | An optional number limit for the tapes in the list returned by this
    -- call.
    limit :: Core.Maybe Core.Natural,
    -- | A string that indicates the position at which to begin the returned list
    -- of tapes.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARNs', 'listTapes_tapeARNs' - Undocumented member.
--
-- 'limit', 'listTapes_limit' - An optional number limit for the tapes in the list returned by this
-- call.
--
-- 'marker', 'listTapes_marker' - A string that indicates the position at which to begin the returned list
-- of tapes.
newListTapes ::
  ListTapes
newListTapes =
  ListTapes'
    { tapeARNs = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Undocumented member.
listTapes_tapeARNs :: Lens.Lens' ListTapes (Core.Maybe [Core.Text])
listTapes_tapeARNs = Lens.lens (\ListTapes' {tapeARNs} -> tapeARNs) (\s@ListTapes' {} a -> s {tapeARNs = a} :: ListTapes) Core.. Lens.mapping Lens._Coerce

-- | An optional number limit for the tapes in the list returned by this
-- call.
listTapes_limit :: Lens.Lens' ListTapes (Core.Maybe Core.Natural)
listTapes_limit = Lens.lens (\ListTapes' {limit} -> limit) (\s@ListTapes' {} a -> s {limit = a} :: ListTapes)

-- | A string that indicates the position at which to begin the returned list
-- of tapes.
listTapes_marker :: Lens.Lens' ListTapes (Core.Maybe Core.Text)
listTapes_marker = Lens.lens (\ListTapes' {marker} -> marker) (\s@ListTapes' {} a -> s {marker = a} :: ListTapes)

instance Core.AWSPager ListTapes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTapesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTapesResponse_tapeInfos Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTapes_marker
          Lens..~ rs Lens.^? listTapesResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListTapes where
  type AWSResponse ListTapes = ListTapesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapesResponse'
            Core.<$> (x Core..?> "TapeInfos" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTapes

instance Core.NFData ListTapes

instance Core.ToHeaders ListTapes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListTapes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTapes where
  toJSON ListTapes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TapeARNs" Core..=) Core.<$> tapeARNs,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListTapes where
  toPath = Core.const "/"

instance Core.ToQuery ListTapes where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- -   ListTapesOutput$Marker
--
-- -   ListTapesOutput$VolumeInfos
--
-- /See:/ 'newListTapesResponse' smart constructor.
data ListTapesResponse = ListTapesResponse'
  { tapeInfos :: Core.Maybe [TapeInfo],
    -- | A string that indicates the position at which to begin returning the
    -- next list of tapes. Use the marker in your next request to continue
    -- pagination of tapes. If there are no more tapes to list, this element
    -- does not appear in the response body.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTapesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeInfos', 'listTapesResponse_tapeInfos' - Undocumented member.
--
-- 'marker', 'listTapesResponse_marker' - A string that indicates the position at which to begin returning the
-- next list of tapes. Use the marker in your next request to continue
-- pagination of tapes. If there are no more tapes to list, this element
-- does not appear in the response body.
--
-- 'httpStatus', 'listTapesResponse_httpStatus' - The response's http status code.
newListTapesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTapesResponse
newListTapesResponse pHttpStatus_ =
  ListTapesResponse'
    { tapeInfos = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listTapesResponse_tapeInfos :: Lens.Lens' ListTapesResponse (Core.Maybe [TapeInfo])
listTapesResponse_tapeInfos = Lens.lens (\ListTapesResponse' {tapeInfos} -> tapeInfos) (\s@ListTapesResponse' {} a -> s {tapeInfos = a} :: ListTapesResponse) Core.. Lens.mapping Lens._Coerce

-- | A string that indicates the position at which to begin returning the
-- next list of tapes. Use the marker in your next request to continue
-- pagination of tapes. If there are no more tapes to list, this element
-- does not appear in the response body.
listTapesResponse_marker :: Lens.Lens' ListTapesResponse (Core.Maybe Core.Text)
listTapesResponse_marker = Lens.lens (\ListTapesResponse' {marker} -> marker) (\s@ListTapesResponse' {} a -> s {marker = a} :: ListTapesResponse)

-- | The response's http status code.
listTapesResponse_httpStatus :: Lens.Lens' ListTapesResponse Core.Int
listTapesResponse_httpStatus = Lens.lens (\ListTapesResponse' {httpStatus} -> httpStatus) (\s@ListTapesResponse' {} a -> s {httpStatus = a} :: ListTapesResponse)

instance Core.NFData ListTapesResponse

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
-- Module      : Network.AWS.IoT.ListOutgoingCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists certificates that are being transferred but not yet accepted.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOutgoingCertificates
  ( -- * Creating a Request
    ListOutgoingCertificates (..),
    newListOutgoingCertificates,

    -- * Request Lenses
    listOutgoingCertificates_pageSize,
    listOutgoingCertificates_ascendingOrder,
    listOutgoingCertificates_marker,

    -- * Destructuring the Response
    ListOutgoingCertificatesResponse (..),
    newListOutgoingCertificatesResponse,

    -- * Response Lenses
    listOutgoingCertificatesResponse_nextMarker,
    listOutgoingCertificatesResponse_outgoingCertificates,
    listOutgoingCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
  { -- | The result page size.
    pageSize :: Core.Maybe Core.Natural,
    -- | Specifies the order for results. If True, the results are returned in
    -- ascending order, based on the creation date.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOutgoingCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listOutgoingCertificates_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listOutgoingCertificates_ascendingOrder' - Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
--
-- 'marker', 'listOutgoingCertificates_marker' - The marker for the next set of results.
newListOutgoingCertificates ::
  ListOutgoingCertificates
newListOutgoingCertificates =
  ListOutgoingCertificates'
    { pageSize = Core.Nothing,
      ascendingOrder = Core.Nothing,
      marker = Core.Nothing
    }

-- | The result page size.
listOutgoingCertificates_pageSize :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Natural)
listOutgoingCertificates_pageSize = Lens.lens (\ListOutgoingCertificates' {pageSize} -> pageSize) (\s@ListOutgoingCertificates' {} a -> s {pageSize = a} :: ListOutgoingCertificates)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listOutgoingCertificates_ascendingOrder :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Bool)
listOutgoingCertificates_ascendingOrder = Lens.lens (\ListOutgoingCertificates' {ascendingOrder} -> ascendingOrder) (\s@ListOutgoingCertificates' {} a -> s {ascendingOrder = a} :: ListOutgoingCertificates)

-- | The marker for the next set of results.
listOutgoingCertificates_marker :: Lens.Lens' ListOutgoingCertificates (Core.Maybe Core.Text)
listOutgoingCertificates_marker = Lens.lens (\ListOutgoingCertificates' {marker} -> marker) (\s@ListOutgoingCertificates' {} a -> s {marker = a} :: ListOutgoingCertificates)

instance Core.AWSPager ListOutgoingCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_outgoingCertificates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOutgoingCertificates_marker
          Lens..~ rs
          Lens.^? listOutgoingCertificatesResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListOutgoingCertificates where
  type
    AWSResponse ListOutgoingCertificates =
      ListOutgoingCertificatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingCertificatesResponse'
            Core.<$> (x Core..?> "nextMarker")
            Core.<*> ( x Core..?> "outgoingCertificates"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOutgoingCertificates

instance Core.NFData ListOutgoingCertificates

instance Core.ToHeaders ListOutgoingCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListOutgoingCertificates where
  toPath = Core.const "/certificates-out-going"

instance Core.ToQuery ListOutgoingCertificates where
  toQuery ListOutgoingCertificates' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder,
        "marker" Core.=: marker
      ]

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { -- | The marker for the next set of results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The certificates that are being transferred but not yet accepted.
    outgoingCertificates :: Core.Maybe [OutgoingCertificate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOutgoingCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listOutgoingCertificatesResponse_nextMarker' - The marker for the next set of results.
--
-- 'outgoingCertificates', 'listOutgoingCertificatesResponse_outgoingCertificates' - The certificates that are being transferred but not yet accepted.
--
-- 'httpStatus', 'listOutgoingCertificatesResponse_httpStatus' - The response's http status code.
newListOutgoingCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOutgoingCertificatesResponse
newListOutgoingCertificatesResponse pHttpStatus_ =
  ListOutgoingCertificatesResponse'
    { nextMarker =
        Core.Nothing,
      outgoingCertificates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker for the next set of results.
listOutgoingCertificatesResponse_nextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe Core.Text)
listOutgoingCertificatesResponse_nextMarker = Lens.lens (\ListOutgoingCertificatesResponse' {nextMarker} -> nextMarker) (\s@ListOutgoingCertificatesResponse' {} a -> s {nextMarker = a} :: ListOutgoingCertificatesResponse)

-- | The certificates that are being transferred but not yet accepted.
listOutgoingCertificatesResponse_outgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Core.Maybe [OutgoingCertificate])
listOutgoingCertificatesResponse_outgoingCertificates = Lens.lens (\ListOutgoingCertificatesResponse' {outgoingCertificates} -> outgoingCertificates) (\s@ListOutgoingCertificatesResponse' {} a -> s {outgoingCertificates = a} :: ListOutgoingCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOutgoingCertificatesResponse_httpStatus :: Lens.Lens' ListOutgoingCertificatesResponse Core.Int
listOutgoingCertificatesResponse_httpStatus = Lens.lens (\ListOutgoingCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListOutgoingCertificatesResponse' {} a -> s {httpStatus = a} :: ListOutgoingCertificatesResponse)

instance Core.NFData ListOutgoingCertificatesResponse

{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
  { -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the order for results. If True, the results are returned in
    -- ascending order, based on the creation date.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { pageSize =
        Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The result page size.
listOutgoingCertificates_pageSize :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Natural)
listOutgoingCertificates_pageSize = Lens.lens (\ListOutgoingCertificates' {pageSize} -> pageSize) (\s@ListOutgoingCertificates' {} a -> s {pageSize = a} :: ListOutgoingCertificates)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listOutgoingCertificates_ascendingOrder :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Bool)
listOutgoingCertificates_ascendingOrder = Lens.lens (\ListOutgoingCertificates' {ascendingOrder} -> ascendingOrder) (\s@ListOutgoingCertificates' {} a -> s {ascendingOrder = a} :: ListOutgoingCertificates)

-- | The marker for the next set of results.
listOutgoingCertificates_marker :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Text)
listOutgoingCertificates_marker = Lens.lens (\ListOutgoingCertificates' {marker} -> marker) (\s@ListOutgoingCertificates' {} a -> s {marker = a} :: ListOutgoingCertificates)

instance Pager.AWSPager ListOutgoingCertificates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_outgoingCertificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listOutgoingCertificates_marker
          Lens..~ rs
          Lens.^? listOutgoingCertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListOutgoingCertificates where
  type
    Rs ListOutgoingCertificates =
      ListOutgoingCertificatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingCertificatesResponse'
            Prelude.<$> (x Prelude..?> "nextMarker")
            Prelude.<*> ( x Prelude..?> "outgoingCertificates"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOutgoingCertificates

instance Prelude.NFData ListOutgoingCertificates

instance Prelude.ToHeaders ListOutgoingCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListOutgoingCertificates where
  toPath = Prelude.const "/certificates-out-going"

instance Prelude.ToQuery ListOutgoingCertificates where
  toQuery ListOutgoingCertificates' {..} =
    Prelude.mconcat
      [ "pageSize" Prelude.=: pageSize,
        "isAscendingOrder" Prelude.=: ascendingOrder,
        "marker" Prelude.=: marker
      ]

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { -- | The marker for the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The certificates that are being transferred but not yet accepted.
    outgoingCertificates :: Prelude.Maybe [OutgoingCertificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListOutgoingCertificatesResponse
newListOutgoingCertificatesResponse pHttpStatus_ =
  ListOutgoingCertificatesResponse'
    { nextMarker =
        Prelude.Nothing,
      outgoingCertificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker for the next set of results.
listOutgoingCertificatesResponse_nextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Prelude.Maybe Prelude.Text)
listOutgoingCertificatesResponse_nextMarker = Lens.lens (\ListOutgoingCertificatesResponse' {nextMarker} -> nextMarker) (\s@ListOutgoingCertificatesResponse' {} a -> s {nextMarker = a} :: ListOutgoingCertificatesResponse)

-- | The certificates that are being transferred but not yet accepted.
listOutgoingCertificatesResponse_outgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Prelude.Maybe [OutgoingCertificate])
listOutgoingCertificatesResponse_outgoingCertificates = Lens.lens (\ListOutgoingCertificatesResponse' {outgoingCertificates} -> outgoingCertificates) (\s@ListOutgoingCertificatesResponse' {} a -> s {outgoingCertificates = a} :: ListOutgoingCertificatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listOutgoingCertificatesResponse_httpStatus :: Lens.Lens' ListOutgoingCertificatesResponse Prelude.Int
listOutgoingCertificatesResponse_httpStatus = Lens.lens (\ListOutgoingCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListOutgoingCertificatesResponse' {} a -> s {httpStatus = a} :: ListOutgoingCertificatesResponse)

instance
  Prelude.NFData
    ListOutgoingCertificatesResponse

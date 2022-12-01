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
-- Module      : Amazonka.IoT.ListOutgoingCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists certificates that are being transferred but not yet accepted.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListOutgoingCertificates>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListOutgoingCertificates
  ( -- * Creating a Request
    ListOutgoingCertificates (..),
    newListOutgoingCertificates,

    -- * Request Lenses
    listOutgoingCertificates_marker,
    listOutgoingCertificates_pageSize,
    listOutgoingCertificates_ascendingOrder,

    -- * Destructuring the Response
    ListOutgoingCertificatesResponse (..),
    newListOutgoingCertificatesResponse,

    -- * Response Lenses
    listOutgoingCertificatesResponse_outgoingCertificates,
    listOutgoingCertificatesResponse_nextMarker,
    listOutgoingCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificates' smart constructor.
data ListOutgoingCertificates = ListOutgoingCertificates'
  { -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the order for results. If True, the results are returned in
    -- ascending order, based on the creation date.
    ascendingOrder :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutgoingCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listOutgoingCertificates_marker' - The marker for the next set of results.
--
-- 'pageSize', 'listOutgoingCertificates_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listOutgoingCertificates_ascendingOrder' - Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
newListOutgoingCertificates ::
  ListOutgoingCertificates
newListOutgoingCertificates =
  ListOutgoingCertificates'
    { marker = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | The marker for the next set of results.
listOutgoingCertificates_marker :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Text)
listOutgoingCertificates_marker = Lens.lens (\ListOutgoingCertificates' {marker} -> marker) (\s@ListOutgoingCertificates' {} a -> s {marker = a} :: ListOutgoingCertificates)

-- | The result page size.
listOutgoingCertificates_pageSize :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Natural)
listOutgoingCertificates_pageSize = Lens.lens (\ListOutgoingCertificates' {pageSize} -> pageSize) (\s@ListOutgoingCertificates' {} a -> s {pageSize = a} :: ListOutgoingCertificates)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listOutgoingCertificates_ascendingOrder :: Lens.Lens' ListOutgoingCertificates (Prelude.Maybe Prelude.Bool)
listOutgoingCertificates_ascendingOrder = Lens.lens (\ListOutgoingCertificates' {ascendingOrder} -> ascendingOrder) (\s@ListOutgoingCertificates' {} a -> s {ascendingOrder = a} :: ListOutgoingCertificates)

instance Core.AWSPager ListOutgoingCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOutgoingCertificatesResponse_outgoingCertificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOutgoingCertificates_marker
          Lens..~ rs
          Lens.^? listOutgoingCertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListOutgoingCertificates where
  type
    AWSResponse ListOutgoingCertificates =
      ListOutgoingCertificatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingCertificatesResponse'
            Prelude.<$> ( x Core..?> "outgoingCertificates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOutgoingCertificates where
  hashWithSalt _salt ListOutgoingCertificates' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` ascendingOrder

instance Prelude.NFData ListOutgoingCertificates where
  rnf ListOutgoingCertificates' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf ascendingOrder

instance Core.ToHeaders ListOutgoingCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListOutgoingCertificates where
  toPath = Prelude.const "/certificates-out-going"

instance Core.ToQuery ListOutgoingCertificates where
  toQuery ListOutgoingCertificates' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder
      ]

-- | The output from the ListOutgoingCertificates operation.
--
-- /See:/ 'newListOutgoingCertificatesResponse' smart constructor.
data ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse'
  { -- | The certificates that are being transferred but not yet accepted.
    outgoingCertificates :: Prelude.Maybe [OutgoingCertificate],
    -- | The marker for the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutgoingCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outgoingCertificates', 'listOutgoingCertificatesResponse_outgoingCertificates' - The certificates that are being transferred but not yet accepted.
--
-- 'nextMarker', 'listOutgoingCertificatesResponse_nextMarker' - The marker for the next set of results.
--
-- 'httpStatus', 'listOutgoingCertificatesResponse_httpStatus' - The response's http status code.
newListOutgoingCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOutgoingCertificatesResponse
newListOutgoingCertificatesResponse pHttpStatus_ =
  ListOutgoingCertificatesResponse'
    { outgoingCertificates =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificates that are being transferred but not yet accepted.
listOutgoingCertificatesResponse_outgoingCertificates :: Lens.Lens' ListOutgoingCertificatesResponse (Prelude.Maybe [OutgoingCertificate])
listOutgoingCertificatesResponse_outgoingCertificates = Lens.lens (\ListOutgoingCertificatesResponse' {outgoingCertificates} -> outgoingCertificates) (\s@ListOutgoingCertificatesResponse' {} a -> s {outgoingCertificates = a} :: ListOutgoingCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the next set of results.
listOutgoingCertificatesResponse_nextMarker :: Lens.Lens' ListOutgoingCertificatesResponse (Prelude.Maybe Prelude.Text)
listOutgoingCertificatesResponse_nextMarker = Lens.lens (\ListOutgoingCertificatesResponse' {nextMarker} -> nextMarker) (\s@ListOutgoingCertificatesResponse' {} a -> s {nextMarker = a} :: ListOutgoingCertificatesResponse)

-- | The response's http status code.
listOutgoingCertificatesResponse_httpStatus :: Lens.Lens' ListOutgoingCertificatesResponse Prelude.Int
listOutgoingCertificatesResponse_httpStatus = Lens.lens (\ListOutgoingCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListOutgoingCertificatesResponse' {} a -> s {httpStatus = a} :: ListOutgoingCertificatesResponse)

instance
  Prelude.NFData
    ListOutgoingCertificatesResponse
  where
  rnf ListOutgoingCertificatesResponse' {..} =
    Prelude.rnf outgoingCertificates
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus

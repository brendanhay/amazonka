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
-- Module      : Amazonka.IoT.ListCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the certificates registered in your Amazon Web Services account.
--
-- The results are paginated with a default page size of 25. You can use
-- the returned marker to retrieve additional results.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListCertificates>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_marker,
    listCertificates_pageSize,
    listCertificates_ascendingOrder,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_certificates,
    listCertificatesResponse_nextMarker,
    listCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListCertificates operation.
--
-- /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
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
-- Create a value of 'ListCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listCertificates_marker' - The marker for the next set of results.
--
-- 'pageSize', 'listCertificates_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listCertificates_ascendingOrder' - Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { marker = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | The marker for the next set of results.
listCertificates_marker :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_marker = Lens.lens (\ListCertificates' {marker} -> marker) (\s@ListCertificates' {} a -> s {marker = a} :: ListCertificates)

-- | The result page size.
listCertificates_pageSize :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_pageSize = Lens.lens (\ListCertificates' {pageSize} -> pageSize) (\s@ListCertificates' {} a -> s {pageSize = a} :: ListCertificates)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listCertificates_ascendingOrder :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Bool)
listCertificates_ascendingOrder = Lens.lens (\ListCertificates' {ascendingOrder} -> ascendingOrder) (\s@ListCertificates' {} a -> s {ascendingOrder = a} :: ListCertificates)

instance Core.AWSPager ListCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCertificates_marker
          Lens..~ rs
          Lens.^? listCertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListCertificates where
  type
    AWSResponse ListCertificates =
      ListCertificatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Prelude.<$> (x Core..?> "certificates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates where
  hashWithSalt _salt ListCertificates' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` ascendingOrder

instance Prelude.NFData ListCertificates where
  rnf ListCertificates' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf ascendingOrder

instance Core.ToHeaders ListCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListCertificates where
  toPath = Prelude.const "/certificates"

instance Core.ToQuery ListCertificates where
  toQuery ListCertificates' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder
      ]

-- | The output of the ListCertificates operation.
--
-- /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | The descriptions of the certificates.
    certificates :: Prelude.Maybe [Certificate],
    -- | The marker for the next set of results, or null if there are no
    -- additional results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'listCertificatesResponse_certificates' - The descriptions of the certificates.
--
-- 'nextMarker', 'listCertificatesResponse_nextMarker' - The marker for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { certificates =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The descriptions of the certificates.
listCertificatesResponse_certificates :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [Certificate])
listCertificatesResponse_certificates = Lens.lens (\ListCertificatesResponse' {certificates} -> certificates) (\s@ListCertificatesResponse' {} a -> s {certificates = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the next set of results, or null if there are no
-- additional results.
listCertificatesResponse_nextMarker :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextMarker = Lens.lens (\ListCertificatesResponse' {nextMarker} -> nextMarker) (\s@ListCertificatesResponse' {} a -> s {nextMarker = a} :: ListCertificatesResponse)

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse where
  rnf ListCertificatesResponse' {..} =
    Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.IoT.ListCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the certificates registered in your AWS account.
--
-- The results are paginated with a default page size of 25. You can use
-- the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_pageSize,
    listCertificates_ascendingOrder,
    listCertificates_marker,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_nextMarker,
    listCertificatesResponse_certificates,
    listCertificatesResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListCertificates operation.
--
-- /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
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
-- Create a value of 'ListCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listCertificates_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listCertificates_ascendingOrder' - Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
--
-- 'marker', 'listCertificates_marker' - The marker for the next set of results.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The result page size.
listCertificates_pageSize :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_pageSize = Lens.lens (\ListCertificates' {pageSize} -> pageSize) (\s@ListCertificates' {} a -> s {pageSize = a} :: ListCertificates)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listCertificates_ascendingOrder :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Bool)
listCertificates_ascendingOrder = Lens.lens (\ListCertificates' {ascendingOrder} -> ascendingOrder) (\s@ListCertificates' {} a -> s {ascendingOrder = a} :: ListCertificates)

-- | The marker for the next set of results.
listCertificates_marker :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_marker = Lens.lens (\ListCertificates' {marker} -> marker) (\s@ListCertificates' {} a -> s {marker = a} :: ListCertificates)

instance Pager.AWSPager ListCertificates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listCertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listCertificates_marker
          Lens..~ rs
          Lens.^? listCertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Prelude.<$> (x Prelude..?> "nextMarker")
            Prelude.<*> ( x Prelude..?> "certificates"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates

instance Prelude.NFData ListCertificates

instance Prelude.ToHeaders ListCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListCertificates where
  toPath = Prelude.const "/certificates"

instance Prelude.ToQuery ListCertificates where
  toQuery ListCertificates' {..} =
    Prelude.mconcat
      [ "pageSize" Prelude.=: pageSize,
        "isAscendingOrder" Prelude.=: ascendingOrder,
        "marker" Prelude.=: marker
      ]

-- | The output of the ListCertificates operation.
--
-- /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | The marker for the next set of results, or null if there are no
    -- additional results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The descriptions of the certificates.
    certificates :: Prelude.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listCertificatesResponse_nextMarker' - The marker for the next set of results, or null if there are no
-- additional results.
--
-- 'certificates', 'listCertificatesResponse_certificates' - The descriptions of the certificates.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { nextMarker =
        Prelude.Nothing,
      certificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker for the next set of results, or null if there are no
-- additional results.
listCertificatesResponse_nextMarker :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextMarker = Lens.lens (\ListCertificatesResponse' {nextMarker} -> nextMarker) (\s@ListCertificatesResponse' {} a -> s {nextMarker = a} :: ListCertificatesResponse)

-- | The descriptions of the certificates.
listCertificatesResponse_certificates :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [Certificate])
listCertificatesResponse_certificates = Lens.lens (\ListCertificatesResponse' {certificates} -> certificates) (\s@ListCertificatesResponse' {} a -> s {certificates = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse

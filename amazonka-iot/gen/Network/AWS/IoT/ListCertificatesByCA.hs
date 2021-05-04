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
-- Module      : Network.AWS.IoT.ListCertificatesByCA
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the device certificates signed by the specified CA certificate.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCertificatesByCA
  ( -- * Creating a Request
    ListCertificatesByCA (..),
    newListCertificatesByCA,

    -- * Request Lenses
    listCertificatesByCA_pageSize,
    listCertificatesByCA_ascendingOrder,
    listCertificatesByCA_marker,
    listCertificatesByCA_caCertificateId,

    -- * Destructuring the Response
    ListCertificatesByCAResponse (..),
    newListCertificatesByCAResponse,

    -- * Response Lenses
    listCertificatesByCAResponse_nextMarker,
    listCertificatesByCAResponse_certificates,
    listCertificatesByCAResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the ListCertificatesByCA operation.
--
-- /See:/ 'newListCertificatesByCA' smart constructor.
data ListCertificatesByCA = ListCertificatesByCA'
  { -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the order for results. If True, the results are returned in
    -- ascending order, based on the creation date.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CA certificate. This operation will list all registered
    -- device certificate that were signed by this CA certificate.
    caCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesByCA' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listCertificatesByCA_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listCertificatesByCA_ascendingOrder' - Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
--
-- 'marker', 'listCertificatesByCA_marker' - The marker for the next set of results.
--
-- 'caCertificateId', 'listCertificatesByCA_caCertificateId' - The ID of the CA certificate. This operation will list all registered
-- device certificate that were signed by this CA certificate.
newListCertificatesByCA ::
  -- | 'caCertificateId'
  Prelude.Text ->
  ListCertificatesByCA
newListCertificatesByCA pCaCertificateId_ =
  ListCertificatesByCA'
    { pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing,
      caCertificateId = pCaCertificateId_
    }

-- | The result page size.
listCertificatesByCA_pageSize :: Lens.Lens' ListCertificatesByCA (Prelude.Maybe Prelude.Natural)
listCertificatesByCA_pageSize = Lens.lens (\ListCertificatesByCA' {pageSize} -> pageSize) (\s@ListCertificatesByCA' {} a -> s {pageSize = a} :: ListCertificatesByCA)

-- | Specifies the order for results. If True, the results are returned in
-- ascending order, based on the creation date.
listCertificatesByCA_ascendingOrder :: Lens.Lens' ListCertificatesByCA (Prelude.Maybe Prelude.Bool)
listCertificatesByCA_ascendingOrder = Lens.lens (\ListCertificatesByCA' {ascendingOrder} -> ascendingOrder) (\s@ListCertificatesByCA' {} a -> s {ascendingOrder = a} :: ListCertificatesByCA)

-- | The marker for the next set of results.
listCertificatesByCA_marker :: Lens.Lens' ListCertificatesByCA (Prelude.Maybe Prelude.Text)
listCertificatesByCA_marker = Lens.lens (\ListCertificatesByCA' {marker} -> marker) (\s@ListCertificatesByCA' {} a -> s {marker = a} :: ListCertificatesByCA)

-- | The ID of the CA certificate. This operation will list all registered
-- device certificate that were signed by this CA certificate.
listCertificatesByCA_caCertificateId :: Lens.Lens' ListCertificatesByCA Prelude.Text
listCertificatesByCA_caCertificateId = Lens.lens (\ListCertificatesByCA' {caCertificateId} -> caCertificateId) (\s@ListCertificatesByCA' {} a -> s {caCertificateId = a} :: ListCertificatesByCA)

instance Pager.AWSPager ListCertificatesByCA where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listCertificatesByCAResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCertificatesByCAResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listCertificatesByCA_marker
          Lens..~ rs
          Lens.^? listCertificatesByCAResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListCertificatesByCA where
  type
    Rs ListCertificatesByCA =
      ListCertificatesByCAResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesByCAResponse'
            Prelude.<$> (x Prelude..?> "nextMarker")
            Prelude.<*> ( x Prelude..?> "certificates"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificatesByCA

instance Prelude.NFData ListCertificatesByCA

instance Prelude.ToHeaders ListCertificatesByCA where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListCertificatesByCA where
  toPath ListCertificatesByCA' {..} =
    Prelude.mconcat
      [ "/certificates-by-ca/",
        Prelude.toBS caCertificateId
      ]

instance Prelude.ToQuery ListCertificatesByCA where
  toQuery ListCertificatesByCA' {..} =
    Prelude.mconcat
      [ "pageSize" Prelude.=: pageSize,
        "isAscendingOrder" Prelude.=: ascendingOrder,
        "marker" Prelude.=: marker
      ]

-- | The output of the ListCertificatesByCA operation.
--
-- /See:/ 'newListCertificatesByCAResponse' smart constructor.
data ListCertificatesByCAResponse = ListCertificatesByCAResponse'
  { -- | The marker for the next set of results, or null if there are no
    -- additional results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The device certificates signed by the specified CA certificate.
    certificates :: Prelude.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesByCAResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listCertificatesByCAResponse_nextMarker' - The marker for the next set of results, or null if there are no
-- additional results.
--
-- 'certificates', 'listCertificatesByCAResponse_certificates' - The device certificates signed by the specified CA certificate.
--
-- 'httpStatus', 'listCertificatesByCAResponse_httpStatus' - The response's http status code.
newListCertificatesByCAResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesByCAResponse
newListCertificatesByCAResponse pHttpStatus_ =
  ListCertificatesByCAResponse'
    { nextMarker =
        Prelude.Nothing,
      certificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker for the next set of results, or null if there are no
-- additional results.
listCertificatesByCAResponse_nextMarker :: Lens.Lens' ListCertificatesByCAResponse (Prelude.Maybe Prelude.Text)
listCertificatesByCAResponse_nextMarker = Lens.lens (\ListCertificatesByCAResponse' {nextMarker} -> nextMarker) (\s@ListCertificatesByCAResponse' {} a -> s {nextMarker = a} :: ListCertificatesByCAResponse)

-- | The device certificates signed by the specified CA certificate.
listCertificatesByCAResponse_certificates :: Lens.Lens' ListCertificatesByCAResponse (Prelude.Maybe [Certificate])
listCertificatesByCAResponse_certificates = Lens.lens (\ListCertificatesByCAResponse' {certificates} -> certificates) (\s@ListCertificatesByCAResponse' {} a -> s {certificates = a} :: ListCertificatesByCAResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCertificatesByCAResponse_httpStatus :: Lens.Lens' ListCertificatesByCAResponse Prelude.Int
listCertificatesByCAResponse_httpStatus = Lens.lens (\ListCertificatesByCAResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesByCAResponse' {} a -> s {httpStatus = a} :: ListCertificatesByCAResponse)

instance Prelude.NFData ListCertificatesByCAResponse

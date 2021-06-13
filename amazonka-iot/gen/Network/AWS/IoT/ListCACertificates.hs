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
-- Module      : Network.AWS.IoT.ListCACertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the CA certificates registered for your AWS account.
--
-- The results are paginated with a default page size of 25. You can use
-- the returned marker to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCACertificates
  ( -- * Creating a Request
    ListCACertificates (..),
    newListCACertificates,

    -- * Request Lenses
    listCACertificates_pageSize,
    listCACertificates_ascendingOrder,
    listCACertificates_marker,

    -- * Destructuring the Response
    ListCACertificatesResponse (..),
    newListCACertificatesResponse,

    -- * Response Lenses
    listCACertificatesResponse_nextMarker,
    listCACertificatesResponse_certificates,
    listCACertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for the ListCACertificates operation.
--
-- /See:/ 'newListCACertificates' smart constructor.
data ListCACertificates = ListCACertificates'
  { -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Determines the order of the results.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCACertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listCACertificates_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listCACertificates_ascendingOrder' - Determines the order of the results.
--
-- 'marker', 'listCACertificates_marker' - The marker for the next set of results.
newListCACertificates ::
  ListCACertificates
newListCACertificates =
  ListCACertificates'
    { pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The result page size.
listCACertificates_pageSize :: Lens.Lens' ListCACertificates (Prelude.Maybe Prelude.Natural)
listCACertificates_pageSize = Lens.lens (\ListCACertificates' {pageSize} -> pageSize) (\s@ListCACertificates' {} a -> s {pageSize = a} :: ListCACertificates)

-- | Determines the order of the results.
listCACertificates_ascendingOrder :: Lens.Lens' ListCACertificates (Prelude.Maybe Prelude.Bool)
listCACertificates_ascendingOrder = Lens.lens (\ListCACertificates' {ascendingOrder} -> ascendingOrder) (\s@ListCACertificates' {} a -> s {ascendingOrder = a} :: ListCACertificates)

-- | The marker for the next set of results.
listCACertificates_marker :: Lens.Lens' ListCACertificates (Prelude.Maybe Prelude.Text)
listCACertificates_marker = Lens.lens (\ListCACertificates' {marker} -> marker) (\s@ListCACertificates' {} a -> s {marker = a} :: ListCACertificates)

instance Core.AWSPager ListCACertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCACertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCACertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCACertificates_marker
          Lens..~ rs
          Lens.^? listCACertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListCACertificates where
  type
    AWSResponse ListCACertificates =
      ListCACertificatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCACertificatesResponse'
            Prelude.<$> (x Core..?> "nextMarker")
            Prelude.<*> (x Core..?> "certificates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCACertificates

instance Prelude.NFData ListCACertificates

instance Core.ToHeaders ListCACertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListCACertificates where
  toPath = Prelude.const "/cacertificates"

instance Core.ToQuery ListCACertificates where
  toQuery ListCACertificates' {..} =
    Prelude.mconcat
      [ "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder,
        "marker" Core.=: marker
      ]

-- | The output from the ListCACertificates operation.
--
-- /See:/ 'newListCACertificatesResponse' smart constructor.
data ListCACertificatesResponse = ListCACertificatesResponse'
  { -- | The current position within the list of CA certificates.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The CA certificates registered in your AWS account.
    certificates :: Prelude.Maybe [CACertificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCACertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listCACertificatesResponse_nextMarker' - The current position within the list of CA certificates.
--
-- 'certificates', 'listCACertificatesResponse_certificates' - The CA certificates registered in your AWS account.
--
-- 'httpStatus', 'listCACertificatesResponse_httpStatus' - The response's http status code.
newListCACertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCACertificatesResponse
newListCACertificatesResponse pHttpStatus_ =
  ListCACertificatesResponse'
    { nextMarker =
        Prelude.Nothing,
      certificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current position within the list of CA certificates.
listCACertificatesResponse_nextMarker :: Lens.Lens' ListCACertificatesResponse (Prelude.Maybe Prelude.Text)
listCACertificatesResponse_nextMarker = Lens.lens (\ListCACertificatesResponse' {nextMarker} -> nextMarker) (\s@ListCACertificatesResponse' {} a -> s {nextMarker = a} :: ListCACertificatesResponse)

-- | The CA certificates registered in your AWS account.
listCACertificatesResponse_certificates :: Lens.Lens' ListCACertificatesResponse (Prelude.Maybe [CACertificate])
listCACertificatesResponse_certificates = Lens.lens (\ListCACertificatesResponse' {certificates} -> certificates) (\s@ListCACertificatesResponse' {} a -> s {certificates = a} :: ListCACertificatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCACertificatesResponse_httpStatus :: Lens.Lens' ListCACertificatesResponse Prelude.Int
listCACertificatesResponse_httpStatus = Lens.lens (\ListCACertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCACertificatesResponse' {} a -> s {httpStatus = a} :: ListCACertificatesResponse)

instance Prelude.NFData ListCACertificatesResponse

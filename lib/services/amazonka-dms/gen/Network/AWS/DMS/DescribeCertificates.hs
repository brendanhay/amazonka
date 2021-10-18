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
-- Module      : Network.AWS.DMS.DescribeCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the certificate.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeCertificates
  ( -- * Creating a Request
    DescribeCertificates (..),
    newDescribeCertificates,

    -- * Request Lenses
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_marker,

    -- * Destructuring the Response
    DescribeCertificatesResponse (..),
    newDescribeCertificatesResponse,

    -- * Response Lenses
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | Filters applied to the certificates described in the form of key-value
    -- pairs.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 10
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeCertificates_filters' - Filters applied to the certificates described in the form of key-value
-- pairs.
--
-- 'maxRecords', 'describeCertificates_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 10
--
-- 'marker', 'describeCertificates_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
newDescribeCertificates ::
  DescribeCertificates
newDescribeCertificates =
  DescribeCertificates'
    { filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Filters applied to the certificates described in the form of key-value
-- pairs.
describeCertificates_filters :: Lens.Lens' DescribeCertificates (Prelude.Maybe [Filter])
describeCertificates_filters = Lens.lens (\DescribeCertificates' {filters} -> filters) (\s@DescribeCertificates' {} a -> s {filters = a} :: DescribeCertificates) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 10
describeCertificates_maxRecords :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Int)
describeCertificates_maxRecords = Lens.lens (\DescribeCertificates' {maxRecords} -> maxRecords) (\s@DescribeCertificates' {} a -> s {maxRecords = a} :: DescribeCertificates)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeCertificates_marker :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_marker = Lens.lens (\DescribeCertificates' {marker} -> marker) (\s@DescribeCertificates' {} a -> s {marker = a} :: DescribeCertificates)

instance Core.AWSPager DescribeCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCertificatesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCertificates_marker
          Lens..~ rs
          Lens.^? describeCertificatesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCertificates where
  type
    AWSResponse DescribeCertificates =
      DescribeCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificatesResponse'
            Prelude.<$> (x Core..?> "Certificates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificates

instance Prelude.NFData DescribeCertificates

instance Core.ToHeaders DescribeCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCertificates where
  toJSON DescribeCertificates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords,
            ("Marker" Core..=) Prelude.<$> marker
          ]
      )

instance Core.ToPath DescribeCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | The Secure Sockets Layer (SSL) certificates associated with the
    -- replication instance.
    certificates :: Prelude.Maybe [Certificate],
    -- | The pagination token.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'describeCertificatesResponse_certificates' - The Secure Sockets Layer (SSL) certificates associated with the
-- replication instance.
--
-- 'marker', 'describeCertificatesResponse_marker' - The pagination token.
--
-- 'httpStatus', 'describeCertificatesResponse_httpStatus' - The response's http status code.
newDescribeCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCertificatesResponse
newDescribeCertificatesResponse pHttpStatus_ =
  DescribeCertificatesResponse'
    { certificates =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Secure Sockets Layer (SSL) certificates associated with the
-- replication instance.
describeCertificatesResponse_certificates :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe [Certificate])
describeCertificatesResponse_certificates = Lens.lens (\DescribeCertificatesResponse' {certificates} -> certificates) (\s@DescribeCertificatesResponse' {} a -> s {certificates = a} :: DescribeCertificatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token.
describeCertificatesResponse_marker :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe Prelude.Text)
describeCertificatesResponse_marker = Lens.lens (\DescribeCertificatesResponse' {marker} -> marker) (\s@DescribeCertificatesResponse' {} a -> s {marker = a} :: DescribeCertificatesResponse)

-- | The response's http status code.
describeCertificatesResponse_httpStatus :: Lens.Lens' DescribeCertificatesResponse Prelude.Int
describeCertificatesResponse_httpStatus = Lens.lens (\DescribeCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeCertificatesResponse)

instance Prelude.NFData DescribeCertificatesResponse

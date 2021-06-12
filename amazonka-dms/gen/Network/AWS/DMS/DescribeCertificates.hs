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
    describeCertificates_marker,
    describeCertificates_maxRecords,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | Filters applied to the certificates described in the form of key-value
    -- pairs.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 10
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'marker', 'describeCertificates_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCertificates_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 10
newDescribeCertificates ::
  DescribeCertificates
newDescribeCertificates =
  DescribeCertificates'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to the certificates described in the form of key-value
-- pairs.
describeCertificates_filters :: Lens.Lens' DescribeCertificates (Core.Maybe [Filter])
describeCertificates_filters = Lens.lens (\DescribeCertificates' {filters} -> filters) (\s@DescribeCertificates' {} a -> s {filters = a} :: DescribeCertificates) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeCertificates_marker :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Text)
describeCertificates_marker = Lens.lens (\DescribeCertificates' {marker} -> marker) (\s@DescribeCertificates' {} a -> s {marker = a} :: DescribeCertificates)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 10
describeCertificates_maxRecords :: Lens.Lens' DescribeCertificates (Core.Maybe Core.Int)
describeCertificates_maxRecords = Lens.lens (\DescribeCertificates' {maxRecords} -> maxRecords) (\s@DescribeCertificates' {} a -> s {maxRecords = a} :: DescribeCertificates)

instance Core.AWSPager DescribeCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCertificatesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCertificatesResponse_certificates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCertificates_marker
          Lens..~ rs
          Lens.^? describeCertificatesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeCertificates where
  type
    AWSResponse DescribeCertificates =
      DescribeCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificatesResponse'
            Core.<$> (x Core..?> "Certificates" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCertificates

instance Core.NFData DescribeCertificates

instance Core.ToHeaders DescribeCertificates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeCertificates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCertificates where
  toJSON DescribeCertificates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeCertificates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCertificates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | The Secure Sockets Layer (SSL) certificates associated with the
    -- replication instance.
    certificates :: Core.Maybe [Certificate],
    -- | The pagination token.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeCertificatesResponse
newDescribeCertificatesResponse pHttpStatus_ =
  DescribeCertificatesResponse'
    { certificates =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Secure Sockets Layer (SSL) certificates associated with the
-- replication instance.
describeCertificatesResponse_certificates :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe [Certificate])
describeCertificatesResponse_certificates = Lens.lens (\DescribeCertificatesResponse' {certificates} -> certificates) (\s@DescribeCertificatesResponse' {} a -> s {certificates = a} :: DescribeCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
describeCertificatesResponse_marker :: Lens.Lens' DescribeCertificatesResponse (Core.Maybe Core.Text)
describeCertificatesResponse_marker = Lens.lens (\DescribeCertificatesResponse' {marker} -> marker) (\s@DescribeCertificatesResponse' {} a -> s {marker = a} :: DescribeCertificatesResponse)

-- | The response's http status code.
describeCertificatesResponse_httpStatus :: Lens.Lens' DescribeCertificatesResponse Core.Int
describeCertificatesResponse_httpStatus = Lens.lens (\DescribeCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeCertificatesResponse)

instance Core.NFData DescribeCertificatesResponse

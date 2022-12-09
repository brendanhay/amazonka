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
-- Module      : Amazonka.DMS.DescribeCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the certificate.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeCertificates
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | Filters applied to the certificates described in the form of key-value
    -- pairs. Valid values are @certificate-arn@ and @certificate-id@.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 10
    maxRecords :: Prelude.Maybe Prelude.Int
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
-- pairs. Valid values are @certificate-arn@ and @certificate-id@.
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
    { filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to the certificates described in the form of key-value
-- pairs. Valid values are @certificate-arn@ and @certificate-id@.
describeCertificates_filters :: Lens.Lens' DescribeCertificates (Prelude.Maybe [Filter])
describeCertificates_filters = Lens.lens (\DescribeCertificates' {filters} -> filters) (\s@DescribeCertificates' {} a -> s {filters = a} :: DescribeCertificates) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeCertificates_marker :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_marker = Lens.lens (\DescribeCertificates' {marker} -> marker) (\s@DescribeCertificates' {} a -> s {marker = a} :: DescribeCertificates)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 10
describeCertificates_maxRecords :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Int)
describeCertificates_maxRecords = Lens.lens (\DescribeCertificates' {maxRecords} -> maxRecords) (\s@DescribeCertificates' {} a -> s {maxRecords = a} :: DescribeCertificates)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificatesResponse'
            Prelude.<$> (x Data..?> "Certificates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificates where
  hashWithSalt _salt DescribeCertificates' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeCertificates where
  rnf DescribeCertificates' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCertificates where
  toJSON DescribeCertificates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance Data.ToPath DescribeCertificates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCertificates where
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
describeCertificatesResponse_certificates = Lens.lens (\DescribeCertificatesResponse' {certificates} -> certificates) (\s@DescribeCertificatesResponse' {} a -> s {certificates = a} :: DescribeCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
describeCertificatesResponse_marker :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe Prelude.Text)
describeCertificatesResponse_marker = Lens.lens (\DescribeCertificatesResponse' {marker} -> marker) (\s@DescribeCertificatesResponse' {} a -> s {marker = a} :: DescribeCertificatesResponse)

-- | The response's http status code.
describeCertificatesResponse_httpStatus :: Lens.Lens' DescribeCertificatesResponse Prelude.Int
describeCertificatesResponse_httpStatus = Lens.lens (\DescribeCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeCertificatesResponse)

instance Prelude.NFData DescribeCertificatesResponse where
  rnf DescribeCertificatesResponse' {..} =
    Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

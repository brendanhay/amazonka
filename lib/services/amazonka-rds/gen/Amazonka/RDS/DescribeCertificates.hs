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
-- Module      : Amazonka.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this Amazon
-- Web Services account.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeCertificates
  ( -- * Creating a Request
    DescribeCertificates (..),
    newDescribeCertificates,

    -- * Request Lenses
    describeCertificates_marker,
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_certificateIdentifier,

    -- * Destructuring the Response
    DescribeCertificatesResponse (..),
    newDescribeCertificatesResponse,

    -- * Response Lenses
    describeCertificatesResponse_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | An optional pagination token provided by a previous
    -- @DescribeCertificates@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The user-supplied certificate identifier. If this parameter is
    -- specified, information for only the identified certificate is returned.
    -- This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match an existing CertificateIdentifier.
    certificateIdentifier :: Prelude.Maybe Prelude.Text
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
-- 'marker', 'describeCertificates_marker' - An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'filters', 'describeCertificates_filters' - This parameter isn\'t currently supported.
--
-- 'maxRecords', 'describeCertificates_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'certificateIdentifier', 'describeCertificates_certificateIdentifier' - The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing CertificateIdentifier.
newDescribeCertificates ::
  DescribeCertificates
newDescribeCertificates =
  DescribeCertificates'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      certificateIdentifier = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeCertificates_marker :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_marker = Lens.lens (\DescribeCertificates' {marker} -> marker) (\s@DescribeCertificates' {} a -> s {marker = a} :: DescribeCertificates)

-- | This parameter isn\'t currently supported.
describeCertificates_filters :: Lens.Lens' DescribeCertificates (Prelude.Maybe [Filter])
describeCertificates_filters = Lens.lens (\DescribeCertificates' {filters} -> filters) (\s@DescribeCertificates' {} a -> s {filters = a} :: DescribeCertificates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeCertificates_maxRecords :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Int)
describeCertificates_maxRecords = Lens.lens (\DescribeCertificates' {maxRecords} -> maxRecords) (\s@DescribeCertificates' {} a -> s {maxRecords = a} :: DescribeCertificates)

-- | The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing CertificateIdentifier.
describeCertificates_certificateIdentifier :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_certificateIdentifier = Lens.lens (\DescribeCertificates' {certificateIdentifier} -> certificateIdentifier) (\s@DescribeCertificates' {} a -> s {certificateIdentifier = a} :: DescribeCertificates)

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
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCertificatesResult"
      ( \s h x ->
          DescribeCertificatesResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "Certificates" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "Certificate")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificates where
  hashWithSalt _salt DescribeCertificates' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` certificateIdentifier

instance Prelude.NFData DescribeCertificates where
  rnf DescribeCertificates' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf certificateIdentifier

instance Core.ToHeaders DescribeCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCertificates where
  toQuery DescribeCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeCertificates" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords,
        "CertificateIdentifier"
          Core.=: certificateIdentifier
      ]

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'newDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | An optional pagination token provided by a previous
    -- @DescribeCertificates@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of @Certificate@ objects for the Amazon Web Services account.
    certificates :: Prelude.Maybe [Certificate],
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
-- 'marker', 'describeCertificatesResponse_marker' - An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
--
-- 'certificates', 'describeCertificatesResponse_certificates' - The list of @Certificate@ objects for the Amazon Web Services account.
--
-- 'httpStatus', 'describeCertificatesResponse_httpStatus' - The response's http status code.
newDescribeCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCertificatesResponse
newDescribeCertificatesResponse pHttpStatus_ =
  DescribeCertificatesResponse'
    { marker =
        Prelude.Nothing,
      certificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeCertificatesResponse_marker :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe Prelude.Text)
describeCertificatesResponse_marker = Lens.lens (\DescribeCertificatesResponse' {marker} -> marker) (\s@DescribeCertificatesResponse' {} a -> s {marker = a} :: DescribeCertificatesResponse)

-- | The list of @Certificate@ objects for the Amazon Web Services account.
describeCertificatesResponse_certificates :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe [Certificate])
describeCertificatesResponse_certificates = Lens.lens (\DescribeCertificatesResponse' {certificates} -> certificates) (\s@DescribeCertificatesResponse' {} a -> s {certificates = a} :: DescribeCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCertificatesResponse_httpStatus :: Lens.Lens' DescribeCertificatesResponse Prelude.Int
describeCertificatesResponse_httpStatus = Lens.lens (\DescribeCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeCertificatesResponse)

instance Prelude.NFData DescribeCertificatesResponse where
  rnf DescribeCertificatesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf httpStatus

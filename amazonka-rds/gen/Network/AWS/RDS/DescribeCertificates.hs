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
-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this AWS
-- account.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCertificates
  ( -- * Creating a Request
    DescribeCertificates (..),
    newDescribeCertificates,

    -- * Request Lenses
    describeCertificates_certificateIdentifier,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { -- | The user-supplied certificate identifier. If this parameter is
    -- specified, information for only the identified certificate is returned.
    -- This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match an existing CertificateIdentifier.
    certificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeCertificates@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateIdentifier', 'describeCertificates_certificateIdentifier' - The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing CertificateIdentifier.
--
-- 'filters', 'describeCertificates_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeCertificates_marker' - An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCertificates_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeCertificates ::
  DescribeCertificates
newDescribeCertificates =
  DescribeCertificates'
    { certificateIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing CertificateIdentifier.
describeCertificates_certificateIdentifier :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_certificateIdentifier = Lens.lens (\DescribeCertificates' {certificateIdentifier} -> certificateIdentifier) (\s@DescribeCertificates' {} a -> s {certificateIdentifier = a} :: DescribeCertificates)

-- | This parameter isn\'t currently supported.
describeCertificates_filters :: Lens.Lens' DescribeCertificates (Prelude.Maybe [Filter])
describeCertificates_filters = Lens.lens (\DescribeCertificates' {filters} -> filters) (\s@DescribeCertificates' {} a -> s {filters = a} :: DescribeCertificates) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeCertificates_marker :: Lens.Lens' DescribeCertificates (Prelude.Maybe Prelude.Text)
describeCertificates_marker = Lens.lens (\DescribeCertificates' {marker} -> marker) (\s@DescribeCertificates' {} a -> s {marker = a} :: DescribeCertificates)

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

instance Pager.AWSPager DescribeCertificates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeCertificatesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeCertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeCertificates_marker
          Lens..~ rs
          Lens.^? describeCertificatesResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeCertificates where
  type
    Rs DescribeCertificates =
      DescribeCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCertificatesResult"
      ( \s h x ->
          DescribeCertificatesResponse'
            Prelude.<$> ( x Prelude..@? "Certificates"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "Certificate")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificates

instance Prelude.NFData DescribeCertificates

instance Prelude.ToHeaders DescribeCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeCertificates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeCertificates where
  toQuery DescribeCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeCertificates" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "CertificateIdentifier"
          Prelude.=: certificateIdentifier,
        "Filters"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'newDescribeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { -- | The list of @Certificate@ objects for the AWS account.
    certificates :: Prelude.Maybe [Certificate],
    -- | An optional pagination token provided by a previous
    -- @DescribeCertificates@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'describeCertificatesResponse_certificates' - The list of @Certificate@ objects for the AWS account.
--
-- 'marker', 'describeCertificatesResponse_marker' - An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
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

-- | The list of @Certificate@ objects for the AWS account.
describeCertificatesResponse_certificates :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe [Certificate])
describeCertificatesResponse_certificates = Lens.lens (\DescribeCertificatesResponse' {certificates} -> certificates) (\s@DescribeCertificatesResponse' {} a -> s {certificates = a} :: DescribeCertificatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeCertificates@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeCertificatesResponse_marker :: Lens.Lens' DescribeCertificatesResponse (Prelude.Maybe Prelude.Text)
describeCertificatesResponse_marker = Lens.lens (\DescribeCertificatesResponse' {marker} -> marker) (\s@DescribeCertificatesResponse' {} a -> s {marker = a} :: DescribeCertificatesResponse)

-- | The response's http status code.
describeCertificatesResponse_httpStatus :: Lens.Lens' DescribeCertificatesResponse Prelude.Int
describeCertificatesResponse_httpStatus = Lens.lens (\DescribeCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeCertificatesResponse)

instance Prelude.NFData DescribeCertificatesResponse

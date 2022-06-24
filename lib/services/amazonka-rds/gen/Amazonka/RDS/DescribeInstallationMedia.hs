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
-- Module      : Amazonka.RDS.DescribeInstallationMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available installation media for a DB engine that requires
-- an on-premises customer provided license, such as Microsoft SQL Server.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeInstallationMedia
  ( -- * Creating a Request
    DescribeInstallationMedia (..),
    newDescribeInstallationMedia,

    -- * Request Lenses
    describeInstallationMedia_marker,
    describeInstallationMedia_filters,
    describeInstallationMedia_maxRecords,
    describeInstallationMedia_installationMediaId,

    -- * Destructuring the Response
    DescribeInstallationMediaResponse (..),
    newDescribeInstallationMediaResponse,

    -- * Response Lenses
    describeInstallationMediaResponse_marker,
    describeInstallationMediaResponse_installationMedia,
    describeInstallationMediaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstallationMedia' smart constructor.
data DescribeInstallationMedia = DescribeInstallationMedia'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more installation media to describe.
    -- Supported filters include the following:
    --
    -- -   @custom-availability-zone-id@ - Accepts custom Availability Zone
    --     (AZ) identifiers. The results list includes information about only
    --     the custom AZs identified by these identifiers.
    --
    -- -   @engine@ - Accepts database engines. The results list includes
    --     information about only the database engines identified by these
    --     identifiers.
    --
    --     For more information about the valid engines for installation media,
    --     see ImportInstallationMedia.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- DescribeInstallationMedia request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The installation medium ID.
    installationMediaId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstallationMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeInstallationMedia_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeInstallationMedia_filters' - A filter that specifies one or more installation media to describe.
-- Supported filters include the following:
--
-- -   @custom-availability-zone-id@ - Accepts custom Availability Zone
--     (AZ) identifiers. The results list includes information about only
--     the custom AZs identified by these identifiers.
--
-- -   @engine@ - Accepts database engines. The results list includes
--     information about only the database engines identified by these
--     identifiers.
--
--     For more information about the valid engines for installation media,
--     see ImportInstallationMedia.
--
-- 'maxRecords', 'describeInstallationMedia_maxRecords' - An optional pagination token provided by a previous
-- DescribeInstallationMedia request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'installationMediaId', 'describeInstallationMedia_installationMediaId' - The installation medium ID.
newDescribeInstallationMedia ::
  DescribeInstallationMedia
newDescribeInstallationMedia =
  DescribeInstallationMedia'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      installationMediaId = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeInstallationMedia_marker :: Lens.Lens' DescribeInstallationMedia (Prelude.Maybe Prelude.Text)
describeInstallationMedia_marker = Lens.lens (\DescribeInstallationMedia' {marker} -> marker) (\s@DescribeInstallationMedia' {} a -> s {marker = a} :: DescribeInstallationMedia)

-- | A filter that specifies one or more installation media to describe.
-- Supported filters include the following:
--
-- -   @custom-availability-zone-id@ - Accepts custom Availability Zone
--     (AZ) identifiers. The results list includes information about only
--     the custom AZs identified by these identifiers.
--
-- -   @engine@ - Accepts database engines. The results list includes
--     information about only the database engines identified by these
--     identifiers.
--
--     For more information about the valid engines for installation media,
--     see ImportInstallationMedia.
describeInstallationMedia_filters :: Lens.Lens' DescribeInstallationMedia (Prelude.Maybe [Filter])
describeInstallationMedia_filters = Lens.lens (\DescribeInstallationMedia' {filters} -> filters) (\s@DescribeInstallationMedia' {} a -> s {filters = a} :: DescribeInstallationMedia) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- DescribeInstallationMedia request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeInstallationMedia_maxRecords :: Lens.Lens' DescribeInstallationMedia (Prelude.Maybe Prelude.Int)
describeInstallationMedia_maxRecords = Lens.lens (\DescribeInstallationMedia' {maxRecords} -> maxRecords) (\s@DescribeInstallationMedia' {} a -> s {maxRecords = a} :: DescribeInstallationMedia)

-- | The installation medium ID.
describeInstallationMedia_installationMediaId :: Lens.Lens' DescribeInstallationMedia (Prelude.Maybe Prelude.Text)
describeInstallationMedia_installationMediaId = Lens.lens (\DescribeInstallationMedia' {installationMediaId} -> installationMediaId) (\s@DescribeInstallationMedia' {} a -> s {installationMediaId = a} :: DescribeInstallationMedia)

instance Core.AWSPager DescribeInstallationMedia where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstallationMediaResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstallationMediaResponse_installationMedia
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstallationMedia_marker
          Lens..~ rs
          Lens.^? describeInstallationMediaResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstallationMedia where
  type
    AWSResponse DescribeInstallationMedia =
      DescribeInstallationMediaResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeInstallationMediaResult"
      ( \s h x ->
          DescribeInstallationMediaResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "InstallationMedia"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "InstallationMedia")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstallationMedia where
  hashWithSalt _salt DescribeInstallationMedia' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` installationMediaId

instance Prelude.NFData DescribeInstallationMedia where
  rnf DescribeInstallationMedia' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf installationMediaId

instance Core.ToHeaders DescribeInstallationMedia where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstallationMedia where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstallationMedia where
  toQuery DescribeInstallationMedia' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstallationMedia" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords,
        "InstallationMediaId" Core.=: installationMediaId
      ]

-- | /See:/ 'newDescribeInstallationMediaResponse' smart constructor.
data DescribeInstallationMediaResponse = DescribeInstallationMediaResponse'
  { -- | An optional pagination token provided by a previous
    -- DescribeInstallationMedia request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of InstallationMedia objects for the Amazon Web Services
    -- account.
    installationMedia :: Prelude.Maybe [InstallationMedia],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstallationMediaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeInstallationMediaResponse_marker' - An optional pagination token provided by a previous
-- DescribeInstallationMedia request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'installationMedia', 'describeInstallationMediaResponse_installationMedia' - The list of InstallationMedia objects for the Amazon Web Services
-- account.
--
-- 'httpStatus', 'describeInstallationMediaResponse_httpStatus' - The response's http status code.
newDescribeInstallationMediaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstallationMediaResponse
newDescribeInstallationMediaResponse pHttpStatus_ =
  DescribeInstallationMediaResponse'
    { marker =
        Prelude.Nothing,
      installationMedia = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous
-- DescribeInstallationMedia request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeInstallationMediaResponse_marker :: Lens.Lens' DescribeInstallationMediaResponse (Prelude.Maybe Prelude.Text)
describeInstallationMediaResponse_marker = Lens.lens (\DescribeInstallationMediaResponse' {marker} -> marker) (\s@DescribeInstallationMediaResponse' {} a -> s {marker = a} :: DescribeInstallationMediaResponse)

-- | The list of InstallationMedia objects for the Amazon Web Services
-- account.
describeInstallationMediaResponse_installationMedia :: Lens.Lens' DescribeInstallationMediaResponse (Prelude.Maybe [InstallationMedia])
describeInstallationMediaResponse_installationMedia = Lens.lens (\DescribeInstallationMediaResponse' {installationMedia} -> installationMedia) (\s@DescribeInstallationMediaResponse' {} a -> s {installationMedia = a} :: DescribeInstallationMediaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstallationMediaResponse_httpStatus :: Lens.Lens' DescribeInstallationMediaResponse Prelude.Int
describeInstallationMediaResponse_httpStatus = Lens.lens (\DescribeInstallationMediaResponse' {httpStatus} -> httpStatus) (\s@DescribeInstallationMediaResponse' {} a -> s {httpStatus = a} :: DescribeInstallationMediaResponse)

instance
  Prelude.NFData
    DescribeInstallationMediaResponse
  where
  rnf DescribeInstallationMediaResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf installationMedia
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.RDS.DescribeSourceRegions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the source Amazon Web Services Regions where the
-- current Amazon Web Services Region can create a read replica, copy a DB
-- snapshot from, or replicate automated backups from. This API action
-- supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeSourceRegions
  ( -- * Creating a Request
    DescribeSourceRegions (..),
    newDescribeSourceRegions,

    -- * Request Lenses
    describeSourceRegions_regionName,
    describeSourceRegions_filters,
    describeSourceRegions_marker,
    describeSourceRegions_maxRecords,

    -- * Destructuring the Response
    DescribeSourceRegionsResponse (..),
    newDescribeSourceRegionsResponse,

    -- * Response Lenses
    describeSourceRegionsResponse_marker,
    describeSourceRegionsResponse_sourceRegions,
    describeSourceRegionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeSourceRegions' smart constructor.
data DescribeSourceRegions = DescribeSourceRegions'
  { -- | The source Amazon Web Services Region name. For example, @us-east-1@.
    --
    -- Constraints:
    --
    -- -   Must specify a valid Amazon Web Services Region name.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeSourceRegions@ request. If this parameter is specified, the
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'describeSourceRegions_regionName' - The source Amazon Web Services Region name. For example, @us-east-1@.
--
-- Constraints:
--
-- -   Must specify a valid Amazon Web Services Region name.
--
-- 'filters', 'describeSourceRegions_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeSourceRegions_marker' - An optional pagination token provided by a previous
-- @DescribeSourceRegions@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeSourceRegions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeSourceRegions ::
  DescribeSourceRegions
newDescribeSourceRegions =
  DescribeSourceRegions'
    { regionName =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The source Amazon Web Services Region name. For example, @us-east-1@.
--
-- Constraints:
--
-- -   Must specify a valid Amazon Web Services Region name.
describeSourceRegions_regionName :: Lens.Lens' DescribeSourceRegions (Prelude.Maybe Prelude.Text)
describeSourceRegions_regionName = Lens.lens (\DescribeSourceRegions' {regionName} -> regionName) (\s@DescribeSourceRegions' {} a -> s {regionName = a} :: DescribeSourceRegions)

-- | This parameter isn\'t currently supported.
describeSourceRegions_filters :: Lens.Lens' DescribeSourceRegions (Prelude.Maybe [Filter])
describeSourceRegions_filters = Lens.lens (\DescribeSourceRegions' {filters} -> filters) (\s@DescribeSourceRegions' {} a -> s {filters = a} :: DescribeSourceRegions) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeSourceRegions@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeSourceRegions_marker :: Lens.Lens' DescribeSourceRegions (Prelude.Maybe Prelude.Text)
describeSourceRegions_marker = Lens.lens (\DescribeSourceRegions' {marker} -> marker) (\s@DescribeSourceRegions' {} a -> s {marker = a} :: DescribeSourceRegions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeSourceRegions_maxRecords :: Lens.Lens' DescribeSourceRegions (Prelude.Maybe Prelude.Int)
describeSourceRegions_maxRecords = Lens.lens (\DescribeSourceRegions' {maxRecords} -> maxRecords) (\s@DescribeSourceRegions' {} a -> s {maxRecords = a} :: DescribeSourceRegions)

instance Core.AWSPager DescribeSourceRegions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSourceRegionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSourceRegionsResponse_sourceRegions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSourceRegions_marker
          Lens..~ rs
          Lens.^? describeSourceRegionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSourceRegions where
  type
    AWSResponse DescribeSourceRegions =
      DescribeSourceRegionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSourceRegionsResult"
      ( \s h x ->
          DescribeSourceRegionsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "SourceRegions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "SourceRegion")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSourceRegions

instance Prelude.NFData DescribeSourceRegions

instance Core.ToHeaders DescribeSourceRegions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSourceRegions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSourceRegions where
  toQuery DescribeSourceRegions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeSourceRegions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "RegionName" Core.=: regionName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeSourceRegions@ action.
--
-- /See:/ 'newDescribeSourceRegionsResponse' smart constructor.
data DescribeSourceRegionsResponse = DescribeSourceRegionsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of SourceRegion instances that contains each source Amazon Web
    -- Services Region that the current Amazon Web Services Region can get a
    -- read replica or a DB snapshot from.
    sourceRegions :: Prelude.Maybe [SourceRegion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSourceRegionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'sourceRegions', 'describeSourceRegionsResponse_sourceRegions' - A list of SourceRegion instances that contains each source Amazon Web
-- Services Region that the current Amazon Web Services Region can get a
-- read replica or a DB snapshot from.
--
-- 'httpStatus', 'describeSourceRegionsResponse_httpStatus' - The response's http status code.
newDescribeSourceRegionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSourceRegionsResponse
newDescribeSourceRegionsResponse pHttpStatus_ =
  DescribeSourceRegionsResponse'
    { marker =
        Prelude.Nothing,
      sourceRegions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeSourceRegionsResponse_marker :: Lens.Lens' DescribeSourceRegionsResponse (Prelude.Maybe Prelude.Text)
describeSourceRegionsResponse_marker = Lens.lens (\DescribeSourceRegionsResponse' {marker} -> marker) (\s@DescribeSourceRegionsResponse' {} a -> s {marker = a} :: DescribeSourceRegionsResponse)

-- | A list of SourceRegion instances that contains each source Amazon Web
-- Services Region that the current Amazon Web Services Region can get a
-- read replica or a DB snapshot from.
describeSourceRegionsResponse_sourceRegions :: Lens.Lens' DescribeSourceRegionsResponse (Prelude.Maybe [SourceRegion])
describeSourceRegionsResponse_sourceRegions = Lens.lens (\DescribeSourceRegionsResponse' {sourceRegions} -> sourceRegions) (\s@DescribeSourceRegionsResponse' {} a -> s {sourceRegions = a} :: DescribeSourceRegionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSourceRegionsResponse_httpStatus :: Lens.Lens' DescribeSourceRegionsResponse Prelude.Int
describeSourceRegionsResponse_httpStatus = Lens.lens (\DescribeSourceRegionsResponse' {httpStatus} -> httpStatus) (\s@DescribeSourceRegionsResponse' {} a -> s {httpStatus = a} :: DescribeSourceRegionsResponse)

instance Prelude.NFData DescribeSourceRegionsResponse

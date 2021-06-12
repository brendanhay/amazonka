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
-- Module      : Network.AWS.RDS.DescribeDBProxies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxies.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxies
  ( -- * Creating a Request
    DescribeDBProxies (..),
    newDescribeDBProxies,

    -- * Request Lenses
    describeDBProxies_filters,
    describeDBProxies_dbProxyName,
    describeDBProxies_marker,
    describeDBProxies_maxRecords,

    -- * Destructuring the Response
    DescribeDBProxiesResponse (..),
    newDescribeDBProxiesResponse,

    -- * Response Lenses
    describeDBProxiesResponse_dbProxies,
    describeDBProxiesResponse_marker,
    describeDBProxiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxies' smart constructor.
data DescribeDBProxies = DescribeDBProxies'
  { -- | This parameter is not currently supported.
    filters :: Core.Maybe [Filter],
    -- | The name of the DB proxy.
    dbProxyName :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBProxies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeDBProxies_filters' - This parameter is not currently supported.
--
-- 'dbProxyName', 'describeDBProxies_dbProxyName' - The name of the DB proxy.
--
-- 'marker', 'describeDBProxies_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBProxies_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBProxies ::
  DescribeDBProxies
newDescribeDBProxies =
  DescribeDBProxies'
    { filters = Core.Nothing,
      dbProxyName = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | This parameter is not currently supported.
describeDBProxies_filters :: Lens.Lens' DescribeDBProxies (Core.Maybe [Filter])
describeDBProxies_filters = Lens.lens (\DescribeDBProxies' {filters} -> filters) (\s@DescribeDBProxies' {} a -> s {filters = a} :: DescribeDBProxies) Core.. Lens.mapping Lens._Coerce

-- | The name of the DB proxy.
describeDBProxies_dbProxyName :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Text)
describeDBProxies_dbProxyName = Lens.lens (\DescribeDBProxies' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxies' {} a -> s {dbProxyName = a} :: DescribeDBProxies)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxies_marker :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Text)
describeDBProxies_marker = Lens.lens (\DescribeDBProxies' {marker} -> marker) (\s@DescribeDBProxies' {} a -> s {marker = a} :: DescribeDBProxies)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxies_maxRecords :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Natural)
describeDBProxies_maxRecords = Lens.lens (\DescribeDBProxies' {maxRecords} -> maxRecords) (\s@DescribeDBProxies' {} a -> s {maxRecords = a} :: DescribeDBProxies)

instance Core.AWSPager DescribeDBProxies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxiesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxiesResponse_dbProxies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBProxies_marker
          Lens..~ rs
          Lens.^? describeDBProxiesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeDBProxies where
  type
    AWSResponse DescribeDBProxies =
      DescribeDBProxiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxiesResult"
      ( \s h x ->
          DescribeDBProxiesResponse'
            Core.<$> ( x Core..@? "DBProxies" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBProxies

instance Core.NFData DescribeDBProxies

instance Core.ToHeaders DescribeDBProxies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBProxies where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBProxies where
  toQuery DescribeDBProxies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBProxies" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "DBProxyName" Core.=: dbProxyName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeDBProxiesResponse' smart constructor.
data DescribeDBProxiesResponse = DescribeDBProxiesResponse'
  { -- | A return value representing an arbitrary number of @DBProxy@ data
    -- structures.
    dbProxies :: Core.Maybe [DBProxy],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBProxiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxies', 'describeDBProxiesResponse_dbProxies' - A return value representing an arbitrary number of @DBProxy@ data
-- structures.
--
-- 'marker', 'describeDBProxiesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBProxiesResponse_httpStatus' - The response's http status code.
newDescribeDBProxiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBProxiesResponse
newDescribeDBProxiesResponse pHttpStatus_ =
  DescribeDBProxiesResponse'
    { dbProxies =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A return value representing an arbitrary number of @DBProxy@ data
-- structures.
describeDBProxiesResponse_dbProxies :: Lens.Lens' DescribeDBProxiesResponse (Core.Maybe [DBProxy])
describeDBProxiesResponse_dbProxies = Lens.lens (\DescribeDBProxiesResponse' {dbProxies} -> dbProxies) (\s@DescribeDBProxiesResponse' {} a -> s {dbProxies = a} :: DescribeDBProxiesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxiesResponse_marker :: Lens.Lens' DescribeDBProxiesResponse (Core.Maybe Core.Text)
describeDBProxiesResponse_marker = Lens.lens (\DescribeDBProxiesResponse' {marker} -> marker) (\s@DescribeDBProxiesResponse' {} a -> s {marker = a} :: DescribeDBProxiesResponse)

-- | The response's http status code.
describeDBProxiesResponse_httpStatus :: Lens.Lens' DescribeDBProxiesResponse Core.Int
describeDBProxiesResponse_httpStatus = Lens.lens (\DescribeDBProxiesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxiesResponse' {} a -> s {httpStatus = a} :: DescribeDBProxiesResponse)

instance Core.NFData DescribeDBProxiesResponse

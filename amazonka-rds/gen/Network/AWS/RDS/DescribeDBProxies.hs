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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxies' smart constructor.
data DescribeDBProxies = DescribeDBProxies'
  { -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The name of the DB proxy.
    dbProxyName :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { filters = Prelude.Nothing,
      dbProxyName = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | This parameter is not currently supported.
describeDBProxies_filters :: Lens.Lens' DescribeDBProxies (Prelude.Maybe [Filter])
describeDBProxies_filters = Lens.lens (\DescribeDBProxies' {filters} -> filters) (\s@DescribeDBProxies' {} a -> s {filters = a} :: DescribeDBProxies) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the DB proxy.
describeDBProxies_dbProxyName :: Lens.Lens' DescribeDBProxies (Prelude.Maybe Prelude.Text)
describeDBProxies_dbProxyName = Lens.lens (\DescribeDBProxies' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxies' {} a -> s {dbProxyName = a} :: DescribeDBProxies)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxies_marker :: Lens.Lens' DescribeDBProxies (Prelude.Maybe Prelude.Text)
describeDBProxies_marker = Lens.lens (\DescribeDBProxies' {marker} -> marker) (\s@DescribeDBProxies' {} a -> s {marker = a} :: DescribeDBProxies)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxies_maxRecords :: Lens.Lens' DescribeDBProxies (Prelude.Maybe Prelude.Natural)
describeDBProxies_maxRecords = Lens.lens (\DescribeDBProxies' {maxRecords} -> maxRecords) (\s@DescribeDBProxies' {} a -> s {maxRecords = a} :: DescribeDBProxies)

instance Pager.AWSPager DescribeDBProxies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeDBProxiesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeDBProxiesResponse_dbProxies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeDBProxies_marker
          Lens..~ rs
          Lens.^? describeDBProxiesResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeDBProxies where
  type Rs DescribeDBProxies = DescribeDBProxiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxiesResult"
      ( \s h x ->
          DescribeDBProxiesResponse'
            Prelude.<$> ( x Prelude..@? "DBProxies" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxies

instance Prelude.NFData DescribeDBProxies

instance Prelude.ToHeaders DescribeDBProxies where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeDBProxies where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDBProxies where
  toQuery DescribeDBProxies' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeDBProxies" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "DBProxyName" Prelude.=: dbProxyName,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | /See:/ 'newDescribeDBProxiesResponse' smart constructor.
data DescribeDBProxiesResponse = DescribeDBProxiesResponse'
  { -- | A return value representing an arbitrary number of @DBProxy@ data
    -- structures.
    dbProxies :: Prelude.Maybe [DBProxy],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBProxiesResponse
newDescribeDBProxiesResponse pHttpStatus_ =
  DescribeDBProxiesResponse'
    { dbProxies =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A return value representing an arbitrary number of @DBProxy@ data
-- structures.
describeDBProxiesResponse_dbProxies :: Lens.Lens' DescribeDBProxiesResponse (Prelude.Maybe [DBProxy])
describeDBProxiesResponse_dbProxies = Lens.lens (\DescribeDBProxiesResponse' {dbProxies} -> dbProxies) (\s@DescribeDBProxiesResponse' {} a -> s {dbProxies = a} :: DescribeDBProxiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxiesResponse_marker :: Lens.Lens' DescribeDBProxiesResponse (Prelude.Maybe Prelude.Text)
describeDBProxiesResponse_marker = Lens.lens (\DescribeDBProxiesResponse' {marker} -> marker) (\s@DescribeDBProxiesResponse' {} a -> s {marker = a} :: DescribeDBProxiesResponse)

-- | The response's http status code.
describeDBProxiesResponse_httpStatus :: Lens.Lens' DescribeDBProxiesResponse Prelude.Int
describeDBProxiesResponse_httpStatus = Lens.lens (\DescribeDBProxiesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxiesResponse' {} a -> s {httpStatus = a} :: DescribeDBProxiesResponse)

instance Prelude.NFData DescribeDBProxiesResponse

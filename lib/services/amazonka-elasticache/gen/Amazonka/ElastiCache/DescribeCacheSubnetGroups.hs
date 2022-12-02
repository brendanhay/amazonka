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
-- Module      : Amazonka.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache subnet group descriptions. If a subnet group
-- name is specified, the list contains only the description of that group.
-- This is applicable only when you have ElastiCache in VPC setup. All
-- ElastiCache clusters now launch in VPC by default.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheSubnetGroups
  ( -- * Creating a Request
    DescribeCacheSubnetGroups (..),
    newDescribeCacheSubnetGroups,

    -- * Request Lenses
    describeCacheSubnetGroups_cacheSubnetGroupName,
    describeCacheSubnetGroups_marker,
    describeCacheSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeCacheSubnetGroupsResponse (..),
    newDescribeCacheSubnetGroupsResponse,

    -- * Response Lenses
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'newDescribeCacheSubnetGroups' smart constructor.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
  { -- | The name of the cache subnet group to return details for.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroupName', 'describeCacheSubnetGroups_cacheSubnetGroupName' - The name of the cache subnet group to return details for.
--
-- 'marker', 'describeCacheSubnetGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeCacheSubnetGroups ::
  DescribeCacheSubnetGroups
newDescribeCacheSubnetGroups =
  DescribeCacheSubnetGroups'
    { cacheSubnetGroupName =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of the cache subnet group to return details for.
describeCacheSubnetGroups_cacheSubnetGroupName :: Lens.Lens' DescribeCacheSubnetGroups (Prelude.Maybe Prelude.Text)
describeCacheSubnetGroups_cacheSubnetGroupName = Lens.lens (\DescribeCacheSubnetGroups' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@DescribeCacheSubnetGroups' {} a -> s {cacheSubnetGroupName = a} :: DescribeCacheSubnetGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheSubnetGroups_marker :: Lens.Lens' DescribeCacheSubnetGroups (Prelude.Maybe Prelude.Text)
describeCacheSubnetGroups_marker = Lens.lens (\DescribeCacheSubnetGroups' {marker} -> marker) (\s@DescribeCacheSubnetGroups' {} a -> s {marker = a} :: DescribeCacheSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheSubnetGroups_maxRecords :: Lens.Lens' DescribeCacheSubnetGroups (Prelude.Maybe Prelude.Int)
describeCacheSubnetGroups_maxRecords = Lens.lens (\DescribeCacheSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeCacheSubnetGroups' {} a -> s {maxRecords = a} :: DescribeCacheSubnetGroups)

instance Core.AWSPager DescribeCacheSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_cacheSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCacheSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeCacheSubnetGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheSubnetGroups where
  type
    AWSResponse DescribeCacheSubnetGroups =
      DescribeCacheSubnetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheSubnetGroupsResult"
      ( \s h x ->
          DescribeCacheSubnetGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "CacheSubnetGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "CacheSubnetGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheSubnetGroups where
  hashWithSalt _salt DescribeCacheSubnetGroups' {..} =
    _salt `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeCacheSubnetGroups where
  rnf DescribeCacheSubnetGroups' {..} =
    Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeCacheSubnetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheSubnetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheSubnetGroups where
  toQuery DescribeCacheSubnetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeCacheSubnetGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSubnetGroupName" Data.=: cacheSubnetGroupName,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'newDescribeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of cache subnet groups. Each element in the list contains
    -- detailed information about one group.
    cacheSubnetGroups :: Prelude.Maybe [CacheSubnetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCacheSubnetGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'cacheSubnetGroups', 'describeCacheSubnetGroupsResponse_cacheSubnetGroups' - A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
--
-- 'httpStatus', 'describeCacheSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheSubnetGroupsResponse
newDescribeCacheSubnetGroupsResponse pHttpStatus_ =
  DescribeCacheSubnetGroupsResponse'
    { marker =
        Prelude.Nothing,
      cacheSubnetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheSubnetGroupsResponse_marker :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeCacheSubnetGroupsResponse_marker = Lens.lens (\DescribeCacheSubnetGroupsResponse' {marker} -> marker) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeCacheSubnetGroupsResponse)

-- | A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
describeCacheSubnetGroupsResponse_cacheSubnetGroups :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Prelude.Maybe [CacheSubnetGroup])
describeCacheSubnetGroupsResponse_cacheSubnetGroups = Lens.lens (\DescribeCacheSubnetGroupsResponse' {cacheSubnetGroups} -> cacheSubnetGroups) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {cacheSubnetGroups = a} :: DescribeCacheSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCacheSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheSubnetGroupsResponse Prelude.Int
describeCacheSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeCacheSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeCacheSubnetGroupsResponse
  where
  rnf DescribeCacheSubnetGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf cacheSubnetGroups
      `Prelude.seq` Prelude.rnf httpStatus

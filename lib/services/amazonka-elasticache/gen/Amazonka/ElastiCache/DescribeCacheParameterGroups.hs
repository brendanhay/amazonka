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
-- Module      : Amazonka.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache parameter group descriptions. If a cache
-- parameter group name is specified, the list contains only the
-- descriptions for that group.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheParameterGroups
  ( -- * Creating a Request
    DescribeCacheParameterGroups (..),
    newDescribeCacheParameterGroups,

    -- * Request Lenses
    describeCacheParameterGroups_cacheParameterGroupName,
    describeCacheParameterGroups_marker,
    describeCacheParameterGroups_maxRecords,

    -- * Destructuring the Response
    DescribeCacheParameterGroupsResponse (..),
    newDescribeCacheParameterGroupsResponse,

    -- * Response Lenses
    describeCacheParameterGroupsResponse_cacheParameterGroups,
    describeCacheParameterGroupsResponse_marker,
    describeCacheParameterGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'newDescribeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DescribeCacheParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'describeCacheParameterGroups_cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
--
-- 'marker', 'describeCacheParameterGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeCacheParameterGroups ::
  DescribeCacheParameterGroups
newDescribeCacheParameterGroups =
  DescribeCacheParameterGroups'
    { cacheParameterGroupName =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of a specific cache parameter group to return details for.
describeCacheParameterGroups_cacheParameterGroupName :: Lens.Lens' DescribeCacheParameterGroups (Prelude.Maybe Prelude.Text)
describeCacheParameterGroups_cacheParameterGroupName = Lens.lens (\DescribeCacheParameterGroups' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@DescribeCacheParameterGroups' {} a -> s {cacheParameterGroupName = a} :: DescribeCacheParameterGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheParameterGroups_marker :: Lens.Lens' DescribeCacheParameterGroups (Prelude.Maybe Prelude.Text)
describeCacheParameterGroups_marker = Lens.lens (\DescribeCacheParameterGroups' {marker} -> marker) (\s@DescribeCacheParameterGroups' {} a -> s {marker = a} :: DescribeCacheParameterGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheParameterGroups_maxRecords :: Lens.Lens' DescribeCacheParameterGroups (Prelude.Maybe Prelude.Int)
describeCacheParameterGroups_maxRecords = Lens.lens (\DescribeCacheParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeCacheParameterGroups' {} a -> s {maxRecords = a} :: DescribeCacheParameterGroups)

instance Core.AWSPager DescribeCacheParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheParameterGroupsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheParameterGroupsResponse_cacheParameterGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeCacheParameterGroups_marker
          Lens..~ rs
          Lens.^? describeCacheParameterGroupsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheParameterGroups where
  type
    AWSResponse DescribeCacheParameterGroups =
      DescribeCacheParameterGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheParameterGroupsResult"
      ( \s h x ->
          DescribeCacheParameterGroupsResponse'
            Prelude.<$> ( x
                            Data..@? "CacheParameterGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "CacheParameterGroup")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCacheParameterGroups
  where
  hashWithSalt _salt DescribeCacheParameterGroups' {..} =
    _salt
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeCacheParameterGroups where
  rnf DescribeCacheParameterGroups' {..} =
    Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeCacheParameterGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheParameterGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheParameterGroups where
  toQuery DescribeCacheParameterGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeCacheParameterGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'newDescribeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { -- | A list of cache parameter groups. Each element in the list contains
    -- detailed information about one cache parameter group.
    cacheParameterGroups :: Prelude.Maybe [CacheParameterGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroups', 'describeCacheParameterGroupsResponse_cacheParameterGroups' - A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
--
-- 'marker', 'describeCacheParameterGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheParameterGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheParameterGroupsResponse
newDescribeCacheParameterGroupsResponse pHttpStatus_ =
  DescribeCacheParameterGroupsResponse'
    { cacheParameterGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
describeCacheParameterGroupsResponse_cacheParameterGroups :: Lens.Lens' DescribeCacheParameterGroupsResponse (Prelude.Maybe [CacheParameterGroup])
describeCacheParameterGroupsResponse_cacheParameterGroups = Lens.lens (\DescribeCacheParameterGroupsResponse' {cacheParameterGroups} -> cacheParameterGroups) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {cacheParameterGroups = a} :: DescribeCacheParameterGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheParameterGroupsResponse_marker :: Lens.Lens' DescribeCacheParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeCacheParameterGroupsResponse_marker = Lens.lens (\DescribeCacheParameterGroupsResponse' {marker} -> marker) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {marker = a} :: DescribeCacheParameterGroupsResponse)

-- | The response's http status code.
describeCacheParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheParameterGroupsResponse Prelude.Int
describeCacheParameterGroupsResponse_httpStatus = Lens.lens (\DescribeCacheParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeCacheParameterGroupsResponse
  where
  rnf DescribeCacheParameterGroupsResponse' {..} =
    Prelude.rnf cacheParameterGroups
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

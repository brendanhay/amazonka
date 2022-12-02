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
-- Module      : Amazonka.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache security group descriptions. If a cache security
-- group name is specified, the list contains only the description of that
-- group. This applicable only when you have ElastiCache in Classic setup
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheSecurityGroups
  ( -- * Creating a Request
    DescribeCacheSecurityGroups (..),
    newDescribeCacheSecurityGroups,

    -- * Request Lenses
    describeCacheSecurityGroups_marker,
    describeCacheSecurityGroups_maxRecords,
    describeCacheSecurityGroups_cacheSecurityGroupName,

    -- * Destructuring the Response
    DescribeCacheSecurityGroupsResponse (..),
    newDescribeCacheSecurityGroupsResponse,

    -- * Response Lenses
    describeCacheSecurityGroupsResponse_marker,
    describeCacheSecurityGroupsResponse_cacheSecurityGroups,
    describeCacheSecurityGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'newDescribeCacheSecurityGroups' smart constructor.
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
  { -- | An optional marker returned from a prior request. Use this marker for
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
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the cache security group to return details for.
    cacheSecurityGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCacheSecurityGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheSecurityGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
--
-- 'cacheSecurityGroupName', 'describeCacheSecurityGroups_cacheSecurityGroupName' - The name of the cache security group to return details for.
newDescribeCacheSecurityGroups ::
  DescribeCacheSecurityGroups
newDescribeCacheSecurityGroups =
  DescribeCacheSecurityGroups'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      cacheSecurityGroupName = Prelude.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheSecurityGroups_marker :: Lens.Lens' DescribeCacheSecurityGroups (Prelude.Maybe Prelude.Text)
describeCacheSecurityGroups_marker = Lens.lens (\DescribeCacheSecurityGroups' {marker} -> marker) (\s@DescribeCacheSecurityGroups' {} a -> s {marker = a} :: DescribeCacheSecurityGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheSecurityGroups_maxRecords :: Lens.Lens' DescribeCacheSecurityGroups (Prelude.Maybe Prelude.Int)
describeCacheSecurityGroups_maxRecords = Lens.lens (\DescribeCacheSecurityGroups' {maxRecords} -> maxRecords) (\s@DescribeCacheSecurityGroups' {} a -> s {maxRecords = a} :: DescribeCacheSecurityGroups)

-- | The name of the cache security group to return details for.
describeCacheSecurityGroups_cacheSecurityGroupName :: Lens.Lens' DescribeCacheSecurityGroups (Prelude.Maybe Prelude.Text)
describeCacheSecurityGroups_cacheSecurityGroupName = Lens.lens (\DescribeCacheSecurityGroups' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@DescribeCacheSecurityGroups' {} a -> s {cacheSecurityGroupName = a} :: DescribeCacheSecurityGroups)

instance Core.AWSPager DescribeCacheSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheSecurityGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheSecurityGroupsResponse_cacheSecurityGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCacheSecurityGroups_marker
          Lens..~ rs
          Lens.^? describeCacheSecurityGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheSecurityGroups where
  type
    AWSResponse DescribeCacheSecurityGroups =
      DescribeCacheSecurityGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheSecurityGroupsResult"
      ( \s h x ->
          DescribeCacheSecurityGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "CacheSecurityGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "CacheSecurityGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheSecurityGroups where
  hashWithSalt _salt DescribeCacheSecurityGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` cacheSecurityGroupName

instance Prelude.NFData DescribeCacheSecurityGroups where
  rnf DescribeCacheSecurityGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf cacheSecurityGroupName

instance Data.ToHeaders DescribeCacheSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheSecurityGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheSecurityGroups where
  toQuery DescribeCacheSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeCacheSecurityGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "CacheSecurityGroupName"
          Data.=: cacheSecurityGroupName
      ]

-- | Represents the output of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'newDescribeCacheSecurityGroupsResponse' smart constructor.
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of cache security groups. Each element in the list contains
    -- detailed information about one group.
    cacheSecurityGroups :: Prelude.Maybe [CacheSecurityGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCacheSecurityGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'cacheSecurityGroups', 'describeCacheSecurityGroupsResponse_cacheSecurityGroups' - A list of cache security groups. Each element in the list contains
-- detailed information about one group.
--
-- 'httpStatus', 'describeCacheSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheSecurityGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheSecurityGroupsResponse
newDescribeCacheSecurityGroupsResponse pHttpStatus_ =
  DescribeCacheSecurityGroupsResponse'
    { marker =
        Prelude.Nothing,
      cacheSecurityGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheSecurityGroupsResponse_marker :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Prelude.Maybe Prelude.Text)
describeCacheSecurityGroupsResponse_marker = Lens.lens (\DescribeCacheSecurityGroupsResponse' {marker} -> marker) (\s@DescribeCacheSecurityGroupsResponse' {} a -> s {marker = a} :: DescribeCacheSecurityGroupsResponse)

-- | A list of cache security groups. Each element in the list contains
-- detailed information about one group.
describeCacheSecurityGroupsResponse_cacheSecurityGroups :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Prelude.Maybe [CacheSecurityGroup])
describeCacheSecurityGroupsResponse_cacheSecurityGroups = Lens.lens (\DescribeCacheSecurityGroupsResponse' {cacheSecurityGroups} -> cacheSecurityGroups) (\s@DescribeCacheSecurityGroupsResponse' {} a -> s {cacheSecurityGroups = a} :: DescribeCacheSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCacheSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheSecurityGroupsResponse Prelude.Int
describeCacheSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeCacheSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheSecurityGroupsResponse)

instance
  Prelude.NFData
    DescribeCacheSecurityGroupsResponse
  where
  rnf DescribeCacheSecurityGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf cacheSecurityGroups
      `Prelude.seq` Prelude.rnf httpStatus

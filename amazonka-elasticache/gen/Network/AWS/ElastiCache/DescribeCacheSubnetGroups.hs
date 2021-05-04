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
-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
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
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager DescribeCacheSubnetGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_cacheSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeCacheSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeCacheSubnetGroupsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeCacheSubnetGroups where
  type
    Rs DescribeCacheSubnetGroups =
      DescribeCacheSubnetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCacheSubnetGroupsResult"
      ( \s h x ->
          DescribeCacheSubnetGroupsResponse'
            Prelude.<$> ( x Prelude..@? "CacheSubnetGroups"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may
                              (Prelude.parseXMLList "CacheSubnetGroup")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheSubnetGroups

instance Prelude.NFData DescribeCacheSubnetGroups

instance Prelude.ToHeaders DescribeCacheSubnetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeCacheSubnetGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeCacheSubnetGroups where
  toQuery DescribeCacheSubnetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeCacheSubnetGroups" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSubnetGroupName"
          Prelude.=: cacheSubnetGroupName,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'newDescribeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
  { -- | A list of cache subnet groups. Each element in the list contains
    -- detailed information about one group.
    cacheSubnetGroups :: Prelude.Maybe [CacheSubnetGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroups', 'describeCacheSubnetGroupsResponse_cacheSubnetGroups' - A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
--
-- 'marker', 'describeCacheSubnetGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheSubnetGroupsResponse
newDescribeCacheSubnetGroupsResponse pHttpStatus_ =
  DescribeCacheSubnetGroupsResponse'
    { cacheSubnetGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
describeCacheSubnetGroupsResponse_cacheSubnetGroups :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Prelude.Maybe [CacheSubnetGroup])
describeCacheSubnetGroupsResponse_cacheSubnetGroups = Lens.lens (\DescribeCacheSubnetGroupsResponse' {cacheSubnetGroups} -> cacheSubnetGroups) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {cacheSubnetGroups = a} :: DescribeCacheSubnetGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheSubnetGroupsResponse_marker :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeCacheSubnetGroupsResponse_marker = Lens.lens (\DescribeCacheSubnetGroupsResponse' {marker} -> marker) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeCacheSubnetGroupsResponse)

-- | The response's http status code.
describeCacheSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheSubnetGroupsResponse Prelude.Int
describeCacheSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeCacheSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeCacheSubnetGroupsResponse

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
-- Module      : Amazonka.DMS.DescribeReplicationSubnetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication subnet groups.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeReplicationSubnetGroups
  ( -- * Creating a Request
    DescribeReplicationSubnetGroups (..),
    newDescribeReplicationSubnetGroups,

    -- * Request Lenses
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationSubnetGroupsResponse (..),
    newDescribeReplicationSubnetGroupsResponse,

    -- * Response Lenses
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { -- | Filters applied to replication subnet groups.
    --
    -- Valid filter names: replication-subnet-group-id
    filters :: Prelude.Maybe [Filter],
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReplicationSubnetGroups_filters' - Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
--
-- 'marker', 'describeReplicationSubnetGroups_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReplicationSubnetGroups ::
  DescribeReplicationSubnetGroups
newDescribeReplicationSubnetGroups =
  DescribeReplicationSubnetGroups'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
describeReplicationSubnetGroups_filters :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe [Filter])
describeReplicationSubnetGroups_filters = Lens.lens (\DescribeReplicationSubnetGroups' {filters} -> filters) (\s@DescribeReplicationSubnetGroups' {} a -> s {filters = a} :: DescribeReplicationSubnetGroups) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroups_marker :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe Prelude.Text)
describeReplicationSubnetGroups_marker = Lens.lens (\DescribeReplicationSubnetGroups' {marker} -> marker) (\s@DescribeReplicationSubnetGroups' {} a -> s {marker = a} :: DescribeReplicationSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationSubnetGroups_maxRecords :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe Prelude.Int)
describeReplicationSubnetGroups_maxRecords = Lens.lens (\DescribeReplicationSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeReplicationSubnetGroups' {} a -> s {maxRecords = a} :: DescribeReplicationSubnetGroups)

instance
  Core.AWSPager
    DescribeReplicationSubnetGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_replicationSubnetGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeReplicationSubnetGroups_marker
              Lens..~ rs
              Lens.^? describeReplicationSubnetGroupsResponse_marker
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReplicationSubnetGroups
  where
  type
    AWSResponse DescribeReplicationSubnetGroups =
      DescribeReplicationSubnetGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationSubnetGroupsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "ReplicationSubnetGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationSubnetGroups
  where
  hashWithSalt
    _salt
    DescribeReplicationSubnetGroups' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeReplicationSubnetGroups
  where
  rnf DescribeReplicationSubnetGroups' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeReplicationSubnetGroups
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeReplicationSubnetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReplicationSubnetGroups where
  toJSON DescribeReplicationSubnetGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance Data.ToPath DescribeReplicationSubnetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReplicationSubnetGroups where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A description of the replication subnet groups.
    replicationSubnetGroups :: Prelude.Maybe [ReplicationSubnetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReplicationSubnetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'replicationSubnetGroups', 'describeReplicationSubnetGroupsResponse_replicationSubnetGroups' - A description of the replication subnet groups.
--
-- 'httpStatus', 'describeReplicationSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeReplicationSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationSubnetGroupsResponse
newDescribeReplicationSubnetGroupsResponse
  pHttpStatus_ =
    DescribeReplicationSubnetGroupsResponse'
      { marker =
          Prelude.Nothing,
        replicationSubnetGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroupsResponse_marker :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeReplicationSubnetGroupsResponse_marker = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {marker} -> marker) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeReplicationSubnetGroupsResponse)

-- | A description of the replication subnet groups.
describeReplicationSubnetGroupsResponse_replicationSubnetGroups :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Prelude.Maybe [ReplicationSubnetGroup])
describeReplicationSubnetGroupsResponse_replicationSubnetGroups = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {replicationSubnetGroups} -> replicationSubnetGroups) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {replicationSubnetGroups = a} :: DescribeReplicationSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeReplicationSubnetGroupsResponse Prelude.Int
describeReplicationSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeReplicationSubnetGroupsResponse
  where
  rnf DescribeReplicationSubnetGroupsResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf replicationSubnetGroups `Prelude.seq`
        Prelude.rnf httpStatus

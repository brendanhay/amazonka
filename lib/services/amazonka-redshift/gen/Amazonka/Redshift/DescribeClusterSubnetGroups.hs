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
-- Module      : Amazonka.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in your
-- Amazon Web Services account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all subnet groups that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- subnet groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, subnet groups
-- are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeClusterSubnetGroups
  ( -- * Creating a Request
    DescribeClusterSubnetGroups (..),
    newDescribeClusterSubnetGroups,

    -- * Request Lenses
    describeClusterSubnetGroups_clusterSubnetGroupName,
    describeClusterSubnetGroups_marker,
    describeClusterSubnetGroups_maxRecords,
    describeClusterSubnetGroups_tagKeys,
    describeClusterSubnetGroups_tagValues,

    -- * Destructuring the Response
    DescribeClusterSubnetGroupsResponse (..),
    newDescribeClusterSubnetGroupsResponse,

    -- * Response Lenses
    describeClusterSubnetGroupsResponse_clusterSubnetGroups,
    describeClusterSubnetGroupsResponse_marker,
    describeClusterSubnetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterSubnetGroups' smart constructor.
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'
  { -- | The name of the cluster subnet group for which information is requested.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterSubnetGroups
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A tag key or keys for which you want to return all matching cluster
    -- subnet groups that are associated with the specified key or keys. For
    -- example, suppose that you have subnet groups that are tagged with keys
    -- called @owner@ and @environment@. If you specify both of these tag keys
    -- in the request, Amazon Redshift returns a response with the subnet
    -- groups that have either or both of these tag keys associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A tag value or values for which you want to return all matching cluster
    -- subnet groups that are associated with the specified tag value or
    -- values. For example, suppose that you have subnet groups that are tagged
    -- with values called @admin@ and @test@. If you specify both of these tag
    -- values in the request, Amazon Redshift returns a response with the
    -- subnet groups that have either or both of these tag values associated
    -- with them.
    tagValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroupName', 'describeClusterSubnetGroups_clusterSubnetGroupName' - The name of the cluster subnet group for which information is requested.
--
-- 'marker', 'describeClusterSubnetGroups_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'describeClusterSubnetGroups_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'tagKeys', 'describeClusterSubnetGroups_tagKeys' - A tag key or keys for which you want to return all matching cluster
-- subnet groups that are associated with the specified key or keys. For
-- example, suppose that you have subnet groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the subnet
-- groups that have either or both of these tag keys associated with them.
--
-- 'tagValues', 'describeClusterSubnetGroups_tagValues' - A tag value or values for which you want to return all matching cluster
-- subnet groups that are associated with the specified tag value or
-- values. For example, suppose that you have subnet groups that are tagged
-- with values called @admin@ and @test@. If you specify both of these tag
-- values in the request, Amazon Redshift returns a response with the
-- subnet groups that have either or both of these tag values associated
-- with them.
newDescribeClusterSubnetGroups ::
  DescribeClusterSubnetGroups
newDescribeClusterSubnetGroups =
  DescribeClusterSubnetGroups'
    { clusterSubnetGroupName =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
describeClusterSubnetGroups_clusterSubnetGroupName :: Lens.Lens' DescribeClusterSubnetGroups (Prelude.Maybe Prelude.Text)
describeClusterSubnetGroups_clusterSubnetGroupName = Lens.lens (\DescribeClusterSubnetGroups' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@DescribeClusterSubnetGroups' {} a -> s {clusterSubnetGroupName = a} :: DescribeClusterSubnetGroups)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeClusterSubnetGroups_marker :: Lens.Lens' DescribeClusterSubnetGroups (Prelude.Maybe Prelude.Text)
describeClusterSubnetGroups_marker = Lens.lens (\DescribeClusterSubnetGroups' {marker} -> marker) (\s@DescribeClusterSubnetGroups' {} a -> s {marker = a} :: DescribeClusterSubnetGroups)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusterSubnetGroups_maxRecords :: Lens.Lens' DescribeClusterSubnetGroups (Prelude.Maybe Prelude.Int)
describeClusterSubnetGroups_maxRecords = Lens.lens (\DescribeClusterSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeClusterSubnetGroups' {} a -> s {maxRecords = a} :: DescribeClusterSubnetGroups)

-- | A tag key or keys for which you want to return all matching cluster
-- subnet groups that are associated with the specified key or keys. For
-- example, suppose that you have subnet groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the subnet
-- groups that have either or both of these tag keys associated with them.
describeClusterSubnetGroups_tagKeys :: Lens.Lens' DescribeClusterSubnetGroups (Prelude.Maybe [Prelude.Text])
describeClusterSubnetGroups_tagKeys = Lens.lens (\DescribeClusterSubnetGroups' {tagKeys} -> tagKeys) (\s@DescribeClusterSubnetGroups' {} a -> s {tagKeys = a} :: DescribeClusterSubnetGroups) Prelude.. Lens.mapping Lens.coerced

-- | A tag value or values for which you want to return all matching cluster
-- subnet groups that are associated with the specified tag value or
-- values. For example, suppose that you have subnet groups that are tagged
-- with values called @admin@ and @test@. If you specify both of these tag
-- values in the request, Amazon Redshift returns a response with the
-- subnet groups that have either or both of these tag values associated
-- with them.
describeClusterSubnetGroups_tagValues :: Lens.Lens' DescribeClusterSubnetGroups (Prelude.Maybe [Prelude.Text])
describeClusterSubnetGroups_tagValues = Lens.lens (\DescribeClusterSubnetGroups' {tagValues} -> tagValues) (\s@DescribeClusterSubnetGroups' {} a -> s {tagValues = a} :: DescribeClusterSubnetGroups) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeClusterSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterSubnetGroupsResponse_clusterSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClusterSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeClusterSubnetGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusterSubnetGroups where
  type
    AWSResponse DescribeClusterSubnetGroups =
      DescribeClusterSubnetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeClusterSubnetGroupsResult"
      ( \s h x ->
          DescribeClusterSubnetGroupsResponse'
            Prelude.<$> ( x Data..@? "ClusterSubnetGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ClusterSubnetGroup")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusterSubnetGroups where
  hashWithSalt _salt DescribeClusterSubnetGroups' {..} =
    _salt `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData DescribeClusterSubnetGroups where
  rnf DescribeClusterSubnetGroups' {..} =
    Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToHeaders DescribeClusterSubnetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClusterSubnetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusterSubnetGroups where
  toQuery DescribeClusterSubnetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeClusterSubnetGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterSubnetGroupName"
          Data.=: clusterSubnetGroupName,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues)
      ]

-- | Contains the output from the DescribeClusterSubnetGroups action.
--
-- /See:/ 'newDescribeClusterSubnetGroupsResponse' smart constructor.
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'
  { -- | A list of ClusterSubnetGroup instances.
    clusterSubnetGroups :: Prelude.Maybe [ClusterSubnetGroup],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroups', 'describeClusterSubnetGroupsResponse_clusterSubnetGroups' - A list of ClusterSubnetGroup instances.
--
-- 'marker', 'describeClusterSubnetGroupsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClusterSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeClusterSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterSubnetGroupsResponse
newDescribeClusterSubnetGroupsResponse pHttpStatus_ =
  DescribeClusterSubnetGroupsResponse'
    { clusterSubnetGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ClusterSubnetGroup instances.
describeClusterSubnetGroupsResponse_clusterSubnetGroups :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Prelude.Maybe [ClusterSubnetGroup])
describeClusterSubnetGroupsResponse_clusterSubnetGroups = Lens.lens (\DescribeClusterSubnetGroupsResponse' {clusterSubnetGroups} -> clusterSubnetGroups) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {clusterSubnetGroups = a} :: DescribeClusterSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterSubnetGroupsResponse_marker :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeClusterSubnetGroupsResponse_marker = Lens.lens (\DescribeClusterSubnetGroupsResponse' {marker} -> marker) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeClusterSubnetGroupsResponse)

-- | The response's http status code.
describeClusterSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeClusterSubnetGroupsResponse Prelude.Int
describeClusterSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeClusterSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeClusterSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeClusterSubnetGroupsResponse
  where
  rnf DescribeClusterSubnetGroupsResponse' {..} =
    Prelude.rnf clusterSubnetGroups
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

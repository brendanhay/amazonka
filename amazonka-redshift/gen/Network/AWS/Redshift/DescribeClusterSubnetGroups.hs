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
-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account.
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
module Network.AWS.Redshift.DescribeClusterSubnetGroups
  ( -- * Creating a Request
    DescribeClusterSubnetGroups (..),
    newDescribeClusterSubnetGroups,

    -- * Request Lenses
    describeClusterSubnetGroups_clusterSubnetGroupName,
    describeClusterSubnetGroups_tagKeys,
    describeClusterSubnetGroups_tagValues,
    describeClusterSubnetGroups_marker,
    describeClusterSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeClusterSubnetGroupsResponse (..),
    newDescribeClusterSubnetGroupsResponse,

    -- * Response Lenses
    describeClusterSubnetGroupsResponse_clusterSubnetGroups,
    describeClusterSubnetGroupsResponse_marker,
    describeClusterSubnetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterSubnetGroups' smart constructor.
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'
  { -- | The name of the cluster subnet group for which information is requested.
    clusterSubnetGroupName :: Core.Maybe Core.Text,
    -- | A tag key or keys for which you want to return all matching cluster
    -- subnet groups that are associated with the specified key or keys. For
    -- example, suppose that you have subnet groups that are tagged with keys
    -- called @owner@ and @environment@. If you specify both of these tag keys
    -- in the request, Amazon Redshift returns a response with the subnet
    -- groups that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Core.Text],
    -- | A tag value or values for which you want to return all matching cluster
    -- subnet groups that are associated with the specified tag value or
    -- values. For example, suppose that you have subnet groups that are tagged
    -- with values called @admin@ and @test@. If you specify both of these tag
    -- values in the request, Amazon Redshift returns a response with the
    -- subnet groups that have either or both of these tag values associated
    -- with them.
    tagValues :: Core.Maybe [Core.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterSubnetGroups
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
--
-- 'marker', 'describeClusterSubnetGroups_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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
newDescribeClusterSubnetGroups ::
  DescribeClusterSubnetGroups
newDescribeClusterSubnetGroups =
  DescribeClusterSubnetGroups'
    { clusterSubnetGroupName =
        Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
describeClusterSubnetGroups_clusterSubnetGroupName :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Core.Text)
describeClusterSubnetGroups_clusterSubnetGroupName = Lens.lens (\DescribeClusterSubnetGroups' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@DescribeClusterSubnetGroups' {} a -> s {clusterSubnetGroupName = a} :: DescribeClusterSubnetGroups)

-- | A tag key or keys for which you want to return all matching cluster
-- subnet groups that are associated with the specified key or keys. For
-- example, suppose that you have subnet groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the subnet
-- groups that have either or both of these tag keys associated with them.
describeClusterSubnetGroups_tagKeys :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe [Core.Text])
describeClusterSubnetGroups_tagKeys = Lens.lens (\DescribeClusterSubnetGroups' {tagKeys} -> tagKeys) (\s@DescribeClusterSubnetGroups' {} a -> s {tagKeys = a} :: DescribeClusterSubnetGroups) Core.. Lens.mapping Lens._Coerce

-- | A tag value or values for which you want to return all matching cluster
-- subnet groups that are associated with the specified tag value or
-- values. For example, suppose that you have subnet groups that are tagged
-- with values called @admin@ and @test@. If you specify both of these tag
-- values in the request, Amazon Redshift returns a response with the
-- subnet groups that have either or both of these tag values associated
-- with them.
describeClusterSubnetGroups_tagValues :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe [Core.Text])
describeClusterSubnetGroups_tagValues = Lens.lens (\DescribeClusterSubnetGroups' {tagValues} -> tagValues) (\s@DescribeClusterSubnetGroups' {} a -> s {tagValues = a} :: DescribeClusterSubnetGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeClusterSubnetGroups_marker :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Core.Text)
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
describeClusterSubnetGroups_maxRecords :: Lens.Lens' DescribeClusterSubnetGroups (Core.Maybe Core.Int)
describeClusterSubnetGroups_maxRecords = Lens.lens (\DescribeClusterSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeClusterSubnetGroups' {} a -> s {maxRecords = a} :: DescribeClusterSubnetGroups)

instance Core.AWSPager DescribeClusterSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterSubnetGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterSubnetGroupsResponse_clusterSubnetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClusterSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeClusterSubnetGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeClusterSubnetGroups where
  type
    AWSResponse DescribeClusterSubnetGroups =
      DescribeClusterSubnetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterSubnetGroupsResult"
      ( \s h x ->
          DescribeClusterSubnetGroupsResponse'
            Core.<$> ( x Core..@? "ClusterSubnetGroups"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ClusterSubnetGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusterSubnetGroups

instance Core.NFData DescribeClusterSubnetGroups

instance Core.ToHeaders DescribeClusterSubnetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClusterSubnetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusterSubnetGroups where
  toQuery DescribeClusterSubnetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClusterSubnetGroups" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterSubnetGroupName"
          Core.=: clusterSubnetGroupName,
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Core.<$> tagKeys),
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Core.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the output from the DescribeClusterSubnetGroups action.
--
-- /See:/ 'newDescribeClusterSubnetGroupsResponse' smart constructor.
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'
  { -- | A list of ClusterSubnetGroup instances.
    clusterSubnetGroups :: Core.Maybe [ClusterSubnetGroup],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeClusterSubnetGroupsResponse
newDescribeClusterSubnetGroupsResponse pHttpStatus_ =
  DescribeClusterSubnetGroupsResponse'
    { clusterSubnetGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ClusterSubnetGroup instances.
describeClusterSubnetGroupsResponse_clusterSubnetGroups :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Core.Maybe [ClusterSubnetGroup])
describeClusterSubnetGroupsResponse_clusterSubnetGroups = Lens.lens (\DescribeClusterSubnetGroupsResponse' {clusterSubnetGroups} -> clusterSubnetGroups) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {clusterSubnetGroups = a} :: DescribeClusterSubnetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterSubnetGroupsResponse_marker :: Lens.Lens' DescribeClusterSubnetGroupsResponse (Core.Maybe Core.Text)
describeClusterSubnetGroupsResponse_marker = Lens.lens (\DescribeClusterSubnetGroupsResponse' {marker} -> marker) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeClusterSubnetGroupsResponse)

-- | The response's http status code.
describeClusterSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeClusterSubnetGroupsResponse Core.Int
describeClusterSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeClusterSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeClusterSubnetGroupsResponse)

instance
  Core.NFData
    DescribeClusterSubnetGroupsResponse

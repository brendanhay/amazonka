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
-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to
-- retrieve the description of a specific parameter group.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all parameter groups that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- parameter groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, parameter
-- groups are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameterGroups
  ( -- * Creating a Request
    DescribeClusterParameterGroups (..),
    newDescribeClusterParameterGroups,

    -- * Request Lenses
    describeClusterParameterGroups_tagKeys,
    describeClusterParameterGroups_parameterGroupName,
    describeClusterParameterGroups_tagValues,
    describeClusterParameterGroups_marker,
    describeClusterParameterGroups_maxRecords,

    -- * Destructuring the Response
    DescribeClusterParameterGroupsResponse (..),
    newDescribeClusterParameterGroupsResponse,

    -- * Response Lenses
    describeClusterParameterGroupsResponse_parameterGroups,
    describeClusterParameterGroupsResponse_marker,
    describeClusterParameterGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterParameterGroups' smart constructor.
data DescribeClusterParameterGroups = DescribeClusterParameterGroups'
  { -- | A tag key or keys for which you want to return all matching cluster
    -- parameter groups that are associated with the specified key or keys. For
    -- example, suppose that you have parameter groups that are tagged with
    -- keys called @owner@ and @environment@. If you specify both of these tag
    -- keys in the request, Amazon Redshift returns a response with the
    -- parameter groups that have either or both of these tag keys associated
    -- with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | The name of a specific parameter group for which to return details. By
    -- default, details about all parameter groups and the default parameter
    -- group are returned.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A tag value or values for which you want to return all matching cluster
    -- parameter groups that are associated with the specified tag value or
    -- values. For example, suppose that you have parameter groups that are
    -- tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the parameter groups that have either or both of these tag values
    -- associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeClusterParameterGroups request exceed the value specified in
    -- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
    -- You can retrieve the next set of response records by providing the
    -- returned marker value in the @Marker@ parameter and retrying the
    -- request.
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'describeClusterParameterGroups_tagKeys' - A tag key or keys for which you want to return all matching cluster
-- parameter groups that are associated with the specified key or keys. For
-- example, suppose that you have parameter groups that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the
-- parameter groups that have either or both of these tag keys associated
-- with them.
--
-- 'parameterGroupName', 'describeClusterParameterGroups_parameterGroupName' - The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
--
-- 'tagValues', 'describeClusterParameterGroups_tagValues' - A tag value or values for which you want to return all matching cluster
-- parameter groups that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the parameter groups that have either or both of these tag values
-- associated with them.
--
-- 'marker', 'describeClusterParameterGroups_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeClusterParameterGroups request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
--
-- 'maxRecords', 'describeClusterParameterGroups_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeClusterParameterGroups ::
  DescribeClusterParameterGroups
newDescribeClusterParameterGroups =
  DescribeClusterParameterGroups'
    { tagKeys =
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | A tag key or keys for which you want to return all matching cluster
-- parameter groups that are associated with the specified key or keys. For
-- example, suppose that you have parameter groups that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the
-- parameter groups that have either or both of these tag keys associated
-- with them.
describeClusterParameterGroups_tagKeys :: Lens.Lens' DescribeClusterParameterGroups (Prelude.Maybe [Prelude.Text])
describeClusterParameterGroups_tagKeys = Lens.lens (\DescribeClusterParameterGroups' {tagKeys} -> tagKeys) (\s@DescribeClusterParameterGroups' {} a -> s {tagKeys = a} :: DescribeClusterParameterGroups) Prelude.. Lens.mapping Lens._Coerce

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
describeClusterParameterGroups_parameterGroupName :: Lens.Lens' DescribeClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeClusterParameterGroups_parameterGroupName = Lens.lens (\DescribeClusterParameterGroups' {parameterGroupName} -> parameterGroupName) (\s@DescribeClusterParameterGroups' {} a -> s {parameterGroupName = a} :: DescribeClusterParameterGroups)

-- | A tag value or values for which you want to return all matching cluster
-- parameter groups that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the parameter groups that have either or both of these tag values
-- associated with them.
describeClusterParameterGroups_tagValues :: Lens.Lens' DescribeClusterParameterGroups (Prelude.Maybe [Prelude.Text])
describeClusterParameterGroups_tagValues = Lens.lens (\DescribeClusterParameterGroups' {tagValues} -> tagValues) (\s@DescribeClusterParameterGroups' {} a -> s {tagValues = a} :: DescribeClusterParameterGroups) Prelude.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeClusterParameterGroups request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
describeClusterParameterGroups_marker :: Lens.Lens' DescribeClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeClusterParameterGroups_marker = Lens.lens (\DescribeClusterParameterGroups' {marker} -> marker) (\s@DescribeClusterParameterGroups' {} a -> s {marker = a} :: DescribeClusterParameterGroups)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusterParameterGroups_maxRecords :: Lens.Lens' DescribeClusterParameterGroups (Prelude.Maybe Prelude.Int)
describeClusterParameterGroups_maxRecords = Lens.lens (\DescribeClusterParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeClusterParameterGroups' {} a -> s {maxRecords = a} :: DescribeClusterParameterGroups)

instance Core.AWSPager DescribeClusterParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterParameterGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterParameterGroupsResponse_parameterGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClusterParameterGroups_marker
          Lens..~ rs
          Lens.^? describeClusterParameterGroupsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeClusterParameterGroups
  where
  type
    AWSResponse DescribeClusterParameterGroups =
      DescribeClusterParameterGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterParameterGroupsResult"
      ( \s h x ->
          DescribeClusterParameterGroupsResponse'
            Prelude.<$> ( x Core..@? "ParameterGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ClusterParameterGroup")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClusterParameterGroups

instance
  Prelude.NFData
    DescribeClusterParameterGroups

instance
  Core.ToHeaders
    DescribeClusterParameterGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeClusterParameterGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClusterParameterGroups where
  toQuery DescribeClusterParameterGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeClusterParameterGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Prelude.<$> tagKeys),
        "ParameterGroupName" Core.=: parameterGroupName,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Prelude.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the output from the DescribeClusterParameterGroups action.
--
-- /See:/ 'newDescribeClusterParameterGroupsResponse' smart constructor.
data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse'
  { -- | A list of ClusterParameterGroup instances. Each instance describes one
    -- cluster parameter group.
    parameterGroups :: Prelude.Maybe [ClusterParameterGroup],
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
-- Create a value of 'DescribeClusterParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroups', 'describeClusterParameterGroupsResponse_parameterGroups' - A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
--
-- 'marker', 'describeClusterParameterGroupsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClusterParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeClusterParameterGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterParameterGroupsResponse
newDescribeClusterParameterGroupsResponse
  pHttpStatus_ =
    DescribeClusterParameterGroupsResponse'
      { parameterGroups =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
describeClusterParameterGroupsResponse_parameterGroups :: Lens.Lens' DescribeClusterParameterGroupsResponse (Prelude.Maybe [ClusterParameterGroup])
describeClusterParameterGroupsResponse_parameterGroups = Lens.lens (\DescribeClusterParameterGroupsResponse' {parameterGroups} -> parameterGroups) (\s@DescribeClusterParameterGroupsResponse' {} a -> s {parameterGroups = a} :: DescribeClusterParameterGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterParameterGroupsResponse_marker :: Lens.Lens' DescribeClusterParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeClusterParameterGroupsResponse_marker = Lens.lens (\DescribeClusterParameterGroupsResponse' {marker} -> marker) (\s@DescribeClusterParameterGroupsResponse' {} a -> s {marker = a} :: DescribeClusterParameterGroupsResponse)

-- | The response's http status code.
describeClusterParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeClusterParameterGroupsResponse Prelude.Int
describeClusterParameterGroupsResponse_httpStatus = Lens.lens (\DescribeClusterParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeClusterParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeClusterParameterGroupsResponse

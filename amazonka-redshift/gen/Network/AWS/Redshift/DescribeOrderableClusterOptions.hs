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
-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available,
-- such as the EC2 Availability Zones (AZ) in the specific AWS Region that
-- you can specify, and the node types you can request. The node types
-- differ by available storage, memory, CPU and price. With the cost
-- involved you might want to obtain a list of cluster options in the
-- specific region and specify values when creating a cluster. For more
-- information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeOrderableClusterOptions
  ( -- * Creating a Request
    DescribeOrderableClusterOptions (..),
    newDescribeOrderableClusterOptions,

    -- * Request Lenses
    describeOrderableClusterOptions_nodeType,
    describeOrderableClusterOptions_clusterVersion,
    describeOrderableClusterOptions_marker,
    describeOrderableClusterOptions_maxRecords,

    -- * Destructuring the Response
    DescribeOrderableClusterOptionsResponse (..),
    newDescribeOrderableClusterOptionsResponse,

    -- * Response Lenses
    describeOrderableClusterOptionsResponse_orderableClusterOptions,
    describeOrderableClusterOptionsResponse_marker,
    describeOrderableClusterOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOrderableClusterOptions' smart constructor.
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions'
  { -- | The node type filter value. Specify this parameter to show only the
    -- available offerings matching the specified node type.
    nodeType :: Core.Maybe Core.Text,
    -- | The version filter value. Specify this parameter to show only the
    -- available offerings matching the specified version.
    --
    -- Default: All versions.
    --
    -- Constraints: Must be one of the version returned from
    -- DescribeClusterVersions.
    clusterVersion :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeOrderableClusterOptions request exceed the value specified in
    -- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
    -- You can retrieve the next set of response records by providing the
    -- returned marker value in the @Marker@ parameter and retrying the
    -- request.
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
-- Create a value of 'DescribeOrderableClusterOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeType', 'describeOrderableClusterOptions_nodeType' - The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
--
-- 'clusterVersion', 'describeOrderableClusterOptions_clusterVersion' - The version filter value. Specify this parameter to show only the
-- available offerings matching the specified version.
--
-- Default: All versions.
--
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
--
-- 'marker', 'describeOrderableClusterOptions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeOrderableClusterOptions request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
--
-- 'maxRecords', 'describeOrderableClusterOptions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeOrderableClusterOptions ::
  DescribeOrderableClusterOptions
newDescribeOrderableClusterOptions =
  DescribeOrderableClusterOptions'
    { nodeType =
        Core.Nothing,
      clusterVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
describeOrderableClusterOptions_nodeType :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
describeOrderableClusterOptions_nodeType = Lens.lens (\DescribeOrderableClusterOptions' {nodeType} -> nodeType) (\s@DescribeOrderableClusterOptions' {} a -> s {nodeType = a} :: DescribeOrderableClusterOptions)

-- | The version filter value. Specify this parameter to show only the
-- available offerings matching the specified version.
--
-- Default: All versions.
--
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
describeOrderableClusterOptions_clusterVersion :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
describeOrderableClusterOptions_clusterVersion = Lens.lens (\DescribeOrderableClusterOptions' {clusterVersion} -> clusterVersion) (\s@DescribeOrderableClusterOptions' {} a -> s {clusterVersion = a} :: DescribeOrderableClusterOptions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeOrderableClusterOptions request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
describeOrderableClusterOptions_marker :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Text)
describeOrderableClusterOptions_marker = Lens.lens (\DescribeOrderableClusterOptions' {marker} -> marker) (\s@DescribeOrderableClusterOptions' {} a -> s {marker = a} :: DescribeOrderableClusterOptions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeOrderableClusterOptions_maxRecords :: Lens.Lens' DescribeOrderableClusterOptions (Core.Maybe Core.Int)
describeOrderableClusterOptions_maxRecords = Lens.lens (\DescribeOrderableClusterOptions' {maxRecords} -> maxRecords) (\s@DescribeOrderableClusterOptions' {} a -> s {maxRecords = a} :: DescribeOrderableClusterOptions)

instance
  Core.AWSPager
    DescribeOrderableClusterOptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrderableClusterOptionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrderableClusterOptionsResponse_orderableClusterOptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeOrderableClusterOptions_marker
          Lens..~ rs
          Lens.^? describeOrderableClusterOptionsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrderableClusterOptions
  where
  type
    AWSResponse DescribeOrderableClusterOptions =
      DescribeOrderableClusterOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeOrderableClusterOptionsResult"
      ( \s h x ->
          DescribeOrderableClusterOptionsResponse'
            Core.<$> ( x Core..@? "OrderableClusterOptions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "OrderableClusterOption")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeOrderableClusterOptions

instance Core.NFData DescribeOrderableClusterOptions

instance
  Core.ToHeaders
    DescribeOrderableClusterOptions
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeOrderableClusterOptions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeOrderableClusterOptions where
  toQuery DescribeOrderableClusterOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeOrderableClusterOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "NodeType" Core.=: nodeType,
        "ClusterVersion" Core.=: clusterVersion,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the output from the DescribeOrderableClusterOptions action.
--
-- /See:/ 'newDescribeOrderableClusterOptionsResponse' smart constructor.
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse'
  { -- | An @OrderableClusterOption@ structure containing information about
    -- orderable options for the cluster.
    orderableClusterOptions :: Core.Maybe [OrderableClusterOption],
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
-- Create a value of 'DescribeOrderableClusterOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderableClusterOptions', 'describeOrderableClusterOptionsResponse_orderableClusterOptions' - An @OrderableClusterOption@ structure containing information about
-- orderable options for the cluster.
--
-- 'marker', 'describeOrderableClusterOptionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeOrderableClusterOptionsResponse_httpStatus' - The response's http status code.
newDescribeOrderableClusterOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOrderableClusterOptionsResponse
newDescribeOrderableClusterOptionsResponse
  pHttpStatus_ =
    DescribeOrderableClusterOptionsResponse'
      { orderableClusterOptions =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An @OrderableClusterOption@ structure containing information about
-- orderable options for the cluster.
describeOrderableClusterOptionsResponse_orderableClusterOptions :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Core.Maybe [OrderableClusterOption])
describeOrderableClusterOptionsResponse_orderableClusterOptions = Lens.lens (\DescribeOrderableClusterOptionsResponse' {orderableClusterOptions} -> orderableClusterOptions) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {orderableClusterOptions = a} :: DescribeOrderableClusterOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeOrderableClusterOptionsResponse_marker :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Core.Maybe Core.Text)
describeOrderableClusterOptionsResponse_marker = Lens.lens (\DescribeOrderableClusterOptionsResponse' {marker} -> marker) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {marker = a} :: DescribeOrderableClusterOptionsResponse)

-- | The response's http status code.
describeOrderableClusterOptionsResponse_httpStatus :: Lens.Lens' DescribeOrderableClusterOptionsResponse Core.Int
describeOrderableClusterOptionsResponse_httpStatus = Lens.lens (\DescribeOrderableClusterOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {httpStatus = a} :: DescribeOrderableClusterOptionsResponse)

instance
  Core.NFData
    DescribeOrderableClusterOptionsResponse

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
-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of the available Amazon Redshift cluster versions.
-- You can call this operation even before creating any clusters to learn
-- more about the Amazon Redshift versions. For more information about
-- managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterVersions
  ( -- * Creating a Request
    DescribeClusterVersions (..),
    newDescribeClusterVersions,

    -- * Request Lenses
    describeClusterVersions_clusterParameterGroupFamily,
    describeClusterVersions_clusterVersion,
    describeClusterVersions_marker,
    describeClusterVersions_maxRecords,

    -- * Destructuring the Response
    DescribeClusterVersionsResponse (..),
    newDescribeClusterVersionsResponse,

    -- * Response Lenses
    describeClusterVersionsResponse_clusterVersions,
    describeClusterVersionsResponse_marker,
    describeClusterVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterVersions' smart constructor.
data DescribeClusterVersions = DescribeClusterVersions'
  { -- | The name of a specific cluster parameter group family to return details
    -- for.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    clusterParameterGroupFamily :: Core.Maybe Core.Text,
    -- | The specific cluster version to return.
    --
    -- Example: @1.0@
    clusterVersion :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterVersions
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
-- Create a value of 'DescribeClusterVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterParameterGroupFamily', 'describeClusterVersions_clusterParameterGroupFamily' - The name of a specific cluster parameter group family to return details
-- for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'clusterVersion', 'describeClusterVersions_clusterVersion' - The specific cluster version to return.
--
-- Example: @1.0@
--
-- 'marker', 'describeClusterVersions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterVersions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeClusterVersions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeClusterVersions ::
  DescribeClusterVersions
newDescribeClusterVersions =
  DescribeClusterVersions'
    { clusterParameterGroupFamily =
        Core.Nothing,
      clusterVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific cluster parameter group family to return details
-- for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
describeClusterVersions_clusterParameterGroupFamily :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
describeClusterVersions_clusterParameterGroupFamily = Lens.lens (\DescribeClusterVersions' {clusterParameterGroupFamily} -> clusterParameterGroupFamily) (\s@DescribeClusterVersions' {} a -> s {clusterParameterGroupFamily = a} :: DescribeClusterVersions)

-- | The specific cluster version to return.
--
-- Example: @1.0@
describeClusterVersions_clusterVersion :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
describeClusterVersions_clusterVersion = Lens.lens (\DescribeClusterVersions' {clusterVersion} -> clusterVersion) (\s@DescribeClusterVersions' {} a -> s {clusterVersion = a} :: DescribeClusterVersions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterVersions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeClusterVersions_marker :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Text)
describeClusterVersions_marker = Lens.lens (\DescribeClusterVersions' {marker} -> marker) (\s@DescribeClusterVersions' {} a -> s {marker = a} :: DescribeClusterVersions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusterVersions_maxRecords :: Lens.Lens' DescribeClusterVersions (Core.Maybe Core.Int)
describeClusterVersions_maxRecords = Lens.lens (\DescribeClusterVersions' {maxRecords} -> maxRecords) (\s@DescribeClusterVersions' {} a -> s {maxRecords = a} :: DescribeClusterVersions)

instance Core.AWSPager DescribeClusterVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterVersionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterVersionsResponse_clusterVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClusterVersions_marker
          Lens..~ rs
          Lens.^? describeClusterVersionsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeClusterVersions where
  type
    AWSResponse DescribeClusterVersions =
      DescribeClusterVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterVersionsResult"
      ( \s h x ->
          DescribeClusterVersionsResponse'
            Core.<$> ( x Core..@? "ClusterVersions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ClusterVersion")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusterVersions

instance Core.NFData DescribeClusterVersions

instance Core.ToHeaders DescribeClusterVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClusterVersions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusterVersions where
  toQuery DescribeClusterVersions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClusterVersions" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterParameterGroupFamily"
          Core.=: clusterParameterGroupFamily,
        "ClusterVersion" Core.=: clusterVersion,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the output from the DescribeClusterVersions action.
--
-- /See:/ 'newDescribeClusterVersionsResponse' smart constructor.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
  { -- | A list of @Version@ elements.
    clusterVersions :: Core.Maybe [ClusterVersion],
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
-- Create a value of 'DescribeClusterVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterVersions', 'describeClusterVersionsResponse_clusterVersions' - A list of @Version@ elements.
--
-- 'marker', 'describeClusterVersionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClusterVersionsResponse_httpStatus' - The response's http status code.
newDescribeClusterVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClusterVersionsResponse
newDescribeClusterVersionsResponse pHttpStatus_ =
  DescribeClusterVersionsResponse'
    { clusterVersions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @Version@ elements.
describeClusterVersionsResponse_clusterVersions :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe [ClusterVersion])
describeClusterVersionsResponse_clusterVersions = Lens.lens (\DescribeClusterVersionsResponse' {clusterVersions} -> clusterVersions) (\s@DescribeClusterVersionsResponse' {} a -> s {clusterVersions = a} :: DescribeClusterVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterVersionsResponse_marker :: Lens.Lens' DescribeClusterVersionsResponse (Core.Maybe Core.Text)
describeClusterVersionsResponse_marker = Lens.lens (\DescribeClusterVersionsResponse' {marker} -> marker) (\s@DescribeClusterVersionsResponse' {} a -> s {marker = a} :: DescribeClusterVersionsResponse)

-- | The response's http status code.
describeClusterVersionsResponse_httpStatus :: Lens.Lens' DescribeClusterVersionsResponse Core.Int
describeClusterVersionsResponse_httpStatus = Lens.lens (\DescribeClusterVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterVersionsResponse' {} a -> s {httpStatus = a} :: DescribeClusterVersionsResponse)

instance Core.NFData DescribeClusterVersionsResponse

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
-- Module      : Amazonka.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available,
-- such as the EC2 Availability Zones (AZ) in the specific Amazon Web
-- Services Region that you can specify, and the node types you can
-- request. The node types differ by available storage, memory, CPU and
-- price. With the cost involved you might want to obtain a list of cluster
-- options in the specific region and specify values when creating a
-- cluster. For more information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeOrderableClusterOptions
  ( -- * Creating a Request
    DescribeOrderableClusterOptions (..),
    newDescribeOrderableClusterOptions,

    -- * Request Lenses
    describeOrderableClusterOptions_clusterVersion,
    describeOrderableClusterOptions_marker,
    describeOrderableClusterOptions_maxRecords,
    describeOrderableClusterOptions_nodeType,

    -- * Destructuring the Response
    DescribeOrderableClusterOptionsResponse (..),
    newDescribeOrderableClusterOptionsResponse,

    -- * Response Lenses
    describeOrderableClusterOptionsResponse_marker,
    describeOrderableClusterOptionsResponse_orderableClusterOptions,
    describeOrderableClusterOptionsResponse_httpStatus,
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
-- /See:/ 'newDescribeOrderableClusterOptions' smart constructor.
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions'
  { -- | The version filter value. Specify this parameter to show only the
    -- available offerings matching the specified version.
    --
    -- Default: All versions.
    --
    -- Constraints: Must be one of the version returned from
    -- DescribeClusterVersions.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeOrderableClusterOptions request exceed the value specified in
    -- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
    -- of the response. You can retrieve the next set of response records by
    -- providing the returned marker value in the @Marker@ parameter and
    -- retrying the request.
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
    -- | The node type filter value. Specify this parameter to show only the
    -- available offerings matching the specified node type.
    nodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrderableClusterOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
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
--
-- 'nodeType', 'describeOrderableClusterOptions_nodeType' - The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
newDescribeOrderableClusterOptions ::
  DescribeOrderableClusterOptions
newDescribeOrderableClusterOptions =
  DescribeOrderableClusterOptions'
    { clusterVersion =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | The version filter value. Specify this parameter to show only the
-- available offerings matching the specified version.
--
-- Default: All versions.
--
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
describeOrderableClusterOptions_clusterVersion :: Lens.Lens' DescribeOrderableClusterOptions (Prelude.Maybe Prelude.Text)
describeOrderableClusterOptions_clusterVersion = Lens.lens (\DescribeOrderableClusterOptions' {clusterVersion} -> clusterVersion) (\s@DescribeOrderableClusterOptions' {} a -> s {clusterVersion = a} :: DescribeOrderableClusterOptions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeOrderableClusterOptions request exceed the value specified in
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
describeOrderableClusterOptions_marker :: Lens.Lens' DescribeOrderableClusterOptions (Prelude.Maybe Prelude.Text)
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
describeOrderableClusterOptions_maxRecords :: Lens.Lens' DescribeOrderableClusterOptions (Prelude.Maybe Prelude.Int)
describeOrderableClusterOptions_maxRecords = Lens.lens (\DescribeOrderableClusterOptions' {maxRecords} -> maxRecords) (\s@DescribeOrderableClusterOptions' {} a -> s {maxRecords = a} :: DescribeOrderableClusterOptions)

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
describeOrderableClusterOptions_nodeType :: Lens.Lens' DescribeOrderableClusterOptions (Prelude.Maybe Prelude.Text)
describeOrderableClusterOptions_nodeType = Lens.lens (\DescribeOrderableClusterOptions' {nodeType} -> nodeType) (\s@DescribeOrderableClusterOptions' {} a -> s {nodeType = a} :: DescribeOrderableClusterOptions)

instance
  Core.AWSPager
    DescribeOrderableClusterOptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrderableClusterOptionsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrderableClusterOptionsResponse_orderableClusterOptions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeOrderableClusterOptions_marker
              Lens..~ rs
              Lens.^? describeOrderableClusterOptionsResponse_marker
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrderableClusterOptions
  where
  type
    AWSResponse DescribeOrderableClusterOptions =
      DescribeOrderableClusterOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeOrderableClusterOptionsResult"
      ( \s h x ->
          DescribeOrderableClusterOptionsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "OrderableClusterOptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "OrderableClusterOption")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrderableClusterOptions
  where
  hashWithSalt
    _salt
    DescribeOrderableClusterOptions' {..} =
      _salt
        `Prelude.hashWithSalt` clusterVersion
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` nodeType

instance
  Prelude.NFData
    DescribeOrderableClusterOptions
  where
  rnf DescribeOrderableClusterOptions' {..} =
    Prelude.rnf clusterVersion `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords `Prelude.seq`
          Prelude.rnf nodeType

instance
  Data.ToHeaders
    DescribeOrderableClusterOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeOrderableClusterOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOrderableClusterOptions where
  toQuery DescribeOrderableClusterOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeOrderableClusterOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterVersion" Data.=: clusterVersion,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "NodeType" Data.=: nodeType
      ]

-- | Contains the output from the DescribeOrderableClusterOptions action.
--
-- /See:/ 'newDescribeOrderableClusterOptionsResponse' smart constructor.
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An @OrderableClusterOption@ structure containing information about
    -- orderable options for the cluster.
    orderableClusterOptions :: Prelude.Maybe [OrderableClusterOption],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrderableClusterOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeOrderableClusterOptionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'orderableClusterOptions', 'describeOrderableClusterOptionsResponse_orderableClusterOptions' - An @OrderableClusterOption@ structure containing information about
-- orderable options for the cluster.
--
-- 'httpStatus', 'describeOrderableClusterOptionsResponse_httpStatus' - The response's http status code.
newDescribeOrderableClusterOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrderableClusterOptionsResponse
newDescribeOrderableClusterOptionsResponse
  pHttpStatus_ =
    DescribeOrderableClusterOptionsResponse'
      { marker =
          Prelude.Nothing,
        orderableClusterOptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeOrderableClusterOptionsResponse_marker :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Prelude.Maybe Prelude.Text)
describeOrderableClusterOptionsResponse_marker = Lens.lens (\DescribeOrderableClusterOptionsResponse' {marker} -> marker) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {marker = a} :: DescribeOrderableClusterOptionsResponse)

-- | An @OrderableClusterOption@ structure containing information about
-- orderable options for the cluster.
describeOrderableClusterOptionsResponse_orderableClusterOptions :: Lens.Lens' DescribeOrderableClusterOptionsResponse (Prelude.Maybe [OrderableClusterOption])
describeOrderableClusterOptionsResponse_orderableClusterOptions = Lens.lens (\DescribeOrderableClusterOptionsResponse' {orderableClusterOptions} -> orderableClusterOptions) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {orderableClusterOptions = a} :: DescribeOrderableClusterOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrderableClusterOptionsResponse_httpStatus :: Lens.Lens' DescribeOrderableClusterOptionsResponse Prelude.Int
describeOrderableClusterOptionsResponse_httpStatus = Lens.lens (\DescribeOrderableClusterOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableClusterOptionsResponse' {} a -> s {httpStatus = a} :: DescribeOrderableClusterOptionsResponse)

instance
  Prelude.NFData
    DescribeOrderableClusterOptionsResponse
  where
  rnf DescribeOrderableClusterOptionsResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf orderableClusterOptions `Prelude.seq`
        Prelude.rnf httpStatus

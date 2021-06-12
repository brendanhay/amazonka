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
-- Module      : Network.AWS.Redshift.DescribeNodeConfigurationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of possible node configurations such as node type,
-- number of nodes, and disk usage for the specified action type.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeNodeConfigurationOptions
  ( -- * Creating a Request
    DescribeNodeConfigurationOptions (..),
    newDescribeNodeConfigurationOptions,

    -- * Request Lenses
    describeNodeConfigurationOptions_snapshotIdentifier,
    describeNodeConfigurationOptions_clusterIdentifier,
    describeNodeConfigurationOptions_filters,
    describeNodeConfigurationOptions_ownerAccount,
    describeNodeConfigurationOptions_marker,
    describeNodeConfigurationOptions_maxRecords,
    describeNodeConfigurationOptions_actionType,

    -- * Destructuring the Response
    DescribeNodeConfigurationOptionsResponse (..),
    newDescribeNodeConfigurationOptionsResponse,

    -- * Response Lenses
    describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList,
    describeNodeConfigurationOptionsResponse_marker,
    describeNodeConfigurationOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNodeConfigurationOptions' smart constructor.
data DescribeNodeConfigurationOptions = DescribeNodeConfigurationOptions'
  { -- | The identifier of the snapshot to evaluate for possible node
    -- configurations.
    snapshotIdentifier :: Core.Maybe Core.Text,
    -- | The identifier of the cluster to evaluate for possible node
    -- configurations.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | A set of name, operator, and value items to filter the results.
    filters :: Core.Maybe [NodeConfigurationOptionsFilter],
    -- | The AWS customer account used to create or copy the snapshot. Required
    -- if you are restoring a snapshot you do not own, optional if you own the
    -- snapshot.
    ownerAccount :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeNodeConfigurationOptions request exceed the value specified in
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
    -- Default: @500@
    --
    -- Constraints: minimum 100, maximum 500.
    maxRecords :: Core.Maybe Core.Int,
    -- | The action type to evaluate for possible node configurations. Specify
    -- \"restore-cluster\" to get configuration combinations based on an
    -- existing snapshot. Specify \"recommend-node-config\" to get
    -- configuration recommendations based on an existing cluster or snapshot.
    -- Specify \"resize-cluster\" to get configuration combinations for elastic
    -- resize based on an existing cluster.
    actionType :: ActionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNodeConfigurationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotIdentifier', 'describeNodeConfigurationOptions_snapshotIdentifier' - The identifier of the snapshot to evaluate for possible node
-- configurations.
--
-- 'clusterIdentifier', 'describeNodeConfigurationOptions_clusterIdentifier' - The identifier of the cluster to evaluate for possible node
-- configurations.
--
-- 'filters', 'describeNodeConfigurationOptions_filters' - A set of name, operator, and value items to filter the results.
--
-- 'ownerAccount', 'describeNodeConfigurationOptions_ownerAccount' - The AWS customer account used to create or copy the snapshot. Required
-- if you are restoring a snapshot you do not own, optional if you own the
-- snapshot.
--
-- 'marker', 'describeNodeConfigurationOptions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeNodeConfigurationOptions request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
--
-- 'maxRecords', 'describeNodeConfigurationOptions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @500@
--
-- Constraints: minimum 100, maximum 500.
--
-- 'actionType', 'describeNodeConfigurationOptions_actionType' - The action type to evaluate for possible node configurations. Specify
-- \"restore-cluster\" to get configuration combinations based on an
-- existing snapshot. Specify \"recommend-node-config\" to get
-- configuration recommendations based on an existing cluster or snapshot.
-- Specify \"resize-cluster\" to get configuration combinations for elastic
-- resize based on an existing cluster.
newDescribeNodeConfigurationOptions ::
  -- | 'actionType'
  ActionType ->
  DescribeNodeConfigurationOptions
newDescribeNodeConfigurationOptions pActionType_ =
  DescribeNodeConfigurationOptions'
    { snapshotIdentifier =
        Core.Nothing,
      clusterIdentifier = Core.Nothing,
      filters = Core.Nothing,
      ownerAccount = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      actionType = pActionType_
    }

-- | The identifier of the snapshot to evaluate for possible node
-- configurations.
describeNodeConfigurationOptions_snapshotIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
describeNodeConfigurationOptions_snapshotIdentifier = Lens.lens (\DescribeNodeConfigurationOptions' {snapshotIdentifier} -> snapshotIdentifier) (\s@DescribeNodeConfigurationOptions' {} a -> s {snapshotIdentifier = a} :: DescribeNodeConfigurationOptions)

-- | The identifier of the cluster to evaluate for possible node
-- configurations.
describeNodeConfigurationOptions_clusterIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
describeNodeConfigurationOptions_clusterIdentifier = Lens.lens (\DescribeNodeConfigurationOptions' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeNodeConfigurationOptions' {} a -> s {clusterIdentifier = a} :: DescribeNodeConfigurationOptions)

-- | A set of name, operator, and value items to filter the results.
describeNodeConfigurationOptions_filters :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe [NodeConfigurationOptionsFilter])
describeNodeConfigurationOptions_filters = Lens.lens (\DescribeNodeConfigurationOptions' {filters} -> filters) (\s@DescribeNodeConfigurationOptions' {} a -> s {filters = a} :: DescribeNodeConfigurationOptions) Core.. Lens.mapping Lens._Coerce

-- | The AWS customer account used to create or copy the snapshot. Required
-- if you are restoring a snapshot you do not own, optional if you own the
-- snapshot.
describeNodeConfigurationOptions_ownerAccount :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
describeNodeConfigurationOptions_ownerAccount = Lens.lens (\DescribeNodeConfigurationOptions' {ownerAccount} -> ownerAccount) (\s@DescribeNodeConfigurationOptions' {} a -> s {ownerAccount = a} :: DescribeNodeConfigurationOptions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeNodeConfigurationOptions request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
describeNodeConfigurationOptions_marker :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Text)
describeNodeConfigurationOptions_marker = Lens.lens (\DescribeNodeConfigurationOptions' {marker} -> marker) (\s@DescribeNodeConfigurationOptions' {} a -> s {marker = a} :: DescribeNodeConfigurationOptions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @500@
--
-- Constraints: minimum 100, maximum 500.
describeNodeConfigurationOptions_maxRecords :: Lens.Lens' DescribeNodeConfigurationOptions (Core.Maybe Core.Int)
describeNodeConfigurationOptions_maxRecords = Lens.lens (\DescribeNodeConfigurationOptions' {maxRecords} -> maxRecords) (\s@DescribeNodeConfigurationOptions' {} a -> s {maxRecords = a} :: DescribeNodeConfigurationOptions)

-- | The action type to evaluate for possible node configurations. Specify
-- \"restore-cluster\" to get configuration combinations based on an
-- existing snapshot. Specify \"recommend-node-config\" to get
-- configuration recommendations based on an existing cluster or snapshot.
-- Specify \"resize-cluster\" to get configuration combinations for elastic
-- resize based on an existing cluster.
describeNodeConfigurationOptions_actionType :: Lens.Lens' DescribeNodeConfigurationOptions ActionType
describeNodeConfigurationOptions_actionType = Lens.lens (\DescribeNodeConfigurationOptions' {actionType} -> actionType) (\s@DescribeNodeConfigurationOptions' {} a -> s {actionType = a} :: DescribeNodeConfigurationOptions)

instance
  Core.AWSPager
    DescribeNodeConfigurationOptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNodeConfigurationOptionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNodeConfigurationOptions_marker
          Lens..~ rs
          Lens.^? describeNodeConfigurationOptionsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeNodeConfigurationOptions
  where
  type
    AWSResponse DescribeNodeConfigurationOptions =
      DescribeNodeConfigurationOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeNodeConfigurationOptionsResult"
      ( \s h x ->
          DescribeNodeConfigurationOptionsResponse'
            Core.<$> ( x Core..@? "NodeConfigurationOptionList"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "NodeConfigurationOption")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeNodeConfigurationOptions

instance Core.NFData DescribeNodeConfigurationOptions

instance
  Core.ToHeaders
    DescribeNodeConfigurationOptions
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeNodeConfigurationOptions where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeNodeConfigurationOptions
  where
  toQuery DescribeNodeConfigurationOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeNodeConfigurationOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "Filter"
          Core.=: Core.toQuery
            ( Core.toQueryList "NodeConfigurationOptionsFilter"
                Core.<$> filters
            ),
        "OwnerAccount" Core.=: ownerAccount,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "ActionType" Core.=: actionType
      ]

-- | /See:/ 'newDescribeNodeConfigurationOptionsResponse' smart constructor.
data DescribeNodeConfigurationOptionsResponse = DescribeNodeConfigurationOptionsResponse'
  { -- | A list of valid node configurations.
    nodeConfigurationOptionList :: Core.Maybe [NodeConfigurationOption],
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
-- Create a value of 'DescribeNodeConfigurationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeConfigurationOptionList', 'describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList' - A list of valid node configurations.
--
-- 'marker', 'describeNodeConfigurationOptionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeNodeConfigurationOptionsResponse_httpStatus' - The response's http status code.
newDescribeNodeConfigurationOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNodeConfigurationOptionsResponse
newDescribeNodeConfigurationOptionsResponse
  pHttpStatus_ =
    DescribeNodeConfigurationOptionsResponse'
      { nodeConfigurationOptionList =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of valid node configurations.
describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe [NodeConfigurationOption])
describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {nodeConfigurationOptionList} -> nodeConfigurationOptionList) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {nodeConfigurationOptionList = a} :: DescribeNodeConfigurationOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeNodeConfigurationOptionsResponse_marker :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Core.Maybe Core.Text)
describeNodeConfigurationOptionsResponse_marker = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {marker} -> marker) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {marker = a} :: DescribeNodeConfigurationOptionsResponse)

-- | The response's http status code.
describeNodeConfigurationOptionsResponse_httpStatus :: Lens.Lens' DescribeNodeConfigurationOptionsResponse Core.Int
describeNodeConfigurationOptionsResponse_httpStatus = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {httpStatus = a} :: DescribeNodeConfigurationOptionsResponse)

instance
  Core.NFData
    DescribeNodeConfigurationOptionsResponse

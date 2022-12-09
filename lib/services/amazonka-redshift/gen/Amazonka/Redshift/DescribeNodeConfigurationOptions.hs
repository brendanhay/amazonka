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
-- Module      : Amazonka.Redshift.DescribeNodeConfigurationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of possible node configurations such as node type,
-- number of nodes, and disk usage for the specified action type.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeNodeConfigurationOptions
  ( -- * Creating a Request
    DescribeNodeConfigurationOptions (..),
    newDescribeNodeConfigurationOptions,

    -- * Request Lenses
    describeNodeConfigurationOptions_clusterIdentifier,
    describeNodeConfigurationOptions_filters,
    describeNodeConfigurationOptions_marker,
    describeNodeConfigurationOptions_maxRecords,
    describeNodeConfigurationOptions_ownerAccount,
    describeNodeConfigurationOptions_snapshotArn,
    describeNodeConfigurationOptions_snapshotIdentifier,
    describeNodeConfigurationOptions_actionType,

    -- * Destructuring the Response
    DescribeNodeConfigurationOptionsResponse (..),
    newDescribeNodeConfigurationOptionsResponse,

    -- * Response Lenses
    describeNodeConfigurationOptionsResponse_marker,
    describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList,
    describeNodeConfigurationOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNodeConfigurationOptions' smart constructor.
data DescribeNodeConfigurationOptions = DescribeNodeConfigurationOptions'
  { -- | The identifier of the cluster to evaluate for possible node
    -- configurations.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A set of name, operator, and value items to filter the results.
    filters :: Prelude.Maybe [NodeConfigurationOptionsFilter],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeNodeConfigurationOptions request exceed the value specified in
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
    -- Default: @500@
    --
    -- Constraints: minimum 100, maximum 500.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account used to create or copy the snapshot.
    -- Required if you are restoring a snapshot you do not own, optional if you
    -- own the snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot associated with the
    -- message to describe node configuration.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the snapshot to evaluate for possible node
    -- configurations.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The action type to evaluate for possible node configurations. Specify
    -- \"restore-cluster\" to get configuration combinations based on an
    -- existing snapshot. Specify \"recommend-node-config\" to get
    -- configuration recommendations based on an existing cluster or snapshot.
    -- Specify \"resize-cluster\" to get configuration combinations for elastic
    -- resize based on an existing cluster.
    actionType :: ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNodeConfigurationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeNodeConfigurationOptions_clusterIdentifier' - The identifier of the cluster to evaluate for possible node
-- configurations.
--
-- 'filters', 'describeNodeConfigurationOptions_filters' - A set of name, operator, and value items to filter the results.
--
-- 'marker', 'describeNodeConfigurationOptions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeNodeConfigurationOptions request exceed the value specified in
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
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
-- 'ownerAccount', 'describeNodeConfigurationOptions_ownerAccount' - The Amazon Web Services account used to create or copy the snapshot.
-- Required if you are restoring a snapshot you do not own, optional if you
-- own the snapshot.
--
-- 'snapshotArn', 'describeNodeConfigurationOptions_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot associated with the
-- message to describe node configuration.
--
-- 'snapshotIdentifier', 'describeNodeConfigurationOptions_snapshotIdentifier' - The identifier of the snapshot to evaluate for possible node
-- configurations.
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
    { clusterIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      snapshotArn = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      actionType = pActionType_
    }

-- | The identifier of the cluster to evaluate for possible node
-- configurations.
describeNodeConfigurationOptions_clusterIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeNodeConfigurationOptions_clusterIdentifier = Lens.lens (\DescribeNodeConfigurationOptions' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeNodeConfigurationOptions' {} a -> s {clusterIdentifier = a} :: DescribeNodeConfigurationOptions)

-- | A set of name, operator, and value items to filter the results.
describeNodeConfigurationOptions_filters :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe [NodeConfigurationOptionsFilter])
describeNodeConfigurationOptions_filters = Lens.lens (\DescribeNodeConfigurationOptions' {filters} -> filters) (\s@DescribeNodeConfigurationOptions' {} a -> s {filters = a} :: DescribeNodeConfigurationOptions) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeNodeConfigurationOptions request exceed the value specified in
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
describeNodeConfigurationOptions_marker :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Text)
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
describeNodeConfigurationOptions_maxRecords :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Int)
describeNodeConfigurationOptions_maxRecords = Lens.lens (\DescribeNodeConfigurationOptions' {maxRecords} -> maxRecords) (\s@DescribeNodeConfigurationOptions' {} a -> s {maxRecords = a} :: DescribeNodeConfigurationOptions)

-- | The Amazon Web Services account used to create or copy the snapshot.
-- Required if you are restoring a snapshot you do not own, optional if you
-- own the snapshot.
describeNodeConfigurationOptions_ownerAccount :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeNodeConfigurationOptions_ownerAccount = Lens.lens (\DescribeNodeConfigurationOptions' {ownerAccount} -> ownerAccount) (\s@DescribeNodeConfigurationOptions' {} a -> s {ownerAccount = a} :: DescribeNodeConfigurationOptions)

-- | The Amazon Resource Name (ARN) of the snapshot associated with the
-- message to describe node configuration.
describeNodeConfigurationOptions_snapshotArn :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeNodeConfigurationOptions_snapshotArn = Lens.lens (\DescribeNodeConfigurationOptions' {snapshotArn} -> snapshotArn) (\s@DescribeNodeConfigurationOptions' {} a -> s {snapshotArn = a} :: DescribeNodeConfigurationOptions)

-- | The identifier of the snapshot to evaluate for possible node
-- configurations.
describeNodeConfigurationOptions_snapshotIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeNodeConfigurationOptions_snapshotIdentifier = Lens.lens (\DescribeNodeConfigurationOptions' {snapshotIdentifier} -> snapshotIdentifier) (\s@DescribeNodeConfigurationOptions' {} a -> s {snapshotIdentifier = a} :: DescribeNodeConfigurationOptions)

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
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNodeConfigurationOptions_marker
          Lens..~ rs
          Lens.^? describeNodeConfigurationOptionsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNodeConfigurationOptions
  where
  type
    AWSResponse DescribeNodeConfigurationOptions =
      DescribeNodeConfigurationOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeNodeConfigurationOptionsResult"
      ( \s h x ->
          DescribeNodeConfigurationOptionsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "NodeConfigurationOptionList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "NodeConfigurationOption")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNodeConfigurationOptions
  where
  hashWithSalt
    _salt
    DescribeNodeConfigurationOptions' {..} =
      _salt `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` ownerAccount
        `Prelude.hashWithSalt` snapshotArn
        `Prelude.hashWithSalt` snapshotIdentifier
        `Prelude.hashWithSalt` actionType

instance
  Prelude.NFData
    DescribeNodeConfigurationOptions
  where
  rnf DescribeNodeConfigurationOptions' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf snapshotIdentifier
      `Prelude.seq` Prelude.rnf actionType

instance
  Data.ToHeaders
    DescribeNodeConfigurationOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeNodeConfigurationOptions where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNodeConfigurationOptions
  where
  toQuery DescribeNodeConfigurationOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNodeConfigurationOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Filter"
          Data.=: Data.toQuery
            ( Data.toQueryList "NodeConfigurationOptionsFilter"
                Prelude.<$> filters
            ),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "OwnerAccount" Data.=: ownerAccount,
        "SnapshotArn" Data.=: snapshotArn,
        "SnapshotIdentifier" Data.=: snapshotIdentifier,
        "ActionType" Data.=: actionType
      ]

-- | /See:/ 'newDescribeNodeConfigurationOptionsResponse' smart constructor.
data DescribeNodeConfigurationOptionsResponse = DescribeNodeConfigurationOptionsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of valid node configurations.
    nodeConfigurationOptionList :: Prelude.Maybe [NodeConfigurationOption],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNodeConfigurationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeNodeConfigurationOptionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'nodeConfigurationOptionList', 'describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList' - A list of valid node configurations.
--
-- 'httpStatus', 'describeNodeConfigurationOptionsResponse_httpStatus' - The response's http status code.
newDescribeNodeConfigurationOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNodeConfigurationOptionsResponse
newDescribeNodeConfigurationOptionsResponse
  pHttpStatus_ =
    DescribeNodeConfigurationOptionsResponse'
      { marker =
          Prelude.Nothing,
        nodeConfigurationOptionList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeNodeConfigurationOptionsResponse_marker :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Prelude.Maybe Prelude.Text)
describeNodeConfigurationOptionsResponse_marker = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {marker} -> marker) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {marker = a} :: DescribeNodeConfigurationOptionsResponse)

-- | A list of valid node configurations.
describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Prelude.Maybe [NodeConfigurationOption])
describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {nodeConfigurationOptionList} -> nodeConfigurationOptionList) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {nodeConfigurationOptionList = a} :: DescribeNodeConfigurationOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNodeConfigurationOptionsResponse_httpStatus :: Lens.Lens' DescribeNodeConfigurationOptionsResponse Prelude.Int
describeNodeConfigurationOptionsResponse_httpStatus = Lens.lens (\DescribeNodeConfigurationOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNodeConfigurationOptionsResponse' {} a -> s {httpStatus = a} :: DescribeNodeConfigurationOptionsResponse)

instance
  Prelude.NFData
    DescribeNodeConfigurationOptionsResponse
  where
  rnf DescribeNodeConfigurationOptionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf nodeConfigurationOptionList
      `Prelude.seq` Prelude.rnf httpStatus

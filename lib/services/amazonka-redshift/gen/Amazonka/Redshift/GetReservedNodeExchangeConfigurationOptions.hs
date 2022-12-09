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
-- Module      : Amazonka.Redshift.GetReservedNodeExchangeConfigurationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration options for the reserved-node exchange. These
-- options include information about the source reserved node and target
-- reserved node offering. Details include the node type, the price, the
-- node count, and the offering type.
--
-- This operation returns paginated results.
module Amazonka.Redshift.GetReservedNodeExchangeConfigurationOptions
  ( -- * Creating a Request
    GetReservedNodeExchangeConfigurationOptions (..),
    newGetReservedNodeExchangeConfigurationOptions,

    -- * Request Lenses
    getReservedNodeExchangeConfigurationOptions_clusterIdentifier,
    getReservedNodeExchangeConfigurationOptions_marker,
    getReservedNodeExchangeConfigurationOptions_maxRecords,
    getReservedNodeExchangeConfigurationOptions_snapshotIdentifier,
    getReservedNodeExchangeConfigurationOptions_actionType,

    -- * Destructuring the Response
    GetReservedNodeExchangeConfigurationOptionsResponse (..),
    newGetReservedNodeExchangeConfigurationOptionsResponse,

    -- * Response Lenses
    getReservedNodeExchangeConfigurationOptionsResponse_marker,
    getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList,
    getReservedNodeExchangeConfigurationOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReservedNodeExchangeConfigurationOptions' smart constructor.
data GetReservedNodeExchangeConfigurationOptions = GetReservedNodeExchangeConfigurationOptions'
  { -- | The identifier for the cluster that is the source for a reserved-node
    -- exchange.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous
    -- @GetReservedNodeExchangeConfigurationOptions@ request. If this parameter
    -- is specified, the response includes only records beyond the marker, up
    -- to the value specified by the @MaxRecords@ parameter. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @Marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the snapshot that is the source for the reserved-node
    -- exchange.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The action type of the reserved-node configuration. The action type can
    -- be an exchange initiated from either a snapshot or a resize.
    actionType :: ReservedNodeExchangeActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedNodeExchangeConfigurationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'getReservedNodeExchangeConfigurationOptions_clusterIdentifier' - The identifier for the cluster that is the source for a reserved-node
-- exchange.
--
-- 'marker', 'getReservedNodeExchangeConfigurationOptions_marker' - An optional pagination token provided by a previous
-- @GetReservedNodeExchangeConfigurationOptions@ request. If this parameter
-- is specified, the response includes only records beyond the marker, up
-- to the value specified by the @MaxRecords@ parameter. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'getReservedNodeExchangeConfigurationOptions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @Marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- 'snapshotIdentifier', 'getReservedNodeExchangeConfigurationOptions_snapshotIdentifier' - The identifier for the snapshot that is the source for the reserved-node
-- exchange.
--
-- 'actionType', 'getReservedNodeExchangeConfigurationOptions_actionType' - The action type of the reserved-node configuration. The action type can
-- be an exchange initiated from either a snapshot or a resize.
newGetReservedNodeExchangeConfigurationOptions ::
  -- | 'actionType'
  ReservedNodeExchangeActionType ->
  GetReservedNodeExchangeConfigurationOptions
newGetReservedNodeExchangeConfigurationOptions
  pActionType_ =
    GetReservedNodeExchangeConfigurationOptions'
      { clusterIdentifier =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        snapshotIdentifier =
          Prelude.Nothing,
        actionType = pActionType_
      }

-- | The identifier for the cluster that is the source for a reserved-node
-- exchange.
getReservedNodeExchangeConfigurationOptions_clusterIdentifier :: Lens.Lens' GetReservedNodeExchangeConfigurationOptions (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeConfigurationOptions_clusterIdentifier = Lens.lens (\GetReservedNodeExchangeConfigurationOptions' {clusterIdentifier} -> clusterIdentifier) (\s@GetReservedNodeExchangeConfigurationOptions' {} a -> s {clusterIdentifier = a} :: GetReservedNodeExchangeConfigurationOptions)

-- | An optional pagination token provided by a previous
-- @GetReservedNodeExchangeConfigurationOptions@ request. If this parameter
-- is specified, the response includes only records beyond the marker, up
-- to the value specified by the @MaxRecords@ parameter. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
getReservedNodeExchangeConfigurationOptions_marker :: Lens.Lens' GetReservedNodeExchangeConfigurationOptions (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeConfigurationOptions_marker = Lens.lens (\GetReservedNodeExchangeConfigurationOptions' {marker} -> marker) (\s@GetReservedNodeExchangeConfigurationOptions' {} a -> s {marker = a} :: GetReservedNodeExchangeConfigurationOptions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @Marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
getReservedNodeExchangeConfigurationOptions_maxRecords :: Lens.Lens' GetReservedNodeExchangeConfigurationOptions (Prelude.Maybe Prelude.Int)
getReservedNodeExchangeConfigurationOptions_maxRecords = Lens.lens (\GetReservedNodeExchangeConfigurationOptions' {maxRecords} -> maxRecords) (\s@GetReservedNodeExchangeConfigurationOptions' {} a -> s {maxRecords = a} :: GetReservedNodeExchangeConfigurationOptions)

-- | The identifier for the snapshot that is the source for the reserved-node
-- exchange.
getReservedNodeExchangeConfigurationOptions_snapshotIdentifier :: Lens.Lens' GetReservedNodeExchangeConfigurationOptions (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeConfigurationOptions_snapshotIdentifier = Lens.lens (\GetReservedNodeExchangeConfigurationOptions' {snapshotIdentifier} -> snapshotIdentifier) (\s@GetReservedNodeExchangeConfigurationOptions' {} a -> s {snapshotIdentifier = a} :: GetReservedNodeExchangeConfigurationOptions)

-- | The action type of the reserved-node configuration. The action type can
-- be an exchange initiated from either a snapshot or a resize.
getReservedNodeExchangeConfigurationOptions_actionType :: Lens.Lens' GetReservedNodeExchangeConfigurationOptions ReservedNodeExchangeActionType
getReservedNodeExchangeConfigurationOptions_actionType = Lens.lens (\GetReservedNodeExchangeConfigurationOptions' {actionType} -> actionType) (\s@GetReservedNodeExchangeConfigurationOptions' {} a -> s {actionType = a} :: GetReservedNodeExchangeConfigurationOptions)

instance
  Core.AWSPager
    GetReservedNodeExchangeConfigurationOptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReservedNodeExchangeConfigurationOptionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getReservedNodeExchangeConfigurationOptions_marker
          Lens..~ rs
            Lens.^? getReservedNodeExchangeConfigurationOptionsResponse_marker
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetReservedNodeExchangeConfigurationOptions
  where
  type
    AWSResponse
      GetReservedNodeExchangeConfigurationOptions =
      GetReservedNodeExchangeConfigurationOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetReservedNodeExchangeConfigurationOptionsResult"
      ( \s h x ->
          GetReservedNodeExchangeConfigurationOptionsResponse'
            Prelude.<$> (x Data..@? "Marker")
              Prelude.<*> ( x Data..@? "ReservedNodeConfigurationOptionList"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may
                                ( Data.parseXMLList
                                    "ReservedNodeConfigurationOption"
                                )
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReservedNodeExchangeConfigurationOptions
  where
  hashWithSalt
    _salt
    GetReservedNodeExchangeConfigurationOptions' {..} =
      _salt `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` snapshotIdentifier
        `Prelude.hashWithSalt` actionType

instance
  Prelude.NFData
    GetReservedNodeExchangeConfigurationOptions
  where
  rnf GetReservedNodeExchangeConfigurationOptions' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf snapshotIdentifier
      `Prelude.seq` Prelude.rnf actionType

instance
  Data.ToHeaders
    GetReservedNodeExchangeConfigurationOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetReservedNodeExchangeConfigurationOptions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetReservedNodeExchangeConfigurationOptions
  where
  toQuery
    GetReservedNodeExchangeConfigurationOptions' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "GetReservedNodeExchangeConfigurationOptions" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2012-12-01" :: Prelude.ByteString),
          "ClusterIdentifier" Data.=: clusterIdentifier,
          "Marker" Data.=: marker,
          "MaxRecords" Data.=: maxRecords,
          "SnapshotIdentifier" Data.=: snapshotIdentifier,
          "ActionType" Data.=: actionType
        ]

-- | /See:/ 'newGetReservedNodeExchangeConfigurationOptionsResponse' smart constructor.
data GetReservedNodeExchangeConfigurationOptionsResponse = GetReservedNodeExchangeConfigurationOptionsResponse'
  { -- | A pagination token provided by a previous
    -- @GetReservedNodeExchangeConfigurationOptions@ request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | the configuration options for the reserved-node exchange. These options
    -- include information about the source reserved node and target reserved
    -- node. Details include the node type, the price, the node count, and the
    -- offering type.
    reservedNodeConfigurationOptionList :: Prelude.Maybe [ReservedNodeConfigurationOption],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedNodeExchangeConfigurationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'getReservedNodeExchangeConfigurationOptionsResponse_marker' - A pagination token provided by a previous
-- @GetReservedNodeExchangeConfigurationOptions@ request.
--
-- 'reservedNodeConfigurationOptionList', 'getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList' - the configuration options for the reserved-node exchange. These options
-- include information about the source reserved node and target reserved
-- node. Details include the node type, the price, the node count, and the
-- offering type.
--
-- 'httpStatus', 'getReservedNodeExchangeConfigurationOptionsResponse_httpStatus' - The response's http status code.
newGetReservedNodeExchangeConfigurationOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReservedNodeExchangeConfigurationOptionsResponse
newGetReservedNodeExchangeConfigurationOptionsResponse
  pHttpStatus_ =
    GetReservedNodeExchangeConfigurationOptionsResponse'
      { marker =
          Prelude.Nothing,
        reservedNodeConfigurationOptionList =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | A pagination token provided by a previous
-- @GetReservedNodeExchangeConfigurationOptions@ request.
getReservedNodeExchangeConfigurationOptionsResponse_marker :: Lens.Lens' GetReservedNodeExchangeConfigurationOptionsResponse (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeConfigurationOptionsResponse_marker = Lens.lens (\GetReservedNodeExchangeConfigurationOptionsResponse' {marker} -> marker) (\s@GetReservedNodeExchangeConfigurationOptionsResponse' {} a -> s {marker = a} :: GetReservedNodeExchangeConfigurationOptionsResponse)

-- | the configuration options for the reserved-node exchange. These options
-- include information about the source reserved node and target reserved
-- node. Details include the node type, the price, the node count, and the
-- offering type.
getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList :: Lens.Lens' GetReservedNodeExchangeConfigurationOptionsResponse (Prelude.Maybe [ReservedNodeConfigurationOption])
getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList = Lens.lens (\GetReservedNodeExchangeConfigurationOptionsResponse' {reservedNodeConfigurationOptionList} -> reservedNodeConfigurationOptionList) (\s@GetReservedNodeExchangeConfigurationOptionsResponse' {} a -> s {reservedNodeConfigurationOptionList = a} :: GetReservedNodeExchangeConfigurationOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReservedNodeExchangeConfigurationOptionsResponse_httpStatus :: Lens.Lens' GetReservedNodeExchangeConfigurationOptionsResponse Prelude.Int
getReservedNodeExchangeConfigurationOptionsResponse_httpStatus = Lens.lens (\GetReservedNodeExchangeConfigurationOptionsResponse' {httpStatus} -> httpStatus) (\s@GetReservedNodeExchangeConfigurationOptionsResponse' {} a -> s {httpStatus = a} :: GetReservedNodeExchangeConfigurationOptionsResponse)

instance
  Prelude.NFData
    GetReservedNodeExchangeConfigurationOptionsResponse
  where
  rnf
    GetReservedNodeExchangeConfigurationOptionsResponse' {..} =
      Prelude.rnf marker
        `Prelude.seq` Prelude.rnf reservedNodeConfigurationOptionList
        `Prelude.seq` Prelude.rnf httpStatus

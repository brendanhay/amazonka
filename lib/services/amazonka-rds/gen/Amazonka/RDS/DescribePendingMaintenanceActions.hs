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
-- Module      : Amazonka.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at
-- least one pending maintenance action.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribePendingMaintenanceActions
  ( -- * Creating a Request
    DescribePendingMaintenanceActions (..),
    newDescribePendingMaintenanceActions,

    -- * Request Lenses
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_resourceIdentifier,

    -- * Destructuring the Response
    DescribePendingMaintenanceActionsResponse (..),
    newDescribePendingMaintenanceActionsResponse,

    -- * Response Lenses
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { -- | A filter that specifies one or more resources to return pending
    -- maintenance actions for.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list only includes pending
    --     maintenance actions for the DB clusters identified by these ARNs.
    --
    -- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
    --     ARNs. The results list only includes pending maintenance actions for
    --     the DB instances identified by these ARNs.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribePendingMaintenanceActions@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to a
    -- number of records specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The ARN of a resource to return pending maintenance actions for.
    resourceIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePendingMaintenanceActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describePendingMaintenanceActions_filters' - A filter that specifies one or more resources to return pending
-- maintenance actions for.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list only includes pending
--     maintenance actions for the DB clusters identified by these ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     ARNs. The results list only includes pending maintenance actions for
--     the DB instances identified by these ARNs.
--
-- 'marker', 'describePendingMaintenanceActions_marker' - An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
--
-- 'maxRecords', 'describePendingMaintenanceActions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'resourceIdentifier', 'describePendingMaintenanceActions_resourceIdentifier' - The ARN of a resource to return pending maintenance actions for.
newDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
newDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | A filter that specifies one or more resources to return pending
-- maintenance actions for.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list only includes pending
--     maintenance actions for the DB clusters identified by these ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     ARNs. The results list only includes pending maintenance actions for
--     the DB instances identified by these ARNs.
describePendingMaintenanceActions_filters :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe [Filter])
describePendingMaintenanceActions_filters = Lens.lens (\DescribePendingMaintenanceActions' {filters} -> filters) (\s@DescribePendingMaintenanceActions' {} a -> s {filters = a} :: DescribePendingMaintenanceActions) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
describePendingMaintenanceActions_marker :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActions_marker = Lens.lens (\DescribePendingMaintenanceActions' {marker} -> marker) (\s@DescribePendingMaintenanceActions' {} a -> s {marker = a} :: DescribePendingMaintenanceActions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describePendingMaintenanceActions_maxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Int)
describePendingMaintenanceActions_maxRecords = Lens.lens (\DescribePendingMaintenanceActions' {maxRecords} -> maxRecords) (\s@DescribePendingMaintenanceActions' {} a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)

-- | The ARN of a resource to return pending maintenance actions for.
describePendingMaintenanceActions_resourceIdentifier :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActions_resourceIdentifier = Lens.lens (\DescribePendingMaintenanceActions' {resourceIdentifier} -> resourceIdentifier) (\s@DescribePendingMaintenanceActions' {} a -> s {resourceIdentifier = a} :: DescribePendingMaintenanceActions)

instance
  Core.AWSPager
    DescribePendingMaintenanceActions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePendingMaintenanceActionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePendingMaintenanceActionsResponse_pendingMaintenanceActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePendingMaintenanceActions_marker
          Lens..~ rs
          Lens.^? describePendingMaintenanceActionsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribePendingMaintenanceActions
  where
  type
    AWSResponse DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribePendingMaintenanceActionsResult"
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Prelude.<$> (x Data..@? "Marker")
              Prelude.<*> ( x Data..@? "PendingMaintenanceActions"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may
                                ( Data.parseXMLList
                                    "ResourcePendingMaintenanceActions"
                                )
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePendingMaintenanceActions
  where
  hashWithSalt
    _salt
    DescribePendingMaintenanceActions' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` resourceIdentifier

instance
  Prelude.NFData
    DescribePendingMaintenanceActions
  where
  rnf DescribePendingMaintenanceActions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance
  Data.ToHeaders
    DescribePendingMaintenanceActions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribePendingMaintenanceActions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribePendingMaintenanceActions
  where
  toQuery DescribePendingMaintenanceActions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribePendingMaintenanceActions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ResourceIdentifier" Data.=: resourceIdentifier
      ]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'newDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | An optional pagination token provided by a previous
    -- @DescribePendingMaintenanceActions@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to a
    -- number of records specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of the pending maintenance actions for the resource.
    pendingMaintenanceActions :: Prelude.Maybe [ResourcePendingMaintenanceActions],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePendingMaintenanceActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describePendingMaintenanceActionsResponse_marker' - An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
--
-- 'pendingMaintenanceActions', 'describePendingMaintenanceActionsResponse_pendingMaintenanceActions' - A list of the pending maintenance actions for the resource.
--
-- 'httpStatus', 'describePendingMaintenanceActionsResponse_httpStatus' - The response's http status code.
newDescribePendingMaintenanceActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePendingMaintenanceActionsResponse
newDescribePendingMaintenanceActionsResponse
  pHttpStatus_ =
    DescribePendingMaintenanceActionsResponse'
      { marker =
          Prelude.Nothing,
        pendingMaintenanceActions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
describePendingMaintenanceActionsResponse_marker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActionsResponse_marker = Lens.lens (\DescribePendingMaintenanceActionsResponse' {marker} -> marker) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {marker = a} :: DescribePendingMaintenanceActionsResponse)

-- | A list of the pending maintenance actions for the resource.
describePendingMaintenanceActionsResponse_pendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Prelude.Maybe [ResourcePendingMaintenanceActions])
describePendingMaintenanceActionsResponse_pendingMaintenanceActions = Lens.lens (\DescribePendingMaintenanceActionsResponse' {pendingMaintenanceActions} -> pendingMaintenanceActions) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {pendingMaintenanceActions = a} :: DescribePendingMaintenanceActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePendingMaintenanceActionsResponse_httpStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Prelude.Int
describePendingMaintenanceActionsResponse_httpStatus = Lens.lens (\DescribePendingMaintenanceActionsResponse' {httpStatus} -> httpStatus) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {httpStatus = a} :: DescribePendingMaintenanceActionsResponse)

instance
  Prelude.NFData
    DescribePendingMaintenanceActionsResponse
  where
  rnf DescribePendingMaintenanceActionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pendingMaintenanceActions
      `Prelude.seq` Prelude.rnf httpStatus

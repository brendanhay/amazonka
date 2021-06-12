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
-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at
-- least one pending maintenance action.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribePendingMaintenanceActions
  ( -- * Creating a Request
    DescribePendingMaintenanceActions (..),
    newDescribePendingMaintenanceActions,

    -- * Request Lenses
    describePendingMaintenanceActions_resourceIdentifier,
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,

    -- * Destructuring the Response
    DescribePendingMaintenanceActionsResponse (..),
    newDescribePendingMaintenanceActionsResponse,

    -- * Response Lenses
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { -- | The ARN of a resource to return pending maintenance actions for.
    resourceIdentifier :: Core.Maybe Core.Text,
    -- | A filter that specifies one or more resources to return pending
    -- maintenance actions for.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list will only include
    --     pending maintenance actions for the DB clusters identified by these
    --     ARNs.
    --
    -- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
    --     ARNs. The results list will only include pending maintenance actions
    --     for the DB instances identified by these ARNs.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribePendingMaintenanceActions@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to a
    -- number of records specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePendingMaintenanceActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'describePendingMaintenanceActions_resourceIdentifier' - The ARN of a resource to return pending maintenance actions for.
--
-- 'filters', 'describePendingMaintenanceActions_filters' - A filter that specifies one or more resources to return pending
-- maintenance actions for.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     pending maintenance actions for the DB clusters identified by these
--     ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     ARNs. The results list will only include pending maintenance actions
--     for the DB instances identified by these ARNs.
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
newDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
newDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { resourceIdentifier =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The ARN of a resource to return pending maintenance actions for.
describePendingMaintenanceActions_resourceIdentifier :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
describePendingMaintenanceActions_resourceIdentifier = Lens.lens (\DescribePendingMaintenanceActions' {resourceIdentifier} -> resourceIdentifier) (\s@DescribePendingMaintenanceActions' {} a -> s {resourceIdentifier = a} :: DescribePendingMaintenanceActions)

-- | A filter that specifies one or more resources to return pending
-- maintenance actions for.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     pending maintenance actions for the DB clusters identified by these
--     ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     ARNs. The results list will only include pending maintenance actions
--     for the DB instances identified by these ARNs.
describePendingMaintenanceActions_filters :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe [Filter])
describePendingMaintenanceActions_filters = Lens.lens (\DescribePendingMaintenanceActions' {filters} -> filters) (\s@DescribePendingMaintenanceActions' {} a -> s {filters = a} :: DescribePendingMaintenanceActions) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
describePendingMaintenanceActions_marker :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
describePendingMaintenanceActions_marker = Lens.lens (\DescribePendingMaintenanceActions' {marker} -> marker) (\s@DescribePendingMaintenanceActions' {} a -> s {marker = a} :: DescribePendingMaintenanceActions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describePendingMaintenanceActions_maxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Int)
describePendingMaintenanceActions_maxRecords = Lens.lens (\DescribePendingMaintenanceActions' {maxRecords} -> maxRecords) (\s@DescribePendingMaintenanceActions' {} a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)

instance
  Core.AWSPager
    DescribePendingMaintenanceActions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePendingMaintenanceActionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePendingMaintenanceActionsResponse_pendingMaintenanceActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePendingMaintenanceActions_marker
          Lens..~ rs
          Lens.^? describePendingMaintenanceActionsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribePendingMaintenanceActions
  where
  type
    AWSResponse DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribePendingMaintenanceActionsResult"
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Core.<$> ( x Core..@? "PendingMaintenanceActions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           ( Core.parseXMLList
                               "ResourcePendingMaintenanceActions"
                           )
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribePendingMaintenanceActions

instance
  Core.NFData
    DescribePendingMaintenanceActions

instance
  Core.ToHeaders
    DescribePendingMaintenanceActions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribePendingMaintenanceActions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribePendingMaintenanceActions
  where
  toQuery DescribePendingMaintenanceActions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribePendingMaintenanceActions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ResourceIdentifier" Core.=: resourceIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'newDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | A list of the pending maintenance actions for the resource.
    pendingMaintenanceActions :: Core.Maybe [ResourcePendingMaintenanceActions],
    -- | An optional pagination token provided by a previous
    -- @DescribePendingMaintenanceActions@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to a
    -- number of records specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePendingMaintenanceActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingMaintenanceActions', 'describePendingMaintenanceActionsResponse_pendingMaintenanceActions' - A list of the pending maintenance actions for the resource.
--
-- 'marker', 'describePendingMaintenanceActionsResponse_marker' - An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
--
-- 'httpStatus', 'describePendingMaintenanceActionsResponse_httpStatus' - The response's http status code.
newDescribePendingMaintenanceActionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePendingMaintenanceActionsResponse
newDescribePendingMaintenanceActionsResponse
  pHttpStatus_ =
    DescribePendingMaintenanceActionsResponse'
      { pendingMaintenanceActions =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of the pending maintenance actions for the resource.
describePendingMaintenanceActionsResponse_pendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe [ResourcePendingMaintenanceActions])
describePendingMaintenanceActionsResponse_pendingMaintenanceActions = Lens.lens (\DescribePendingMaintenanceActionsResponse' {pendingMaintenanceActions} -> pendingMaintenanceActions) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {pendingMaintenanceActions = a} :: DescribePendingMaintenanceActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
describePendingMaintenanceActionsResponse_marker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe Core.Text)
describePendingMaintenanceActionsResponse_marker = Lens.lens (\DescribePendingMaintenanceActionsResponse' {marker} -> marker) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {marker = a} :: DescribePendingMaintenanceActionsResponse)

-- | The response's http status code.
describePendingMaintenanceActionsResponse_httpStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Core.Int
describePendingMaintenanceActionsResponse_httpStatus = Lens.lens (\DescribePendingMaintenanceActionsResponse' {httpStatus} -> httpStatus) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {httpStatus = a} :: DescribePendingMaintenanceActionsResponse)

instance
  Core.NFData
    DescribePendingMaintenanceActionsResponse

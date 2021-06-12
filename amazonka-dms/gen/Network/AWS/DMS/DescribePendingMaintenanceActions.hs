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
-- Module      : Network.AWS.DMS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For internal use only
module Network.AWS.DMS.DescribePendingMaintenanceActions
  ( -- * Creating a Request
    DescribePendingMaintenanceActions (..),
    newDescribePendingMaintenanceActions,

    -- * Request Lenses
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_replicationInstanceArn,
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
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { filters :: Core.Maybe [Filter],
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
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
-- 'filters', 'describePendingMaintenanceActions_filters' -
--
-- 'replicationInstanceArn', 'describePendingMaintenanceActions_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'marker', 'describePendingMaintenanceActions_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describePendingMaintenanceActions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
newDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters =
        Core.Nothing,
      replicationInstanceArn = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- |
describePendingMaintenanceActions_filters :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe [Filter])
describePendingMaintenanceActions_filters = Lens.lens (\DescribePendingMaintenanceActions' {filters} -> filters) (\s@DescribePendingMaintenanceActions' {} a -> s {filters = a} :: DescribePendingMaintenanceActions) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the replication instance.
describePendingMaintenanceActions_replicationInstanceArn :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
describePendingMaintenanceActions_replicationInstanceArn = Lens.lens (\DescribePendingMaintenanceActions' {replicationInstanceArn} -> replicationInstanceArn) (\s@DescribePendingMaintenanceActions' {} a -> s {replicationInstanceArn = a} :: DescribePendingMaintenanceActions)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describePendingMaintenanceActions_marker :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
describePendingMaintenanceActions_marker = Lens.lens (\DescribePendingMaintenanceActions' {marker} -> marker) (\s@DescribePendingMaintenanceActions' {} a -> s {marker = a} :: DescribePendingMaintenanceActions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describePendingMaintenanceActions_maxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Int)
describePendingMaintenanceActions_maxRecords = Lens.lens (\DescribePendingMaintenanceActions' {maxRecords} -> maxRecords) (\s@DescribePendingMaintenanceActions' {} a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)

instance
  Core.AWSRequest
    DescribePendingMaintenanceActions
  where
  type
    AWSResponse DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Core.<$> ( x Core..?> "PendingMaintenanceActions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Marker")
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
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribePendingMaintenanceActions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribePendingMaintenanceActions
  where
  toJSON DescribePendingMaintenanceActions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("ReplicationInstanceArn" Core..=)
              Core.<$> replicationInstanceArn,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance
  Core.ToPath
    DescribePendingMaintenanceActions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribePendingMaintenanceActions
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | The pending maintenance action.
    pendingMaintenanceActions :: Core.Maybe [ResourcePendingMaintenanceActions],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
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
-- 'pendingMaintenanceActions', 'describePendingMaintenanceActionsResponse_pendingMaintenanceActions' - The pending maintenance action.
--
-- 'marker', 'describePendingMaintenanceActionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
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

-- | The pending maintenance action.
describePendingMaintenanceActionsResponse_pendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe [ResourcePendingMaintenanceActions])
describePendingMaintenanceActionsResponse_pendingMaintenanceActions = Lens.lens (\DescribePendingMaintenanceActionsResponse' {pendingMaintenanceActions} -> pendingMaintenanceActions) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {pendingMaintenanceActions = a} :: DescribePendingMaintenanceActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describePendingMaintenanceActionsResponse_marker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe Core.Text)
describePendingMaintenanceActionsResponse_marker = Lens.lens (\DescribePendingMaintenanceActionsResponse' {marker} -> marker) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {marker = a} :: DescribePendingMaintenanceActionsResponse)

-- | The response's http status code.
describePendingMaintenanceActionsResponse_httpStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Core.Int
describePendingMaintenanceActionsResponse_httpStatus = Lens.lens (\DescribePendingMaintenanceActionsResponse' {httpStatus} -> httpStatus) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {httpStatus = a} :: DescribePendingMaintenanceActionsResponse)

instance
  Core.NFData
    DescribePendingMaintenanceActionsResponse

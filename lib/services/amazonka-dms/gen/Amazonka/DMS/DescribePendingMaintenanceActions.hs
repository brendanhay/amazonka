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
-- Module      : Amazonka.DMS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For internal use only
module Amazonka.DMS.DescribePendingMaintenanceActions
  ( -- * Creating a Request
    DescribePendingMaintenanceActions (..),
    newDescribePendingMaintenanceActions,

    -- * Request Lenses
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_replicationInstanceArn,

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
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'describePendingMaintenanceActions_filters' -
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
--
-- 'replicationInstanceArn', 'describePendingMaintenanceActions_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
newDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing
    }

describePendingMaintenanceActions_filters :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe [Filter])
describePendingMaintenanceActions_filters = Lens.lens (\DescribePendingMaintenanceActions' {filters} -> filters) (\s@DescribePendingMaintenanceActions' {} a -> s {filters = a} :: DescribePendingMaintenanceActions) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describePendingMaintenanceActions_marker :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActions_marker = Lens.lens (\DescribePendingMaintenanceActions' {marker} -> marker) (\s@DescribePendingMaintenanceActions' {} a -> s {marker = a} :: DescribePendingMaintenanceActions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describePendingMaintenanceActions_maxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Int)
describePendingMaintenanceActions_maxRecords = Lens.lens (\DescribePendingMaintenanceActions' {maxRecords} -> maxRecords) (\s@DescribePendingMaintenanceActions' {} a -> s {maxRecords = a} :: DescribePendingMaintenanceActions)

-- | The Amazon Resource Name (ARN) of the replication instance.
describePendingMaintenanceActions_replicationInstanceArn :: Lens.Lens' DescribePendingMaintenanceActions (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActions_replicationInstanceArn = Lens.lens (\DescribePendingMaintenanceActions' {replicationInstanceArn} -> replicationInstanceArn) (\s@DescribePendingMaintenanceActions' {} a -> s {replicationInstanceArn = a} :: DescribePendingMaintenanceActions)

instance
  Core.AWSRequest
    DescribePendingMaintenanceActions
  where
  type
    AWSResponse DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "PendingMaintenanceActions"
                            Core..!@ Prelude.mempty
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
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` replicationInstanceArn

instance
  Prelude.NFData
    DescribePendingMaintenanceActions
  where
  rnf DescribePendingMaintenanceActions' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords `Prelude.seq`
          Prelude.rnf replicationInstanceArn

instance
  Data.ToHeaders
    DescribePendingMaintenanceActions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribePendingMaintenanceActions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribePendingMaintenanceActions
  where
  toJSON DescribePendingMaintenanceActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            ("ReplicationInstanceArn" Data..=)
              Prelude.<$> replicationInstanceArn
          ]
      )

instance
  Data.ToPath
    DescribePendingMaintenanceActions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribePendingMaintenanceActions
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The pending maintenance action.
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
-- 'marker', 'describePendingMaintenanceActionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'pendingMaintenanceActions', 'describePendingMaintenanceActionsResponse_pendingMaintenanceActions' - The pending maintenance action.
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

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describePendingMaintenanceActionsResponse_marker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Prelude.Maybe Prelude.Text)
describePendingMaintenanceActionsResponse_marker = Lens.lens (\DescribePendingMaintenanceActionsResponse' {marker} -> marker) (\s@DescribePendingMaintenanceActionsResponse' {} a -> s {marker = a} :: DescribePendingMaintenanceActionsResponse)

-- | The pending maintenance action.
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
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf pendingMaintenanceActions `Prelude.seq`
        Prelude.rnf httpStatus

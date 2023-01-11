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
-- Module      : Amazonka.MemoryDb.BatchUpdateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply the service update to a list of clusters supplied. For more
-- information on service updates and applying them, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/managing-updates.html#applying-updates Applying the service updates>.
module Amazonka.MemoryDb.BatchUpdateCluster
  ( -- * Creating a Request
    BatchUpdateCluster (..),
    newBatchUpdateCluster,

    -- * Request Lenses
    batchUpdateCluster_serviceUpdate,
    batchUpdateCluster_clusterNames,

    -- * Destructuring the Response
    BatchUpdateClusterResponse (..),
    newBatchUpdateClusterResponse,

    -- * Response Lenses
    batchUpdateClusterResponse_processedClusters,
    batchUpdateClusterResponse_unprocessedClusters,
    batchUpdateClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateCluster' smart constructor.
data BatchUpdateCluster = BatchUpdateCluster'
  { -- | The unique ID of the service update
    serviceUpdate :: Prelude.Maybe ServiceUpdateRequest,
    -- | The cluster names to apply the updates.
    clusterNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdate', 'batchUpdateCluster_serviceUpdate' - The unique ID of the service update
--
-- 'clusterNames', 'batchUpdateCluster_clusterNames' - The cluster names to apply the updates.
newBatchUpdateCluster ::
  BatchUpdateCluster
newBatchUpdateCluster =
  BatchUpdateCluster'
    { serviceUpdate =
        Prelude.Nothing,
      clusterNames = Prelude.mempty
    }

-- | The unique ID of the service update
batchUpdateCluster_serviceUpdate :: Lens.Lens' BatchUpdateCluster (Prelude.Maybe ServiceUpdateRequest)
batchUpdateCluster_serviceUpdate = Lens.lens (\BatchUpdateCluster' {serviceUpdate} -> serviceUpdate) (\s@BatchUpdateCluster' {} a -> s {serviceUpdate = a} :: BatchUpdateCluster)

-- | The cluster names to apply the updates.
batchUpdateCluster_clusterNames :: Lens.Lens' BatchUpdateCluster [Prelude.Text]
batchUpdateCluster_clusterNames = Lens.lens (\BatchUpdateCluster' {clusterNames} -> clusterNames) (\s@BatchUpdateCluster' {} a -> s {clusterNames = a} :: BatchUpdateCluster) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateCluster where
  type
    AWSResponse BatchUpdateCluster =
      BatchUpdateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateClusterResponse'
            Prelude.<$> ( x Data..?> "ProcessedClusters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "UnprocessedClusters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateCluster where
  hashWithSalt _salt BatchUpdateCluster' {..} =
    _salt `Prelude.hashWithSalt` serviceUpdate
      `Prelude.hashWithSalt` clusterNames

instance Prelude.NFData BatchUpdateCluster where
  rnf BatchUpdateCluster' {..} =
    Prelude.rnf serviceUpdate
      `Prelude.seq` Prelude.rnf clusterNames

instance Data.ToHeaders BatchUpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.BatchUpdateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateCluster where
  toJSON BatchUpdateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServiceUpdate" Data..=) Prelude.<$> serviceUpdate,
            Prelude.Just ("ClusterNames" Data..= clusterNames)
          ]
      )

instance Data.ToPath BatchUpdateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchUpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateClusterResponse' smart constructor.
data BatchUpdateClusterResponse = BatchUpdateClusterResponse'
  { -- | The list of clusters that have been updated.
    processedClusters :: Prelude.Maybe [Cluster],
    -- | The list of clusters where updates have not been applied.
    unprocessedClusters :: Prelude.Maybe [UnprocessedCluster],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processedClusters', 'batchUpdateClusterResponse_processedClusters' - The list of clusters that have been updated.
--
-- 'unprocessedClusters', 'batchUpdateClusterResponse_unprocessedClusters' - The list of clusters where updates have not been applied.
--
-- 'httpStatus', 'batchUpdateClusterResponse_httpStatus' - The response's http status code.
newBatchUpdateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateClusterResponse
newBatchUpdateClusterResponse pHttpStatus_ =
  BatchUpdateClusterResponse'
    { processedClusters =
        Prelude.Nothing,
      unprocessedClusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of clusters that have been updated.
batchUpdateClusterResponse_processedClusters :: Lens.Lens' BatchUpdateClusterResponse (Prelude.Maybe [Cluster])
batchUpdateClusterResponse_processedClusters = Lens.lens (\BatchUpdateClusterResponse' {processedClusters} -> processedClusters) (\s@BatchUpdateClusterResponse' {} a -> s {processedClusters = a} :: BatchUpdateClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of clusters where updates have not been applied.
batchUpdateClusterResponse_unprocessedClusters :: Lens.Lens' BatchUpdateClusterResponse (Prelude.Maybe [UnprocessedCluster])
batchUpdateClusterResponse_unprocessedClusters = Lens.lens (\BatchUpdateClusterResponse' {unprocessedClusters} -> unprocessedClusters) (\s@BatchUpdateClusterResponse' {} a -> s {unprocessedClusters = a} :: BatchUpdateClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateClusterResponse_httpStatus :: Lens.Lens' BatchUpdateClusterResponse Prelude.Int
batchUpdateClusterResponse_httpStatus = Lens.lens (\BatchUpdateClusterResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateClusterResponse' {} a -> s {httpStatus = a} :: BatchUpdateClusterResponse)

instance Prelude.NFData BatchUpdateClusterResponse where
  rnf BatchUpdateClusterResponse' {..} =
    Prelude.rnf processedClusters
      `Prelude.seq` Prelude.rnf unprocessedClusters
      `Prelude.seq` Prelude.rnf httpStatus

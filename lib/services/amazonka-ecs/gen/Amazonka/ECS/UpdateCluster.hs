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
-- Module      : Amazonka.ECS.UpdateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cluster.
module Amazonka.ECS.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_configuration,
    updateCluster_settings,
    updateCluster_cluster,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The execute command configuration for the cluster.
    configuration :: Prelude.Maybe ClusterConfiguration,
    -- | The cluster settings for your cluster.
    settings :: Prelude.Maybe [ClusterSetting],
    -- | The name of the cluster to modify the settings for.
    cluster :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateCluster_configuration' - The execute command configuration for the cluster.
--
-- 'settings', 'updateCluster_settings' - The cluster settings for your cluster.
--
-- 'cluster', 'updateCluster_cluster' - The name of the cluster to modify the settings for.
newUpdateCluster ::
  -- | 'cluster'
  Prelude.Text ->
  UpdateCluster
newUpdateCluster pCluster_ =
  UpdateCluster'
    { configuration = Prelude.Nothing,
      settings = Prelude.Nothing,
      cluster = pCluster_
    }

-- | The execute command configuration for the cluster.
updateCluster_configuration :: Lens.Lens' UpdateCluster (Prelude.Maybe ClusterConfiguration)
updateCluster_configuration = Lens.lens (\UpdateCluster' {configuration} -> configuration) (\s@UpdateCluster' {} a -> s {configuration = a} :: UpdateCluster)

-- | The cluster settings for your cluster.
updateCluster_settings :: Lens.Lens' UpdateCluster (Prelude.Maybe [ClusterSetting])
updateCluster_settings = Lens.lens (\UpdateCluster' {settings} -> settings) (\s@UpdateCluster' {} a -> s {settings = a} :: UpdateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster to modify the settings for.
updateCluster_cluster :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_cluster = Lens.lens (\UpdateCluster' {cluster} -> cluster) (\s@UpdateCluster' {} a -> s {cluster = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (x Data..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCluster where
  hashWithSalt _salt UpdateCluster' {..} =
    _salt `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` cluster

instance Prelude.NFData UpdateCluster where
  rnf UpdateCluster' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf cluster

instance Data.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuration" Data..=) Prelude.<$> configuration,
            ("settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("cluster" Data..= cluster)
          ]
      )

instance Data.ToPath UpdateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | Details about the cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateClusterResponse_cluster' - Details about the cluster.
--
-- 'httpStatus', 'updateClusterResponse_httpStatus' - The response's http status code.
newUpdateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ =
  UpdateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the cluster.
updateClusterResponse_cluster :: Lens.Lens' UpdateClusterResponse (Prelude.Maybe Cluster)
updateClusterResponse_cluster = Lens.lens (\UpdateClusterResponse' {cluster} -> cluster) (\s@UpdateClusterResponse' {} a -> s {cluster = a} :: UpdateClusterResponse)

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Prelude.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

instance Prelude.NFData UpdateClusterResponse where
  rnf UpdateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus

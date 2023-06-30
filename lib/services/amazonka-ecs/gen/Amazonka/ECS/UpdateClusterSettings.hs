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
-- Module      : Amazonka.ECS.UpdateClusterSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings to use for a cluster.
module Amazonka.ECS.UpdateClusterSettings
  ( -- * Creating a Request
    UpdateClusterSettings (..),
    newUpdateClusterSettings,

    -- * Request Lenses
    updateClusterSettings_cluster,
    updateClusterSettings_settings,

    -- * Destructuring the Response
    UpdateClusterSettingsResponse (..),
    newUpdateClusterSettingsResponse,

    -- * Response Lenses
    updateClusterSettingsResponse_cluster,
    updateClusterSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClusterSettings' smart constructor.
data UpdateClusterSettings = UpdateClusterSettings'
  { -- | The name of the cluster to modify the settings for.
    cluster :: Prelude.Text,
    -- | The setting to use by default for a cluster. This parameter is used to
    -- turn on CloudWatch Container Insights for a cluster. If this value is
    -- specified, it overrides the @containerInsights@ value set with
    -- PutAccountSetting or PutAccountSettingDefault.
    --
    -- Currently, if you delete an existing cluster that does not have
    -- Container Insights turned on, and then create a new cluster with the
    -- same name with Container Insights tuned on, Container Insights will not
    -- actually be turned on. If you want to preserve the same name for your
    -- existing cluster and turn on Container Insights, you must wait 7 days
    -- before you can re-create it.
    settings :: [ClusterSetting]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateClusterSettings_cluster' - The name of the cluster to modify the settings for.
--
-- 'settings', 'updateClusterSettings_settings' - The setting to use by default for a cluster. This parameter is used to
-- turn on CloudWatch Container Insights for a cluster. If this value is
-- specified, it overrides the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
--
-- Currently, if you delete an existing cluster that does not have
-- Container Insights turned on, and then create a new cluster with the
-- same name with Container Insights tuned on, Container Insights will not
-- actually be turned on. If you want to preserve the same name for your
-- existing cluster and turn on Container Insights, you must wait 7 days
-- before you can re-create it.
newUpdateClusterSettings ::
  -- | 'cluster'
  Prelude.Text ->
  UpdateClusterSettings
newUpdateClusterSettings pCluster_ =
  UpdateClusterSettings'
    { cluster = pCluster_,
      settings = Prelude.mempty
    }

-- | The name of the cluster to modify the settings for.
updateClusterSettings_cluster :: Lens.Lens' UpdateClusterSettings Prelude.Text
updateClusterSettings_cluster = Lens.lens (\UpdateClusterSettings' {cluster} -> cluster) (\s@UpdateClusterSettings' {} a -> s {cluster = a} :: UpdateClusterSettings)

-- | The setting to use by default for a cluster. This parameter is used to
-- turn on CloudWatch Container Insights for a cluster. If this value is
-- specified, it overrides the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
--
-- Currently, if you delete an existing cluster that does not have
-- Container Insights turned on, and then create a new cluster with the
-- same name with Container Insights tuned on, Container Insights will not
-- actually be turned on. If you want to preserve the same name for your
-- existing cluster and turn on Container Insights, you must wait 7 days
-- before you can re-create it.
updateClusterSettings_settings :: Lens.Lens' UpdateClusterSettings [ClusterSetting]
updateClusterSettings_settings = Lens.lens (\UpdateClusterSettings' {settings} -> settings) (\s@UpdateClusterSettings' {} a -> s {settings = a} :: UpdateClusterSettings) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateClusterSettings where
  type
    AWSResponse UpdateClusterSettings =
      UpdateClusterSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterSettingsResponse'
            Prelude.<$> (x Data..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClusterSettings where
  hashWithSalt _salt UpdateClusterSettings' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` settings

instance Prelude.NFData UpdateClusterSettings where
  rnf UpdateClusterSettings' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf settings

instance Data.ToHeaders UpdateClusterSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateClusterSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateClusterSettings where
  toJSON UpdateClusterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just ("settings" Data..= settings)
          ]
      )

instance Data.ToPath UpdateClusterSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateClusterSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterSettingsResponse' smart constructor.
data UpdateClusterSettingsResponse = UpdateClusterSettingsResponse'
  { -- | Details about the cluster
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateClusterSettingsResponse_cluster' - Details about the cluster
--
-- 'httpStatus', 'updateClusterSettingsResponse_httpStatus' - The response's http status code.
newUpdateClusterSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterSettingsResponse
newUpdateClusterSettingsResponse pHttpStatus_ =
  UpdateClusterSettingsResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the cluster
updateClusterSettingsResponse_cluster :: Lens.Lens' UpdateClusterSettingsResponse (Prelude.Maybe Cluster)
updateClusterSettingsResponse_cluster = Lens.lens (\UpdateClusterSettingsResponse' {cluster} -> cluster) (\s@UpdateClusterSettingsResponse' {} a -> s {cluster = a} :: UpdateClusterSettingsResponse)

-- | The response's http status code.
updateClusterSettingsResponse_httpStatus :: Lens.Lens' UpdateClusterSettingsResponse Prelude.Int
updateClusterSettingsResponse_httpStatus = Lens.lens (\UpdateClusterSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterSettingsResponse' {} a -> s {httpStatus = a} :: UpdateClusterSettingsResponse)

instance Prelude.NFData UpdateClusterSettingsResponse where
  rnf UpdateClusterSettingsResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus

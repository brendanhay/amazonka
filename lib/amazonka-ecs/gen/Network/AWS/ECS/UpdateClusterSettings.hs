{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateClusterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings to use for a cluster.
module Network.AWS.ECS.UpdateClusterSettings
  ( -- * Creating a request
    UpdateClusterSettings (..),
    mkUpdateClusterSettings,

    -- ** Request lenses
    ucsCluster,
    ucsSettings,

    -- * Destructuring the response
    UpdateClusterSettingsResponse (..),
    mkUpdateClusterSettingsResponse,

    -- ** Response lenses
    ucsrsCluster,
    ucsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateClusterSettings' smart constructor.
data UpdateClusterSettings = UpdateClusterSettings'
  { -- | The name of the cluster to modify the settings for.
    cluster :: Lude.Text,
    -- | The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
    settings :: [ClusterSetting]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClusterSettings' with the minimum fields required to make a request.
--
-- * 'cluster' - The name of the cluster to modify the settings for.
-- * 'settings' - The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
mkUpdateClusterSettings ::
  -- | 'cluster'
  Lude.Text ->
  UpdateClusterSettings
mkUpdateClusterSettings pCluster_ =
  UpdateClusterSettings'
    { cluster = pCluster_,
      settings = Lude.mempty
    }

-- | The name of the cluster to modify the settings for.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsCluster :: Lens.Lens' UpdateClusterSettings Lude.Text
ucsCluster = Lens.lens (cluster :: UpdateClusterSettings -> Lude.Text) (\s a -> s {cluster = a} :: UpdateClusterSettings)
{-# DEPRECATED ucsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsSettings :: Lens.Lens' UpdateClusterSettings [ClusterSetting]
ucsSettings = Lens.lens (settings :: UpdateClusterSettings -> [ClusterSetting]) (\s a -> s {settings = a} :: UpdateClusterSettings)
{-# DEPRECATED ucsSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

instance Lude.AWSRequest UpdateClusterSettings where
  type Rs UpdateClusterSettings = UpdateClusterSettingsResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateClusterSettingsResponse'
            Lude.<$> (x Lude..?> "cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateClusterSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateClusterSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateClusterSettings where
  toJSON UpdateClusterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("settings" Lude..= settings)
          ]
      )

instance Lude.ToPath UpdateClusterSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateClusterSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateClusterSettingsResponse' smart constructor.
data UpdateClusterSettingsResponse = UpdateClusterSettingsResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClusterSettingsResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkUpdateClusterSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateClusterSettingsResponse
mkUpdateClusterSettingsResponse pResponseStatus_ =
  UpdateClusterSettingsResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrsCluster :: Lens.Lens' UpdateClusterSettingsResponse (Lude.Maybe Cluster)
ucsrsCluster = Lens.lens (cluster :: UpdateClusterSettingsResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: UpdateClusterSettingsResponse)
{-# DEPRECATED ucsrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrsResponseStatus :: Lens.Lens' UpdateClusterSettingsResponse Lude.Int
ucsrsResponseStatus = Lens.lens (responseStatus :: UpdateClusterSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateClusterSettingsResponse)
{-# DEPRECATED ucsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

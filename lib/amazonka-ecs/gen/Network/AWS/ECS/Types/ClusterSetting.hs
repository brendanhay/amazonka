{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterSetting
  ( ClusterSetting (..),

    -- * Smart constructor
    mkClusterSetting,

    -- * Lenses
    csValue,
    csName,
  )
where

import Network.AWS.ECS.Types.ClusterSettingName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster.
--
-- /See:/ 'mkClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { -- | The value to set for the cluster setting. The supported values are @enabled@ and @disabled@ . If @enabled@ is specified, CloudWatch Container Insights will be enabled for the cluster, otherwise it will be disabled unless the @containerInsights@ account setting is enabled. If a cluster value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
    value :: Lude.Maybe Lude.Text,
    -- | The name of the cluster setting. The only supported value is @containerInsights@ .
    name :: Lude.Maybe ClusterSettingName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSetting' with the minimum fields required to make a request.
--
-- * 'value' - The value to set for the cluster setting. The supported values are @enabled@ and @disabled@ . If @enabled@ is specified, CloudWatch Container Insights will be enabled for the cluster, otherwise it will be disabled unless the @containerInsights@ account setting is enabled. If a cluster value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
-- * 'name' - The name of the cluster setting. The only supported value is @containerInsights@ .
mkClusterSetting ::
  ClusterSetting
mkClusterSetting =
  ClusterSetting' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value to set for the cluster setting. The supported values are @enabled@ and @disabled@ . If @enabled@ is specified, CloudWatch Container Insights will be enabled for the cluster, otherwise it will be disabled unless the @containerInsights@ account setting is enabled. If a cluster value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csValue :: Lens.Lens' ClusterSetting (Lude.Maybe Lude.Text)
csValue = Lens.lens (value :: ClusterSetting -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ClusterSetting)
{-# DEPRECATED csValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the cluster setting. The only supported value is @containerInsights@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ClusterSetting (Lude.Maybe ClusterSettingName)
csName = Lens.lens (name :: ClusterSetting -> Lude.Maybe ClusterSettingName) (\s a -> s {name = a} :: ClusterSetting)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ClusterSetting where
  parseJSON =
    Lude.withObject
      "ClusterSetting"
      ( \x ->
          ClusterSetting'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "name")
      )

instance Lude.ToJSON ClusterSetting where
  toJSON ClusterSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, ("name" Lude..=) Lude.<$> name]
      )

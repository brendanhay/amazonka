{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterSetting where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.ClusterSettingName
import qualified Network.AWS.Lens as Lens

-- | The settings to use when creating a cluster. This parameter is used to
-- enable CloudWatch Container Insights for a cluster.
--
-- /See:/ 'newClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { -- | The name of the cluster setting. The only supported value is
    -- @containerInsights@.
    name :: Core.Maybe ClusterSettingName,
    -- | The value to set for the cluster setting. The supported values are
    -- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
    -- Container Insights will be enabled for the cluster, otherwise it will be
    -- disabled unless the @containerInsights@ account setting is enabled. If a
    -- cluster value is specified, it will override the @containerInsights@
    -- value set with PutAccountSetting or PutAccountSettingDefault.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'clusterSetting_name' - The name of the cluster setting. The only supported value is
-- @containerInsights@.
--
-- 'value', 'clusterSetting_value' - The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
-- Container Insights will be enabled for the cluster, otherwise it will be
-- disabled unless the @containerInsights@ account setting is enabled. If a
-- cluster value is specified, it will override the @containerInsights@
-- value set with PutAccountSetting or PutAccountSettingDefault.
newClusterSetting ::
  ClusterSetting
newClusterSetting =
  ClusterSetting'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the cluster setting. The only supported value is
-- @containerInsights@.
clusterSetting_name :: Lens.Lens' ClusterSetting (Core.Maybe ClusterSettingName)
clusterSetting_name = Lens.lens (\ClusterSetting' {name} -> name) (\s@ClusterSetting' {} a -> s {name = a} :: ClusterSetting)

-- | The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
-- Container Insights will be enabled for the cluster, otherwise it will be
-- disabled unless the @containerInsights@ account setting is enabled. If a
-- cluster value is specified, it will override the @containerInsights@
-- value set with PutAccountSetting or PutAccountSettingDefault.
clusterSetting_value :: Lens.Lens' ClusterSetting (Core.Maybe Core.Text)
clusterSetting_value = Lens.lens (\ClusterSetting' {value} -> value) (\s@ClusterSetting' {} a -> s {value = a} :: ClusterSetting)

instance Core.FromJSON ClusterSetting where
  parseJSON =
    Core.withObject
      "ClusterSetting"
      ( \x ->
          ClusterSetting'
            Core.<$> (x Core..:? "name") Core.<*> (x Core..:? "value")
      )

instance Core.Hashable ClusterSetting

instance Core.NFData ClusterSetting

instance Core.ToJSON ClusterSetting where
  toJSON ClusterSetting' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("value" Core..=) Core.<$> value
          ]
      )

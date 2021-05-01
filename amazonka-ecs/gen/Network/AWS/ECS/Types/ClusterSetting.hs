{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types.ClusterSettingName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings to use when creating a cluster. This parameter is used to
-- enable CloudWatch Container Insights for a cluster.
--
-- /See:/ 'newClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { -- | The name of the cluster setting. The only supported value is
    -- @containerInsights@.
    name :: Prelude.Maybe ClusterSettingName,
    -- | The value to set for the cluster setting. The supported values are
    -- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
    -- Container Insights will be enabled for the cluster, otherwise it will be
    -- disabled unless the @containerInsights@ account setting is enabled. If a
    -- cluster value is specified, it will override the @containerInsights@
    -- value set with PutAccountSetting or PutAccountSettingDefault.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the cluster setting. The only supported value is
-- @containerInsights@.
clusterSetting_name :: Lens.Lens' ClusterSetting (Prelude.Maybe ClusterSettingName)
clusterSetting_name = Lens.lens (\ClusterSetting' {name} -> name) (\s@ClusterSetting' {} a -> s {name = a} :: ClusterSetting)

-- | The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
-- Container Insights will be enabled for the cluster, otherwise it will be
-- disabled unless the @containerInsights@ account setting is enabled. If a
-- cluster value is specified, it will override the @containerInsights@
-- value set with PutAccountSetting or PutAccountSettingDefault.
clusterSetting_value :: Lens.Lens' ClusterSetting (Prelude.Maybe Prelude.Text)
clusterSetting_value = Lens.lens (\ClusterSetting' {value} -> value) (\s@ClusterSetting' {} a -> s {value = a} :: ClusterSetting)

instance Prelude.FromJSON ClusterSetting where
  parseJSON =
    Prelude.withObject
      "ClusterSetting"
      ( \x ->
          ClusterSetting'
            Prelude.<$> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "value")
      )

instance Prelude.Hashable ClusterSetting

instance Prelude.NFData ClusterSetting

instance Prelude.ToJSON ClusterSetting where
  toJSON ClusterSetting' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("name" Prelude..=) Prelude.<$> name,
            ("value" Prelude..=) Prelude.<$> value
          ]
      )

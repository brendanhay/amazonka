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
-- Module      : Amazonka.ECS.Types.ClusterSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ClusterSetting where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.ClusterSettingName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The settings to use when creating a cluster. This parameter is used to
-- enable CloudWatch Container Insights for a cluster.
--
-- /See:/ 'newClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { -- | The value to set for the cluster setting. The supported values are
    -- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
    -- Container Insights will be enabled for the cluster, otherwise it will be
    -- disabled unless the @containerInsights@ account setting is enabled. If a
    -- cluster value is specified, it will override the @containerInsights@
    -- value set with PutAccountSetting or PutAccountSettingDefault.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster setting. The only supported value is
    -- @containerInsights@.
    name :: Prelude.Maybe ClusterSettingName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'clusterSetting_value' - The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
-- Container Insights will be enabled for the cluster, otherwise it will be
-- disabled unless the @containerInsights@ account setting is enabled. If a
-- cluster value is specified, it will override the @containerInsights@
-- value set with PutAccountSetting or PutAccountSettingDefault.
--
-- 'name', 'clusterSetting_name' - The name of the cluster setting. The only supported value is
-- @containerInsights@.
newClusterSetting ::
  ClusterSetting
newClusterSetting =
  ClusterSetting'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@. If @enabled@ is specified, CloudWatch
-- Container Insights will be enabled for the cluster, otherwise it will be
-- disabled unless the @containerInsights@ account setting is enabled. If a
-- cluster value is specified, it will override the @containerInsights@
-- value set with PutAccountSetting or PutAccountSettingDefault.
clusterSetting_value :: Lens.Lens' ClusterSetting (Prelude.Maybe Prelude.Text)
clusterSetting_value = Lens.lens (\ClusterSetting' {value} -> value) (\s@ClusterSetting' {} a -> s {value = a} :: ClusterSetting)

-- | The name of the cluster setting. The only supported value is
-- @containerInsights@.
clusterSetting_name :: Lens.Lens' ClusterSetting (Prelude.Maybe ClusterSettingName)
clusterSetting_name = Lens.lens (\ClusterSetting' {name} -> name) (\s@ClusterSetting' {} a -> s {name = a} :: ClusterSetting)

instance Core.FromJSON ClusterSetting where
  parseJSON =
    Core.withObject
      "ClusterSetting"
      ( \x ->
          ClusterSetting'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable ClusterSetting where
  hashWithSalt _salt ClusterSetting' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData ClusterSetting where
  rnf ClusterSetting' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON ClusterSetting where
  toJSON ClusterSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name
          ]
      )

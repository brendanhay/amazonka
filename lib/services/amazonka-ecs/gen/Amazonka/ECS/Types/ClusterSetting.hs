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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ClusterSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ClusterSettingName
import qualified Amazonka.Prelude as Prelude

-- | The settings to use when creating a cluster. This parameter is used to
-- turn on CloudWatch Container Insights for a cluster.
--
-- /See:/ 'newClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { -- | The name of the cluster setting. The value is @containerInsights@ .
    name :: Prelude.Maybe ClusterSettingName,
    -- | The value to set for the cluster setting. The supported values are
    -- @enabled@ and @disabled@.
    --
    -- If you set @name@ to @containerInsights@ and @value@ to @enabled@,
    -- CloudWatch Container Insights will be on for the cluster, otherwise it
    -- will be off unless the @containerInsights@ account setting is turned on.
    -- If a cluster value is specified, it will override the
    -- @containerInsights@ value set with
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSetting.html PutAccountSetting>
    -- or
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSettingDefault.html PutAccountSettingDefault>.
    value :: Prelude.Maybe Prelude.Text
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
-- 'name', 'clusterSetting_name' - The name of the cluster setting. The value is @containerInsights@ .
--
-- 'value', 'clusterSetting_value' - The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@.
--
-- If you set @name@ to @containerInsights@ and @value@ to @enabled@,
-- CloudWatch Container Insights will be on for the cluster, otherwise it
-- will be off unless the @containerInsights@ account setting is turned on.
-- If a cluster value is specified, it will override the
-- @containerInsights@ value set with
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSetting.html PutAccountSetting>
-- or
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSettingDefault.html PutAccountSettingDefault>.
newClusterSetting ::
  ClusterSetting
newClusterSetting =
  ClusterSetting'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the cluster setting. The value is @containerInsights@ .
clusterSetting_name :: Lens.Lens' ClusterSetting (Prelude.Maybe ClusterSettingName)
clusterSetting_name = Lens.lens (\ClusterSetting' {name} -> name) (\s@ClusterSetting' {} a -> s {name = a} :: ClusterSetting)

-- | The value to set for the cluster setting. The supported values are
-- @enabled@ and @disabled@.
--
-- If you set @name@ to @containerInsights@ and @value@ to @enabled@,
-- CloudWatch Container Insights will be on for the cluster, otherwise it
-- will be off unless the @containerInsights@ account setting is turned on.
-- If a cluster value is specified, it will override the
-- @containerInsights@ value set with
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSetting.html PutAccountSetting>
-- or
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_PutAccountSettingDefault.html PutAccountSettingDefault>.
clusterSetting_value :: Lens.Lens' ClusterSetting (Prelude.Maybe Prelude.Text)
clusterSetting_value = Lens.lens (\ClusterSetting' {value} -> value) (\s@ClusterSetting' {} a -> s {value = a} :: ClusterSetting)

instance Data.FromJSON ClusterSetting where
  parseJSON =
    Data.withObject
      "ClusterSetting"
      ( \x ->
          ClusterSetting'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ClusterSetting where
  hashWithSalt _salt ClusterSetting' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ClusterSetting where
  rnf ClusterSetting' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ClusterSetting where
  toJSON ClusterSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("value" Data..=) Prelude.<$> value
          ]
      )

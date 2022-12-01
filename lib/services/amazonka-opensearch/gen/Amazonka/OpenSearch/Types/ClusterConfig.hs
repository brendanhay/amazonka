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
-- Module      : Amazonka.OpenSearch.Types.ClusterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ClusterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.ColdStorageOptions
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
import Amazonka.OpenSearch.Types.ZoneAwarenessConfig
import qualified Amazonka.Prelude as Prelude

-- | Container for the cluster configuration of an OpenSearch Service domain.
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html Creating and managing Amazon OpenSearch Service domains>.
--
-- /See:/ 'newClusterConfig' smart constructor.
data ClusterConfig = ClusterConfig'
  { -- | The number of warm nodes in the cluster.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | Container for cold storage configuration options.
    coldStorageOptions :: Prelude.Maybe ColdStorageOptions,
    -- | OpenSearch Service instance type of the dedicated master nodes in the
    -- cluster.
    dedicatedMasterType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | Indicates whether multiple Availability Zones are enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether dedicated master nodes are enabled for the
    -- cluster.@True@ if the cluster will use a dedicated master node.@False@
    -- if the cluster will not.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for the cluster\'s warm nodes.
    warmType :: Prelude.Maybe OpenSearchWarmPartitionInstanceType,
    -- | Instance type of data nodes in the cluster.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | Container for zone awareness configuration options. Only required if
    -- @ZoneAwarenessEnabled@ is @true@.
    zoneAwarenessConfig :: Prelude.Maybe ZoneAwarenessConfig,
    -- | Number of dedicated master nodes in the cluster. This number must be
    -- greater than 1, otherwise you receive a validation exception.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable warm storage for the cluster.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Number of dedicated master nodes in the cluster. This number must be
    -- greater than 1, otherwise you receive a validation exception.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warmCount', 'clusterConfig_warmCount' - The number of warm nodes in the cluster.
--
-- 'coldStorageOptions', 'clusterConfig_coldStorageOptions' - Container for cold storage configuration options.
--
-- 'dedicatedMasterType', 'clusterConfig_dedicatedMasterType' - OpenSearch Service instance type of the dedicated master nodes in the
-- cluster.
--
-- 'zoneAwarenessEnabled', 'clusterConfig_zoneAwarenessEnabled' - Indicates whether multiple Availability Zones are enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
--
-- 'dedicatedMasterEnabled', 'clusterConfig_dedicatedMasterEnabled' - Indicates whether dedicated master nodes are enabled for the
-- cluster.@True@ if the cluster will use a dedicated master node.@False@
-- if the cluster will not.
--
-- 'warmType', 'clusterConfig_warmType' - The instance type for the cluster\'s warm nodes.
--
-- 'instanceType', 'clusterConfig_instanceType' - Instance type of data nodes in the cluster.
--
-- 'zoneAwarenessConfig', 'clusterConfig_zoneAwarenessConfig' - Container for zone awareness configuration options. Only required if
-- @ZoneAwarenessEnabled@ is @true@.
--
-- 'instanceCount', 'clusterConfig_instanceCount' - Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
--
-- 'warmEnabled', 'clusterConfig_warmEnabled' - Whether to enable warm storage for the cluster.
--
-- 'dedicatedMasterCount', 'clusterConfig_dedicatedMasterCount' - Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
newClusterConfig ::
  ClusterConfig
newClusterConfig =
  ClusterConfig'
    { warmCount = Prelude.Nothing,
      coldStorageOptions = Prelude.Nothing,
      dedicatedMasterType = Prelude.Nothing,
      zoneAwarenessEnabled = Prelude.Nothing,
      dedicatedMasterEnabled = Prelude.Nothing,
      warmType = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      zoneAwarenessConfig = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      warmEnabled = Prelude.Nothing,
      dedicatedMasterCount = Prelude.Nothing
    }

-- | The number of warm nodes in the cluster.
clusterConfig_warmCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_warmCount = Lens.lens (\ClusterConfig' {warmCount} -> warmCount) (\s@ClusterConfig' {} a -> s {warmCount = a} :: ClusterConfig)

-- | Container for cold storage configuration options.
clusterConfig_coldStorageOptions :: Lens.Lens' ClusterConfig (Prelude.Maybe ColdStorageOptions)
clusterConfig_coldStorageOptions = Lens.lens (\ClusterConfig' {coldStorageOptions} -> coldStorageOptions) (\s@ClusterConfig' {} a -> s {coldStorageOptions = a} :: ClusterConfig)

-- | OpenSearch Service instance type of the dedicated master nodes in the
-- cluster.
clusterConfig_dedicatedMasterType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_dedicatedMasterType = Lens.lens (\ClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ClusterConfig' {} a -> s {dedicatedMasterType = a} :: ClusterConfig)

-- | Indicates whether multiple Availability Zones are enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
clusterConfig_zoneAwarenessEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_zoneAwarenessEnabled = Lens.lens (\ClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ClusterConfig)

-- | Indicates whether dedicated master nodes are enabled for the
-- cluster.@True@ if the cluster will use a dedicated master node.@False@
-- if the cluster will not.
clusterConfig_dedicatedMasterEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_dedicatedMasterEnabled = Lens.lens (\ClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ClusterConfig)

-- | The instance type for the cluster\'s warm nodes.
clusterConfig_warmType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchWarmPartitionInstanceType)
clusterConfig_warmType = Lens.lens (\ClusterConfig' {warmType} -> warmType) (\s@ClusterConfig' {} a -> s {warmType = a} :: ClusterConfig)

-- | Instance type of data nodes in the cluster.
clusterConfig_instanceType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_instanceType = Lens.lens (\ClusterConfig' {instanceType} -> instanceType) (\s@ClusterConfig' {} a -> s {instanceType = a} :: ClusterConfig)

-- | Container for zone awareness configuration options. Only required if
-- @ZoneAwarenessEnabled@ is @true@.
clusterConfig_zoneAwarenessConfig :: Lens.Lens' ClusterConfig (Prelude.Maybe ZoneAwarenessConfig)
clusterConfig_zoneAwarenessConfig = Lens.lens (\ClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ClusterConfig)

-- | Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
clusterConfig_instanceCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_instanceCount = Lens.lens (\ClusterConfig' {instanceCount} -> instanceCount) (\s@ClusterConfig' {} a -> s {instanceCount = a} :: ClusterConfig)

-- | Whether to enable warm storage for the cluster.
clusterConfig_warmEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_warmEnabled = Lens.lens (\ClusterConfig' {warmEnabled} -> warmEnabled) (\s@ClusterConfig' {} a -> s {warmEnabled = a} :: ClusterConfig)

-- | Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
clusterConfig_dedicatedMasterCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_dedicatedMasterCount = Lens.lens (\ClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ClusterConfig)

instance Core.FromJSON ClusterConfig where
  parseJSON =
    Core.withObject
      "ClusterConfig"
      ( \x ->
          ClusterConfig'
            Prelude.<$> (x Core..:? "WarmCount")
            Prelude.<*> (x Core..:? "ColdStorageOptions")
            Prelude.<*> (x Core..:? "DedicatedMasterType")
            Prelude.<*> (x Core..:? "ZoneAwarenessEnabled")
            Prelude.<*> (x Core..:? "DedicatedMasterEnabled")
            Prelude.<*> (x Core..:? "WarmType")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "ZoneAwarenessConfig")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "WarmEnabled")
            Prelude.<*> (x Core..:? "DedicatedMasterCount")
      )

instance Prelude.Hashable ClusterConfig where
  hashWithSalt _salt ClusterConfig' {..} =
    _salt `Prelude.hashWithSalt` warmCount
      `Prelude.hashWithSalt` coldStorageOptions
      `Prelude.hashWithSalt` dedicatedMasterType
      `Prelude.hashWithSalt` zoneAwarenessEnabled
      `Prelude.hashWithSalt` dedicatedMasterEnabled
      `Prelude.hashWithSalt` warmType
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` zoneAwarenessConfig
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` warmEnabled
      `Prelude.hashWithSalt` dedicatedMasterCount

instance Prelude.NFData ClusterConfig where
  rnf ClusterConfig' {..} =
    Prelude.rnf warmCount
      `Prelude.seq` Prelude.rnf coldStorageOptions
      `Prelude.seq` Prelude.rnf dedicatedMasterType
      `Prelude.seq` Prelude.rnf zoneAwarenessEnabled
      `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
      `Prelude.seq` Prelude.rnf warmType
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf zoneAwarenessConfig
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf warmEnabled
      `Prelude.seq` Prelude.rnf dedicatedMasterCount

instance Core.ToJSON ClusterConfig where
  toJSON ClusterConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WarmCount" Core..=) Prelude.<$> warmCount,
            ("ColdStorageOptions" Core..=)
              Prelude.<$> coldStorageOptions,
            ("DedicatedMasterType" Core..=)
              Prelude.<$> dedicatedMasterType,
            ("ZoneAwarenessEnabled" Core..=)
              Prelude.<$> zoneAwarenessEnabled,
            ("DedicatedMasterEnabled" Core..=)
              Prelude.<$> dedicatedMasterEnabled,
            ("WarmType" Core..=) Prelude.<$> warmType,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("ZoneAwarenessConfig" Core..=)
              Prelude.<$> zoneAwarenessConfig,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("WarmEnabled" Core..=) Prelude.<$> warmEnabled,
            ("DedicatedMasterCount" Core..=)
              Prelude.<$> dedicatedMasterCount
          ]
      )

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
import qualified Amazonka.Data as Data
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
  { -- | Container for cold storage configuration options.
    coldStorageOptions :: Prelude.Maybe ColdStorageOptions,
    -- | Number of dedicated master nodes in the cluster. This number must be
    -- greater than 1, otherwise you receive a validation exception.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether dedicated master nodes are enabled for the
    -- cluster.@True@ if the cluster will use a dedicated master node.@False@
    -- if the cluster will not.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | OpenSearch Service instance type of the dedicated master nodes in the
    -- cluster.
    dedicatedMasterType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | Number of dedicated master nodes in the cluster. This number must be
    -- greater than 1, otherwise you receive a validation exception.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Instance type of data nodes in the cluster.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The number of warm nodes in the cluster.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable warm storage for the cluster.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for the cluster\'s warm nodes.
    warmType :: Prelude.Maybe OpenSearchWarmPartitionInstanceType,
    -- | Container for zone awareness configuration options. Only required if
    -- @ZoneAwarenessEnabled@ is @true@.
    zoneAwarenessConfig :: Prelude.Maybe ZoneAwarenessConfig,
    -- | Indicates whether multiple Availability Zones are enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'coldStorageOptions', 'clusterConfig_coldStorageOptions' - Container for cold storage configuration options.
--
-- 'dedicatedMasterCount', 'clusterConfig_dedicatedMasterCount' - Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
--
-- 'dedicatedMasterEnabled', 'clusterConfig_dedicatedMasterEnabled' - Indicates whether dedicated master nodes are enabled for the
-- cluster.@True@ if the cluster will use a dedicated master node.@False@
-- if the cluster will not.
--
-- 'dedicatedMasterType', 'clusterConfig_dedicatedMasterType' - OpenSearch Service instance type of the dedicated master nodes in the
-- cluster.
--
-- 'instanceCount', 'clusterConfig_instanceCount' - Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
--
-- 'instanceType', 'clusterConfig_instanceType' - Instance type of data nodes in the cluster.
--
-- 'warmCount', 'clusterConfig_warmCount' - The number of warm nodes in the cluster.
--
-- 'warmEnabled', 'clusterConfig_warmEnabled' - Whether to enable warm storage for the cluster.
--
-- 'warmType', 'clusterConfig_warmType' - The instance type for the cluster\'s warm nodes.
--
-- 'zoneAwarenessConfig', 'clusterConfig_zoneAwarenessConfig' - Container for zone awareness configuration options. Only required if
-- @ZoneAwarenessEnabled@ is @true@.
--
-- 'zoneAwarenessEnabled', 'clusterConfig_zoneAwarenessEnabled' - Indicates whether multiple Availability Zones are enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
newClusterConfig ::
  ClusterConfig
newClusterConfig =
  ClusterConfig'
    { coldStorageOptions =
        Prelude.Nothing,
      dedicatedMasterCount = Prelude.Nothing,
      dedicatedMasterEnabled = Prelude.Nothing,
      dedicatedMasterType = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      warmCount = Prelude.Nothing,
      warmEnabled = Prelude.Nothing,
      warmType = Prelude.Nothing,
      zoneAwarenessConfig = Prelude.Nothing,
      zoneAwarenessEnabled = Prelude.Nothing
    }

-- | Container for cold storage configuration options.
clusterConfig_coldStorageOptions :: Lens.Lens' ClusterConfig (Prelude.Maybe ColdStorageOptions)
clusterConfig_coldStorageOptions = Lens.lens (\ClusterConfig' {coldStorageOptions} -> coldStorageOptions) (\s@ClusterConfig' {} a -> s {coldStorageOptions = a} :: ClusterConfig)

-- | Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
clusterConfig_dedicatedMasterCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_dedicatedMasterCount = Lens.lens (\ClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ClusterConfig)

-- | Indicates whether dedicated master nodes are enabled for the
-- cluster.@True@ if the cluster will use a dedicated master node.@False@
-- if the cluster will not.
clusterConfig_dedicatedMasterEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_dedicatedMasterEnabled = Lens.lens (\ClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ClusterConfig)

-- | OpenSearch Service instance type of the dedicated master nodes in the
-- cluster.
clusterConfig_dedicatedMasterType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_dedicatedMasterType = Lens.lens (\ClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ClusterConfig' {} a -> s {dedicatedMasterType = a} :: ClusterConfig)

-- | Number of dedicated master nodes in the cluster. This number must be
-- greater than 1, otherwise you receive a validation exception.
clusterConfig_instanceCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_instanceCount = Lens.lens (\ClusterConfig' {instanceCount} -> instanceCount) (\s@ClusterConfig' {} a -> s {instanceCount = a} :: ClusterConfig)

-- | Instance type of data nodes in the cluster.
clusterConfig_instanceType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_instanceType = Lens.lens (\ClusterConfig' {instanceType} -> instanceType) (\s@ClusterConfig' {} a -> s {instanceType = a} :: ClusterConfig)

-- | The number of warm nodes in the cluster.
clusterConfig_warmCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_warmCount = Lens.lens (\ClusterConfig' {warmCount} -> warmCount) (\s@ClusterConfig' {} a -> s {warmCount = a} :: ClusterConfig)

-- | Whether to enable warm storage for the cluster.
clusterConfig_warmEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_warmEnabled = Lens.lens (\ClusterConfig' {warmEnabled} -> warmEnabled) (\s@ClusterConfig' {} a -> s {warmEnabled = a} :: ClusterConfig)

-- | The instance type for the cluster\'s warm nodes.
clusterConfig_warmType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchWarmPartitionInstanceType)
clusterConfig_warmType = Lens.lens (\ClusterConfig' {warmType} -> warmType) (\s@ClusterConfig' {} a -> s {warmType = a} :: ClusterConfig)

-- | Container for zone awareness configuration options. Only required if
-- @ZoneAwarenessEnabled@ is @true@.
clusterConfig_zoneAwarenessConfig :: Lens.Lens' ClusterConfig (Prelude.Maybe ZoneAwarenessConfig)
clusterConfig_zoneAwarenessConfig = Lens.lens (\ClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ClusterConfig)

-- | Indicates whether multiple Availability Zones are enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>.
clusterConfig_zoneAwarenessEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_zoneAwarenessEnabled = Lens.lens (\ClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ClusterConfig)

instance Data.FromJSON ClusterConfig where
  parseJSON =
    Data.withObject
      "ClusterConfig"
      ( \x ->
          ClusterConfig'
            Prelude.<$> (x Data..:? "ColdStorageOptions")
            Prelude.<*> (x Data..:? "DedicatedMasterCount")
            Prelude.<*> (x Data..:? "DedicatedMasterEnabled")
            Prelude.<*> (x Data..:? "DedicatedMasterType")
            Prelude.<*> (x Data..:? "InstanceCount")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "WarmCount")
            Prelude.<*> (x Data..:? "WarmEnabled")
            Prelude.<*> (x Data..:? "WarmType")
            Prelude.<*> (x Data..:? "ZoneAwarenessConfig")
            Prelude.<*> (x Data..:? "ZoneAwarenessEnabled")
      )

instance Prelude.Hashable ClusterConfig where
  hashWithSalt _salt ClusterConfig' {..} =
    _salt `Prelude.hashWithSalt` coldStorageOptions
      `Prelude.hashWithSalt` dedicatedMasterCount
      `Prelude.hashWithSalt` dedicatedMasterEnabled
      `Prelude.hashWithSalt` dedicatedMasterType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` warmCount
      `Prelude.hashWithSalt` warmEnabled
      `Prelude.hashWithSalt` warmType
      `Prelude.hashWithSalt` zoneAwarenessConfig
      `Prelude.hashWithSalt` zoneAwarenessEnabled

instance Prelude.NFData ClusterConfig where
  rnf ClusterConfig' {..} =
    Prelude.rnf coldStorageOptions
      `Prelude.seq` Prelude.rnf dedicatedMasterCount
      `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
      `Prelude.seq` Prelude.rnf dedicatedMasterType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf warmCount
      `Prelude.seq` Prelude.rnf warmEnabled
      `Prelude.seq` Prelude.rnf warmType
      `Prelude.seq` Prelude.rnf zoneAwarenessConfig
      `Prelude.seq` Prelude.rnf zoneAwarenessEnabled

instance Data.ToJSON ClusterConfig where
  toJSON ClusterConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColdStorageOptions" Data..=)
              Prelude.<$> coldStorageOptions,
            ("DedicatedMasterCount" Data..=)
              Prelude.<$> dedicatedMasterCount,
            ("DedicatedMasterEnabled" Data..=)
              Prelude.<$> dedicatedMasterEnabled,
            ("DedicatedMasterType" Data..=)
              Prelude.<$> dedicatedMasterType,
            ("InstanceCount" Data..=) Prelude.<$> instanceCount,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("WarmCount" Data..=) Prelude.<$> warmCount,
            ("WarmEnabled" Data..=) Prelude.<$> warmEnabled,
            ("WarmType" Data..=) Prelude.<$> warmType,
            ("ZoneAwarenessConfig" Data..=)
              Prelude.<$> zoneAwarenessConfig,
            ("ZoneAwarenessEnabled" Data..=)
              Prelude.<$> zoneAwarenessEnabled
          ]
      )

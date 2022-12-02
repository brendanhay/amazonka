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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails

-- | Details about the configuration of an OpenSearch cluster.
--
-- /See:/ 'newAwsOpenSearchServiceDomainClusterConfigDetails' smart constructor.
data AwsOpenSearchServiceDomainClusterConfigDetails = AwsOpenSearchServiceDomainClusterConfigDetails'
  { -- | The number of UltraWarm instances.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | The hardware configuration of the computer that hosts the dedicated
    -- master node.
    --
    -- If this attribute is specified, then @DedicatedMasterEnabled@ must be
    -- @true@.
    dedicatedMasterType :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable zone awareness for the OpenSearch domain. When zone
    -- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
    -- and replica index shards across Availability Zones (AZs) in the same
    -- Region. This prevents data loss and minimizes downtime if a node or data
    -- center fails.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether to use a dedicated master node for the OpenSearch domain. A
    -- dedicated master node performs cluster management tasks, but does not
    -- hold data or respond to data upload requests.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The type of UltraWarm instance.
    warmType :: Prelude.Maybe Prelude.Text,
    -- | The instance type for your data nodes.
    --
    -- For a list of valid values, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/supported-instance-types.html Supported instance types in Amazon OpenSearch Service>
    -- in the /Amazon OpenSearch Service Developer Guide/.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Configuration options for zone awareness. Provided if
    -- @ZoneAwarenessEnabled@ is @true@.
    zoneAwarenessConfig :: Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails,
    -- | The number of data nodes to use in the OpenSearch domain.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Whether UltraWarm is enabled.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of instances to use for the master node. If this attribute is
    -- specified, then @DedicatedMasterEnabled@ must be @true@.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainClusterConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warmCount', 'awsOpenSearchServiceDomainClusterConfigDetails_warmCount' - The number of UltraWarm instances.
--
-- 'dedicatedMasterType', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType' - The hardware configuration of the computer that hosts the dedicated
-- master node.
--
-- If this attribute is specified, then @DedicatedMasterEnabled@ must be
-- @true@.
--
-- 'zoneAwarenessEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled' - Whether to enable zone awareness for the OpenSearch domain. When zone
-- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
-- and replica index shards across Availability Zones (AZs) in the same
-- Region. This prevents data loss and minimizes downtime if a node or data
-- center fails.
--
-- 'dedicatedMasterEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled' - Whether to use a dedicated master node for the OpenSearch domain. A
-- dedicated master node performs cluster management tasks, but does not
-- hold data or respond to data upload requests.
--
-- 'warmType', 'awsOpenSearchServiceDomainClusterConfigDetails_warmType' - The type of UltraWarm instance.
--
-- 'instanceType', 'awsOpenSearchServiceDomainClusterConfigDetails_instanceType' - The instance type for your data nodes.
--
-- For a list of valid values, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/supported-instance-types.html Supported instance types in Amazon OpenSearch Service>
-- in the /Amazon OpenSearch Service Developer Guide/.
--
-- 'zoneAwarenessConfig', 'awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig' - Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
--
-- 'instanceCount', 'awsOpenSearchServiceDomainClusterConfigDetails_instanceCount' - The number of data nodes to use in the OpenSearch domain.
--
-- 'warmEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled' - Whether UltraWarm is enabled.
--
-- 'dedicatedMasterCount', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount' - The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
newAwsOpenSearchServiceDomainClusterConfigDetails ::
  AwsOpenSearchServiceDomainClusterConfigDetails
newAwsOpenSearchServiceDomainClusterConfigDetails =
  AwsOpenSearchServiceDomainClusterConfigDetails'
    { warmCount =
        Prelude.Nothing,
      dedicatedMasterType =
        Prelude.Nothing,
      zoneAwarenessEnabled =
        Prelude.Nothing,
      dedicatedMasterEnabled =
        Prelude.Nothing,
      warmType = Prelude.Nothing,
      instanceType =
        Prelude.Nothing,
      zoneAwarenessConfig =
        Prelude.Nothing,
      instanceCount =
        Prelude.Nothing,
      warmEnabled =
        Prelude.Nothing,
      dedicatedMasterCount =
        Prelude.Nothing
    }

-- | The number of UltraWarm instances.
awsOpenSearchServiceDomainClusterConfigDetails_warmCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_warmCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmCount} -> warmCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The hardware configuration of the computer that hosts the dedicated
-- master node.
--
-- If this attribute is specified, then @DedicatedMasterEnabled@ must be
-- @true@.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterType} -> dedicatedMasterType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether to enable zone awareness for the OpenSearch domain. When zone
-- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
-- and replica index shards across Availability Zones (AZs) in the same
-- Region. This prevents data loss and minimizes downtime if a node or data
-- center fails.
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {zoneAwarenessEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether to use a dedicated master node for the OpenSearch domain. A
-- dedicated master node performs cluster management tasks, but does not
-- hold data or respond to data upload requests.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The type of UltraWarm instance.
awsOpenSearchServiceDomainClusterConfigDetails_warmType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_warmType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmType} -> warmType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The instance type for your data nodes.
--
-- For a list of valid values, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/supported-instance-types.html Supported instance types in Amazon OpenSearch Service>
-- in the /Amazon OpenSearch Service Developer Guide/.
awsOpenSearchServiceDomainClusterConfigDetails_instanceType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_instanceType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {instanceType} -> instanceType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {instanceType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails)
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {zoneAwarenessConfig = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The number of data nodes to use in the OpenSearch domain.
awsOpenSearchServiceDomainClusterConfigDetails_instanceCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_instanceCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {instanceCount} -> instanceCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {instanceCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether UltraWarm is enabled.
awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmEnabled} -> warmEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainClusterConfigDetails"
      ( \x ->
          AwsOpenSearchServiceDomainClusterConfigDetails'
            Prelude.<$> (x Data..:? "WarmCount")
              Prelude.<*> (x Data..:? "DedicatedMasterType")
              Prelude.<*> (x Data..:? "ZoneAwarenessEnabled")
              Prelude.<*> (x Data..:? "DedicatedMasterEnabled")
              Prelude.<*> (x Data..:? "WarmType")
              Prelude.<*> (x Data..:? "InstanceType")
              Prelude.<*> (x Data..:? "ZoneAwarenessConfig")
              Prelude.<*> (x Data..:? "InstanceCount")
              Prelude.<*> (x Data..:? "WarmEnabled")
              Prelude.<*> (x Data..:? "DedicatedMasterCount")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` warmCount
        `Prelude.hashWithSalt` dedicatedMasterType
        `Prelude.hashWithSalt` zoneAwarenessEnabled
        `Prelude.hashWithSalt` dedicatedMasterEnabled
        `Prelude.hashWithSalt` warmType
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` zoneAwarenessConfig
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` warmEnabled
        `Prelude.hashWithSalt` dedicatedMasterCount

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  rnf
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      Prelude.rnf warmCount
        `Prelude.seq` Prelude.rnf dedicatedMasterType
        `Prelude.seq` Prelude.rnf zoneAwarenessEnabled
        `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
        `Prelude.seq` Prelude.rnf warmType
        `Prelude.seq` Prelude.rnf instanceType
        `Prelude.seq` Prelude.rnf zoneAwarenessConfig
        `Prelude.seq` Prelude.rnf instanceCount
        `Prelude.seq` Prelude.rnf warmEnabled
        `Prelude.seq` Prelude.rnf dedicatedMasterCount

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  toJSON
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("WarmCount" Data..=) Prelude.<$> warmCount,
              ("DedicatedMasterType" Data..=)
                Prelude.<$> dedicatedMasterType,
              ("ZoneAwarenessEnabled" Data..=)
                Prelude.<$> zoneAwarenessEnabled,
              ("DedicatedMasterEnabled" Data..=)
                Prelude.<$> dedicatedMasterEnabled,
              ("WarmType" Data..=) Prelude.<$> warmType,
              ("InstanceType" Data..=) Prelude.<$> instanceType,
              ("ZoneAwarenessConfig" Data..=)
                Prelude.<$> zoneAwarenessConfig,
              ("InstanceCount" Data..=) Prelude.<$> instanceCount,
              ("WarmEnabled" Data..=) Prelude.<$> warmEnabled,
              ("DedicatedMasterCount" Data..=)
                Prelude.<$> dedicatedMasterCount
            ]
        )

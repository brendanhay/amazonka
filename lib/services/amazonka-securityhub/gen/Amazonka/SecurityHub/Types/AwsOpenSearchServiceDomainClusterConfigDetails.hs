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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails

-- | Details about the configuration of an OpenSearch cluster.
--
-- /See:/ 'newAwsOpenSearchServiceDomainClusterConfigDetails' smart constructor.
data AwsOpenSearchServiceDomainClusterConfigDetails = AwsOpenSearchServiceDomainClusterConfigDetails'
  { -- | The number of instances to use for the master node. If this attribute is
    -- specified, then @DedicatedMasterEnabled@ must be @true@.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int,
    -- | The hardware configuration of the computer that hosts the dedicated
    -- master node.
    --
    -- If this attribute is specified, then @DedicatedMasterEnabled@ must be
    -- @true@.
    dedicatedMasterType :: Prelude.Maybe Prelude.Text,
    -- | Whether to use a dedicated master node for the OpenSearch domain. A
    -- dedicated master node performs cluster management tasks, but does not
    -- hold data or respond to data upload requests.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of data nodes to use in the OpenSearch domain.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable zone awareness for the OpenSearch domain. When zone
    -- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
    -- and replica index shards across Availability Zones (AZs) in the same
    -- Region. This prevents data loss and minimizes downtime if a node or data
    -- center fails.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for your data nodes.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Whether UltraWarm is enabled.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Configuration options for zone awareness. Provided if
    -- @ZoneAwarenessEnabled@ is @true@.
    zoneAwarenessConfig :: Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails,
    -- | The number of UltraWarm instances.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | The type of UltraWarm instance.
    warmType :: Prelude.Maybe Prelude.Text
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
-- 'dedicatedMasterCount', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount' - The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
--
-- 'dedicatedMasterType', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType' - The hardware configuration of the computer that hosts the dedicated
-- master node.
--
-- If this attribute is specified, then @DedicatedMasterEnabled@ must be
-- @true@.
--
-- 'dedicatedMasterEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled' - Whether to use a dedicated master node for the OpenSearch domain. A
-- dedicated master node performs cluster management tasks, but does not
-- hold data or respond to data upload requests.
--
-- 'instanceCount', 'awsOpenSearchServiceDomainClusterConfigDetails_instanceCount' - The number of data nodes to use in the OpenSearch domain.
--
-- 'zoneAwarenessEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled' - Whether to enable zone awareness for the OpenSearch domain. When zone
-- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
-- and replica index shards across Availability Zones (AZs) in the same
-- Region. This prevents data loss and minimizes downtime if a node or data
-- center fails.
--
-- 'instanceType', 'awsOpenSearchServiceDomainClusterConfigDetails_instanceType' - The instance type for your data nodes.
--
-- 'warmEnabled', 'awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled' - Whether UltraWarm is enabled.
--
-- 'zoneAwarenessConfig', 'awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig' - Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
--
-- 'warmCount', 'awsOpenSearchServiceDomainClusterConfigDetails_warmCount' - The number of UltraWarm instances.
--
-- 'warmType', 'awsOpenSearchServiceDomainClusterConfigDetails_warmType' - The type of UltraWarm instance.
newAwsOpenSearchServiceDomainClusterConfigDetails ::
  AwsOpenSearchServiceDomainClusterConfigDetails
newAwsOpenSearchServiceDomainClusterConfigDetails =
  AwsOpenSearchServiceDomainClusterConfigDetails'
    { dedicatedMasterCount =
        Prelude.Nothing,
      dedicatedMasterType =
        Prelude.Nothing,
      dedicatedMasterEnabled =
        Prelude.Nothing,
      instanceCount =
        Prelude.Nothing,
      zoneAwarenessEnabled =
        Prelude.Nothing,
      instanceType =
        Prelude.Nothing,
      warmEnabled =
        Prelude.Nothing,
      zoneAwarenessConfig =
        Prelude.Nothing,
      warmCount = Prelude.Nothing,
      warmType = Prelude.Nothing
    }

-- | The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The hardware configuration of the computer that hosts the dedicated
-- master node.
--
-- If this attribute is specified, then @DedicatedMasterEnabled@ must be
-- @true@.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterType} -> dedicatedMasterType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether to use a dedicated master node for the OpenSearch domain. A
-- dedicated master node performs cluster management tasks, but does not
-- hold data or respond to data upload requests.
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {dedicatedMasterEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The number of data nodes to use in the OpenSearch domain.
awsOpenSearchServiceDomainClusterConfigDetails_instanceCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_instanceCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {instanceCount} -> instanceCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {instanceCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether to enable zone awareness for the OpenSearch domain. When zone
-- awareness is enabled, OpenSearch Service allocates the cluster\'s nodes
-- and replica index shards across Availability Zones (AZs) in the same
-- Region. This prevents data loss and minimizes downtime if a node or data
-- center fails.
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {zoneAwarenessEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The instance type for your data nodes.
awsOpenSearchServiceDomainClusterConfigDetails_instanceType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_instanceType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {instanceType} -> instanceType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {instanceType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Whether UltraWarm is enabled.
awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmEnabled} -> warmEnabled) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmEnabled = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails)
awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {zoneAwarenessConfig = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The number of UltraWarm instances.
awsOpenSearchServiceDomainClusterConfigDetails_warmCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigDetails_warmCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmCount} -> warmCount) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmCount = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

-- | The type of UltraWarm instance.
awsOpenSearchServiceDomainClusterConfigDetails_warmType :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainClusterConfigDetails_warmType = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigDetails' {warmType} -> warmType) (\s@AwsOpenSearchServiceDomainClusterConfigDetails' {} a -> s {warmType = a} :: AwsOpenSearchServiceDomainClusterConfigDetails)

instance
  Core.FromJSON
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainClusterConfigDetails"
      ( \x ->
          AwsOpenSearchServiceDomainClusterConfigDetails'
            Prelude.<$> (x Core..:? "DedicatedMasterCount")
              Prelude.<*> (x Core..:? "DedicatedMasterType")
              Prelude.<*> (x Core..:? "DedicatedMasterEnabled")
              Prelude.<*> (x Core..:? "InstanceCount")
              Prelude.<*> (x Core..:? "ZoneAwarenessEnabled")
              Prelude.<*> (x Core..:? "InstanceType")
              Prelude.<*> (x Core..:? "WarmEnabled")
              Prelude.<*> (x Core..:? "ZoneAwarenessConfig")
              Prelude.<*> (x Core..:? "WarmCount")
              Prelude.<*> (x Core..:? "WarmType")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  hashWithSalt
    salt'
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      salt' `Prelude.hashWithSalt` warmType
        `Prelude.hashWithSalt` warmCount
        `Prelude.hashWithSalt` zoneAwarenessConfig
        `Prelude.hashWithSalt` warmEnabled
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` zoneAwarenessEnabled
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` dedicatedMasterEnabled
        `Prelude.hashWithSalt` dedicatedMasterType
        `Prelude.hashWithSalt` dedicatedMasterCount

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  rnf
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      Prelude.rnf dedicatedMasterCount
        `Prelude.seq` Prelude.rnf warmType
        `Prelude.seq` Prelude.rnf warmCount
        `Prelude.seq` Prelude.rnf zoneAwarenessConfig
        `Prelude.seq` Prelude.rnf warmEnabled
        `Prelude.seq` Prelude.rnf instanceType
        `Prelude.seq` Prelude.rnf zoneAwarenessEnabled
        `Prelude.seq` Prelude.rnf instanceCount
        `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
        `Prelude.seq` Prelude.rnf dedicatedMasterType

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainClusterConfigDetails
  where
  toJSON
    AwsOpenSearchServiceDomainClusterConfigDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("DedicatedMasterCount" Core..=)
                Prelude.<$> dedicatedMasterCount,
              ("DedicatedMasterType" Core..=)
                Prelude.<$> dedicatedMasterType,
              ("DedicatedMasterEnabled" Core..=)
                Prelude.<$> dedicatedMasterEnabled,
              ("InstanceCount" Core..=) Prelude.<$> instanceCount,
              ("ZoneAwarenessEnabled" Core..=)
                Prelude.<$> zoneAwarenessEnabled,
              ("InstanceType" Core..=) Prelude.<$> instanceType,
              ("WarmEnabled" Core..=) Prelude.<$> warmEnabled,
              ("ZoneAwarenessConfig" Core..=)
                Prelude.<$> zoneAwarenessConfig,
              ("WarmCount" Core..=) Prelude.<$> warmCount,
              ("WarmType" Core..=) Prelude.<$> warmType
            ]
        )

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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails

-- | details about the configuration of an OpenSearch cluster.
--
-- /See:/ 'newAwsElasticsearchDomainElasticsearchClusterConfigDetails' smart constructor.
data AwsElasticsearchDomainElasticsearchClusterConfigDetails = AwsElasticsearchDomainElasticsearchClusterConfigDetails'
  { -- | The number of instances to use for the master node. If this attribute is
    -- specified, then @DedicatedMasterEnabled@ must be @true@.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int,
    -- | The hardware configuration of the computer that hosts the dedicated
    -- master node. For example, @m3.medium.elasticsearch@. If this attribute
    -- is specified, then @DedicatedMasterEnabled@ must be @true@.
    dedicatedMasterType :: Prelude.Maybe Prelude.Text,
    -- | Whether to use a dedicated master node for the Elasticsearch domain. A
    -- dedicated master node performs cluster management tasks, but doesn\'t
    -- hold data or respond to data upload requests.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of data nodes to use in the Elasticsearch domain.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable zone awareness for the Elasticsearch domain. When zone
    -- awareness is enabled, OpenSearch allocates the cluster\'s nodes and
    -- replica index shards across Availability Zones in the same Region. This
    -- prevents data loss and minimizes downtime if a node or data center
    -- fails.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for your data nodes. For example,
    -- @m3.medium.elasticsearch@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Configuration options for zone awareness. Provided if
    -- @ZoneAwarenessEnabled@ is @true@.
    zoneAwarenessConfig :: Prelude.Maybe AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainElasticsearchClusterConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedMasterCount', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount' - The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
--
-- 'dedicatedMasterType', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType' - The hardware configuration of the computer that hosts the dedicated
-- master node. For example, @m3.medium.elasticsearch@. If this attribute
-- is specified, then @DedicatedMasterEnabled@ must be @true@.
--
-- 'dedicatedMasterEnabled', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled' - Whether to use a dedicated master node for the Elasticsearch domain. A
-- dedicated master node performs cluster management tasks, but doesn\'t
-- hold data or respond to data upload requests.
--
-- 'instanceCount', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount' - The number of data nodes to use in the Elasticsearch domain.
--
-- 'zoneAwarenessEnabled', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled' - Whether to enable zone awareness for the Elasticsearch domain. When zone
-- awareness is enabled, OpenSearch allocates the cluster\'s nodes and
-- replica index shards across Availability Zones in the same Region. This
-- prevents data loss and minimizes downtime if a node or data center
-- fails.
--
-- 'instanceType', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType' - The instance type for your data nodes. For example,
-- @m3.medium.elasticsearch@.
--
-- 'zoneAwarenessConfig', 'awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig' - Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
newAwsElasticsearchDomainElasticsearchClusterConfigDetails ::
  AwsElasticsearchDomainElasticsearchClusterConfigDetails
newAwsElasticsearchDomainElasticsearchClusterConfigDetails =
  AwsElasticsearchDomainElasticsearchClusterConfigDetails'
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
      zoneAwarenessConfig =
        Prelude.Nothing
    }

-- | The number of instances to use for the master node. If this attribute is
-- specified, then @DedicatedMasterEnabled@ must be @true@.
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {dedicatedMasterCount = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | The hardware configuration of the computer that hosts the dedicated
-- master node. For example, @m3.medium.elasticsearch@. If this attribute
-- is specified, then @DedicatedMasterEnabled@ must be @true@.
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {dedicatedMasterType} -> dedicatedMasterType) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {dedicatedMasterType = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | Whether to use a dedicated master node for the Elasticsearch domain. A
-- dedicated master node performs cluster management tasks, but doesn\'t
-- hold data or respond to data upload requests.
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {dedicatedMasterEnabled = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | The number of data nodes to use in the Elasticsearch domain.
awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Int)
awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {instanceCount} -> instanceCount) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {instanceCount = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | Whether to enable zone awareness for the Elasticsearch domain. When zone
-- awareness is enabled, OpenSearch allocates the cluster\'s nodes and
-- replica index shards across Availability Zones in the same Region. This
-- prevents data loss and minimizes downtime if a node or data center
-- fails.
awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {zoneAwarenessEnabled = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | The instance type for your data nodes. For example,
-- @m3.medium.elasticsearch@.
awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {instanceType} -> instanceType) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {instanceType = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

-- | Configuration options for zone awareness. Provided if
-- @ZoneAwarenessEnabled@ is @true@.
awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigDetails (Prelude.Maybe AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails)
awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigDetails' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@AwsElasticsearchDomainElasticsearchClusterConfigDetails' {} a -> s {zoneAwarenessConfig = a} :: AwsElasticsearchDomainElasticsearchClusterConfigDetails)

instance
  Core.FromJSON
    AwsElasticsearchDomainElasticsearchClusterConfigDetails
  where
  parseJSON =
    Core.withObject
      "AwsElasticsearchDomainElasticsearchClusterConfigDetails"
      ( \x ->
          AwsElasticsearchDomainElasticsearchClusterConfigDetails'
            Prelude.<$> (x Core..:? "DedicatedMasterCount")
              Prelude.<*> (x Core..:? "DedicatedMasterType")
              Prelude.<*> (x Core..:? "DedicatedMasterEnabled")
              Prelude.<*> (x Core..:? "InstanceCount")
              Prelude.<*> (x Core..:? "ZoneAwarenessEnabled")
              Prelude.<*> (x Core..:? "InstanceType")
              Prelude.<*> (x Core..:? "ZoneAwarenessConfig")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainElasticsearchClusterConfigDetails
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainElasticsearchClusterConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` dedicatedMasterCount
        `Prelude.hashWithSalt` dedicatedMasterType
        `Prelude.hashWithSalt` dedicatedMasterEnabled
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` zoneAwarenessEnabled
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` zoneAwarenessConfig

instance
  Prelude.NFData
    AwsElasticsearchDomainElasticsearchClusterConfigDetails
  where
  rnf
    AwsElasticsearchDomainElasticsearchClusterConfigDetails' {..} =
      Prelude.rnf dedicatedMasterCount
        `Prelude.seq` Prelude.rnf dedicatedMasterType
        `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
        `Prelude.seq` Prelude.rnf instanceCount
        `Prelude.seq` Prelude.rnf zoneAwarenessEnabled
        `Prelude.seq` Prelude.rnf instanceType
        `Prelude.seq` Prelude.rnf zoneAwarenessConfig

instance
  Core.ToJSON
    AwsElasticsearchDomainElasticsearchClusterConfigDetails
  where
  toJSON
    AwsElasticsearchDomainElasticsearchClusterConfigDetails' {..} =
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
              ("ZoneAwarenessConfig" Core..=)
                Prelude.<$> zoneAwarenessConfig
            ]
        )

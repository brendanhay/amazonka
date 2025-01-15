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
-- Module      : Amazonka.DMS.Types.ReplicationPendingModifiedValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the values of pending modifications to a
-- replication instance. This data type is an object of the
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ReplicationInstance.html ReplicationInstance>
-- user-defined data type.
--
-- /See:/ 'newReplicationPendingModifiedValues' smart constructor.
data ReplicationPendingModifiedValues = ReplicationPendingModifiedValues'
  { -- | The amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The engine version number of the replication instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The type of IP address protocol used by a replication instance, such as
    -- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
    -- IPv6 only is not yet supported.
    networkType :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'replicationPendingModifiedValues_allocatedStorage' - The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'engineVersion', 'replicationPendingModifiedValues_engineVersion' - The engine version number of the replication instance.
--
-- 'multiAZ', 'replicationPendingModifiedValues_multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
--
-- 'networkType', 'replicationPendingModifiedValues_networkType' - The type of IP address protocol used by a replication instance, such as
-- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
-- IPv6 only is not yet supported.
--
-- 'replicationInstanceClass', 'replicationPendingModifiedValues_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
newReplicationPendingModifiedValues ::
  ReplicationPendingModifiedValues
newReplicationPendingModifiedValues =
  ReplicationPendingModifiedValues'
    { allocatedStorage =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      networkType = Prelude.Nothing,
      replicationInstanceClass =
        Prelude.Nothing
    }

-- | The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
replicationPendingModifiedValues_allocatedStorage :: Lens.Lens' ReplicationPendingModifiedValues (Prelude.Maybe Prelude.Int)
replicationPendingModifiedValues_allocatedStorage = Lens.lens (\ReplicationPendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@ReplicationPendingModifiedValues' {} a -> s {allocatedStorage = a} :: ReplicationPendingModifiedValues)

-- | The engine version number of the replication instance.
replicationPendingModifiedValues_engineVersion :: Lens.Lens' ReplicationPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationPendingModifiedValues_engineVersion = Lens.lens (\ReplicationPendingModifiedValues' {engineVersion} -> engineVersion) (\s@ReplicationPendingModifiedValues' {} a -> s {engineVersion = a} :: ReplicationPendingModifiedValues)

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
replicationPendingModifiedValues_multiAZ :: Lens.Lens' ReplicationPendingModifiedValues (Prelude.Maybe Prelude.Bool)
replicationPendingModifiedValues_multiAZ = Lens.lens (\ReplicationPendingModifiedValues' {multiAZ} -> multiAZ) (\s@ReplicationPendingModifiedValues' {} a -> s {multiAZ = a} :: ReplicationPendingModifiedValues)

-- | The type of IP address protocol used by a replication instance, such as
-- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
-- IPv6 only is not yet supported.
replicationPendingModifiedValues_networkType :: Lens.Lens' ReplicationPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationPendingModifiedValues_networkType = Lens.lens (\ReplicationPendingModifiedValues' {networkType} -> networkType) (\s@ReplicationPendingModifiedValues' {} a -> s {networkType = a} :: ReplicationPendingModifiedValues)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
replicationPendingModifiedValues_replicationInstanceClass :: Lens.Lens' ReplicationPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationPendingModifiedValues_replicationInstanceClass = Lens.lens (\ReplicationPendingModifiedValues' {replicationInstanceClass} -> replicationInstanceClass) (\s@ReplicationPendingModifiedValues' {} a -> s {replicationInstanceClass = a} :: ReplicationPendingModifiedValues)

instance
  Data.FromJSON
    ReplicationPendingModifiedValues
  where
  parseJSON =
    Data.withObject
      "ReplicationPendingModifiedValues"
      ( \x ->
          ReplicationPendingModifiedValues'
            Prelude.<$> (x Data..:? "AllocatedStorage")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "MultiAZ")
            Prelude.<*> (x Data..:? "NetworkType")
            Prelude.<*> (x Data..:? "ReplicationInstanceClass")
      )

instance
  Prelude.Hashable
    ReplicationPendingModifiedValues
  where
  hashWithSalt
    _salt
    ReplicationPendingModifiedValues' {..} =
      _salt
        `Prelude.hashWithSalt` allocatedStorage
        `Prelude.hashWithSalt` engineVersion
        `Prelude.hashWithSalt` multiAZ
        `Prelude.hashWithSalt` networkType
        `Prelude.hashWithSalt` replicationInstanceClass

instance
  Prelude.NFData
    ReplicationPendingModifiedValues
  where
  rnf ReplicationPendingModifiedValues' {..} =
    Prelude.rnf allocatedStorage `Prelude.seq`
      Prelude.rnf engineVersion `Prelude.seq`
        Prelude.rnf multiAZ `Prelude.seq`
          Prelude.rnf networkType `Prelude.seq`
            Prelude.rnf replicationInstanceClass

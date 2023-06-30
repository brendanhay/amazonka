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
-- Module      : Amazonka.DMS.Types.OrderableReplicationInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.OrderableReplicationInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.ReleaseStatusValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | In response to the @DescribeOrderableReplicationInstances@ operation,
-- this object describes an available replication instance. This
-- description includes the replication instance\'s type, engine version,
-- and allocated storage.
--
-- /See:/ 'newOrderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
  { -- | List of Availability Zones for this replication instance.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The default amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    defaultAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The version of the replication engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    includedAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of storage (in gigabytes) that can be allocated for
    -- the replication instance.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of storage (in gigabytes) that can be allocated for
    -- the replication instance.
    minAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The value returned when the specified @EngineVersion@ of the replication
    -- instance is in Beta or test mode. This indicates some features might not
    -- work as expected.
    --
    -- DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and later.
    releaseStatus :: Prelude.Maybe ReleaseStatusValues,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. For example to specify the
    -- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The type of storage used by the replication instance.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderableReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'orderableReplicationInstance_availabilityZones' - List of Availability Zones for this replication instance.
--
-- 'defaultAllocatedStorage', 'orderableReplicationInstance_defaultAllocatedStorage' - The default amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'engineVersion', 'orderableReplicationInstance_engineVersion' - The version of the replication engine.
--
-- 'includedAllocatedStorage', 'orderableReplicationInstance_includedAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'maxAllocatedStorage', 'orderableReplicationInstance_maxAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
--
-- 'minAllocatedStorage', 'orderableReplicationInstance_minAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
--
-- 'releaseStatus', 'orderableReplicationInstance_releaseStatus' - The value returned when the specified @EngineVersion@ of the replication
-- instance is in Beta or test mode. This indicates some features might not
-- work as expected.
--
-- DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and later.
--
-- 'replicationInstanceClass', 'orderableReplicationInstance_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
--
-- 'storageType', 'orderableReplicationInstance_storageType' - The type of storage used by the replication instance.
newOrderableReplicationInstance ::
  OrderableReplicationInstance
newOrderableReplicationInstance =
  OrderableReplicationInstance'
    { availabilityZones =
        Prelude.Nothing,
      defaultAllocatedStorage = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      includedAllocatedStorage = Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      minAllocatedStorage = Prelude.Nothing,
      releaseStatus = Prelude.Nothing,
      replicationInstanceClass = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | List of Availability Zones for this replication instance.
orderableReplicationInstance_availabilityZones :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe [Prelude.Text])
orderableReplicationInstance_availabilityZones = Lens.lens (\OrderableReplicationInstance' {availabilityZones} -> availabilityZones) (\s@OrderableReplicationInstance' {} a -> s {availabilityZones = a} :: OrderableReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | The default amount of storage (in gigabytes) that is allocated for the
-- replication instance.
orderableReplicationInstance_defaultAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_defaultAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {defaultAllocatedStorage} -> defaultAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {defaultAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The version of the replication engine.
orderableReplicationInstance_engineVersion :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_engineVersion = Lens.lens (\OrderableReplicationInstance' {engineVersion} -> engineVersion) (\s@OrderableReplicationInstance' {} a -> s {engineVersion = a} :: OrderableReplicationInstance)

-- | The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
orderableReplicationInstance_includedAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_includedAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {includedAllocatedStorage} -> includedAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {includedAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
orderableReplicationInstance_maxAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_maxAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {maxAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
orderableReplicationInstance_minAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_minAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {minAllocatedStorage} -> minAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {minAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The value returned when the specified @EngineVersion@ of the replication
-- instance is in Beta or test mode. This indicates some features might not
-- work as expected.
--
-- DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and later.
orderableReplicationInstance_releaseStatus :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe ReleaseStatusValues)
orderableReplicationInstance_releaseStatus = Lens.lens (\OrderableReplicationInstance' {releaseStatus} -> releaseStatus) (\s@OrderableReplicationInstance' {} a -> s {releaseStatus = a} :: OrderableReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
orderableReplicationInstance_replicationInstanceClass :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_replicationInstanceClass = Lens.lens (\OrderableReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@OrderableReplicationInstance' {} a -> s {replicationInstanceClass = a} :: OrderableReplicationInstance)

-- | The type of storage used by the replication instance.
orderableReplicationInstance_storageType :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_storageType = Lens.lens (\OrderableReplicationInstance' {storageType} -> storageType) (\s@OrderableReplicationInstance' {} a -> s {storageType = a} :: OrderableReplicationInstance)

instance Data.FromJSON OrderableReplicationInstance where
  parseJSON =
    Data.withObject
      "OrderableReplicationInstance"
      ( \x ->
          OrderableReplicationInstance'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DefaultAllocatedStorage")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "IncludedAllocatedStorage")
            Prelude.<*> (x Data..:? "MaxAllocatedStorage")
            Prelude.<*> (x Data..:? "MinAllocatedStorage")
            Prelude.<*> (x Data..:? "ReleaseStatus")
            Prelude.<*> (x Data..:? "ReplicationInstanceClass")
            Prelude.<*> (x Data..:? "StorageType")
      )

instance
  Prelude.Hashable
    OrderableReplicationInstance
  where
  hashWithSalt _salt OrderableReplicationInstance' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` defaultAllocatedStorage
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` includedAllocatedStorage
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` minAllocatedStorage
      `Prelude.hashWithSalt` releaseStatus
      `Prelude.hashWithSalt` replicationInstanceClass
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData OrderableReplicationInstance where
  rnf OrderableReplicationInstance' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf defaultAllocatedStorage
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf includedAllocatedStorage
      `Prelude.seq` Prelude.rnf maxAllocatedStorage
      `Prelude.seq` Prelude.rnf minAllocatedStorage
      `Prelude.seq` Prelude.rnf releaseStatus
      `Prelude.seq` Prelude.rnf replicationInstanceClass
      `Prelude.seq` Prelude.rnf storageType

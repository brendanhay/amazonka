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
-- Module      : Network.AWS.DMS.Types.OrderableReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OrderableReplicationInstance where

import Network.AWS.DMS.Types.ReleaseStatusValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | In response to the @DescribeOrderableReplicationInstances@ operation,
-- this object describes an available replication instance. This
-- description includes the replication instance\'s type, engine version,
-- and allocated storage.
--
-- /See:/ 'newOrderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
  { -- | List of Availability Zones for this replication instance.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The minimum amount of storage (in gigabytes) that can be allocated for
    -- the replication instance.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The type of storage used by the replication instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The default amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    defaultAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    includedAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The value returned when the specified @EngineVersion@ of the replication
    -- instance is in Beta or test mode. This indicates some features might not
    -- work as expected.
    --
    -- AWS DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and
    -- later.
    releaseStatus :: Prelude.Maybe ReleaseStatusValues,
    -- | The version of the replication engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. For example to specify the
    -- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The minimum amount of storage (in gigabytes) that can be allocated for
    -- the replication instance.
    minAllocatedStorage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'maxAllocatedStorage', 'orderableReplicationInstance_maxAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
--
-- 'storageType', 'orderableReplicationInstance_storageType' - The type of storage used by the replication instance.
--
-- 'defaultAllocatedStorage', 'orderableReplicationInstance_defaultAllocatedStorage' - The default amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'includedAllocatedStorage', 'orderableReplicationInstance_includedAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'releaseStatus', 'orderableReplicationInstance_releaseStatus' - The value returned when the specified @EngineVersion@ of the replication
-- instance is in Beta or test mode. This indicates some features might not
-- work as expected.
--
-- AWS DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and
-- later.
--
-- 'engineVersion', 'orderableReplicationInstance_engineVersion' - The version of the replication engine.
--
-- 'replicationInstanceClass', 'orderableReplicationInstance_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
--
-- 'minAllocatedStorage', 'orderableReplicationInstance_minAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
newOrderableReplicationInstance ::
  OrderableReplicationInstance
newOrderableReplicationInstance =
  OrderableReplicationInstance'
    { availabilityZones =
        Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      storageType = Prelude.Nothing,
      defaultAllocatedStorage = Prelude.Nothing,
      includedAllocatedStorage = Prelude.Nothing,
      releaseStatus = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      replicationInstanceClass = Prelude.Nothing,
      minAllocatedStorage = Prelude.Nothing
    }

-- | List of Availability Zones for this replication instance.
orderableReplicationInstance_availabilityZones :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe [Prelude.Text])
orderableReplicationInstance_availabilityZones = Lens.lens (\OrderableReplicationInstance' {availabilityZones} -> availabilityZones) (\s@OrderableReplicationInstance' {} a -> s {availabilityZones = a} :: OrderableReplicationInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
orderableReplicationInstance_maxAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_maxAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {maxAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The type of storage used by the replication instance.
orderableReplicationInstance_storageType :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_storageType = Lens.lens (\OrderableReplicationInstance' {storageType} -> storageType) (\s@OrderableReplicationInstance' {} a -> s {storageType = a} :: OrderableReplicationInstance)

-- | The default amount of storage (in gigabytes) that is allocated for the
-- replication instance.
orderableReplicationInstance_defaultAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_defaultAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {defaultAllocatedStorage} -> defaultAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {defaultAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
orderableReplicationInstance_includedAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_includedAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {includedAllocatedStorage} -> includedAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {includedAllocatedStorage = a} :: OrderableReplicationInstance)

-- | The value returned when the specified @EngineVersion@ of the replication
-- instance is in Beta or test mode. This indicates some features might not
-- work as expected.
--
-- AWS DMS supports the @ReleaseStatus@ parameter in versions 3.1.4 and
-- later.
orderableReplicationInstance_releaseStatus :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe ReleaseStatusValues)
orderableReplicationInstance_releaseStatus = Lens.lens (\OrderableReplicationInstance' {releaseStatus} -> releaseStatus) (\s@OrderableReplicationInstance' {} a -> s {releaseStatus = a} :: OrderableReplicationInstance)

-- | The version of the replication engine.
orderableReplicationInstance_engineVersion :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_engineVersion = Lens.lens (\OrderableReplicationInstance' {engineVersion} -> engineVersion) (\s@OrderableReplicationInstance' {} a -> s {engineVersion = a} :: OrderableReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
orderableReplicationInstance_replicationInstanceClass :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Text)
orderableReplicationInstance_replicationInstanceClass = Lens.lens (\OrderableReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@OrderableReplicationInstance' {} a -> s {replicationInstanceClass = a} :: OrderableReplicationInstance)

-- | The minimum amount of storage (in gigabytes) that can be allocated for
-- the replication instance.
orderableReplicationInstance_minAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Prelude.Maybe Prelude.Int)
orderableReplicationInstance_minAllocatedStorage = Lens.lens (\OrderableReplicationInstance' {minAllocatedStorage} -> minAllocatedStorage) (\s@OrderableReplicationInstance' {} a -> s {minAllocatedStorage = a} :: OrderableReplicationInstance)

instance
  Prelude.FromJSON
    OrderableReplicationInstance
  where
  parseJSON =
    Prelude.withObject
      "OrderableReplicationInstance"
      ( \x ->
          OrderableReplicationInstance'
            Prelude.<$> ( x Prelude..:? "AvailabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "MaxAllocatedStorage")
            Prelude.<*> (x Prelude..:? "StorageType")
            Prelude.<*> (x Prelude..:? "DefaultAllocatedStorage")
            Prelude.<*> (x Prelude..:? "IncludedAllocatedStorage")
            Prelude.<*> (x Prelude..:? "ReleaseStatus")
            Prelude.<*> (x Prelude..:? "EngineVersion")
            Prelude.<*> (x Prelude..:? "ReplicationInstanceClass")
            Prelude.<*> (x Prelude..:? "MinAllocatedStorage")
      )

instance
  Prelude.Hashable
    OrderableReplicationInstance

instance Prelude.NFData OrderableReplicationInstance

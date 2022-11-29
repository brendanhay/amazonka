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
-- Module      : Amazonka.RDS.Types.ReservedDBInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ReservedDBInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the
-- @DescribeReservedDBInstances@ and @PurchaseReservedDBInstancesOffering@
-- actions.
--
-- /See:/ 'newReservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { -- | The DB instance class for the reserved DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The number of reserved DB instances.
    dbInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the reservation.
    reservedDBInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The state of the reserved DB instance.
    state :: Prelude.Maybe Prelude.Text,
    -- | The offering type of this reserved DB instance.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the reserved DB instance.
    reservedDBInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The duration of the reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the reserved DB instance.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The description of the reserved DB instance.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the lease associated with the reserved DB
    -- instance.
    --
    -- Amazon Web Services Support might request the lease ID for an issue
    -- related to a reserved DB instance.
    leaseId :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The fixed price charged for this reserved DB instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The hourly price charged for this reserved DB instance.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | Indicates if the reservation applies to Multi-AZ deployments.
    multiAZ :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceClass', 'reservedDBInstance_dbInstanceClass' - The DB instance class for the reserved DB instance.
--
-- 'dbInstanceCount', 'reservedDBInstance_dbInstanceCount' - The number of reserved DB instances.
--
-- 'reservedDBInstanceId', 'reservedDBInstance_reservedDBInstanceId' - The unique identifier for the reservation.
--
-- 'recurringCharges', 'reservedDBInstance_recurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- 'state', 'reservedDBInstance_state' - The state of the reserved DB instance.
--
-- 'offeringType', 'reservedDBInstance_offeringType' - The offering type of this reserved DB instance.
--
-- 'reservedDBInstanceArn', 'reservedDBInstance_reservedDBInstanceArn' - The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- 'duration', 'reservedDBInstance_duration' - The duration of the reservation in seconds.
--
-- 'currencyCode', 'reservedDBInstance_currencyCode' - The currency code for the reserved DB instance.
--
-- 'productDescription', 'reservedDBInstance_productDescription' - The description of the reserved DB instance.
--
-- 'leaseId', 'reservedDBInstance_leaseId' - The unique identifier for the lease associated with the reserved DB
-- instance.
--
-- Amazon Web Services Support might request the lease ID for an issue
-- related to a reserved DB instance.
--
-- 'reservedDBInstancesOfferingId', 'reservedDBInstance_reservedDBInstancesOfferingId' - The offering identifier.
--
-- 'fixedPrice', 'reservedDBInstance_fixedPrice' - The fixed price charged for this reserved DB instance.
--
-- 'startTime', 'reservedDBInstance_startTime' - The time the reservation started.
--
-- 'usagePrice', 'reservedDBInstance_usagePrice' - The hourly price charged for this reserved DB instance.
--
-- 'multiAZ', 'reservedDBInstance_multiAZ' - Indicates if the reservation applies to Multi-AZ deployments.
newReservedDBInstance ::
  ReservedDBInstance
newReservedDBInstance =
  ReservedDBInstance'
    { dbInstanceClass =
        Prelude.Nothing,
      dbInstanceCount = Prelude.Nothing,
      reservedDBInstanceId = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      state = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      reservedDBInstanceArn = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      leaseId = Prelude.Nothing,
      reservedDBInstancesOfferingId = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      startTime = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      multiAZ = Prelude.Nothing
    }

-- | The DB instance class for the reserved DB instance.
reservedDBInstance_dbInstanceClass :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_dbInstanceClass = Lens.lens (\ReservedDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ReservedDBInstance' {} a -> s {dbInstanceClass = a} :: ReservedDBInstance)

-- | The number of reserved DB instances.
reservedDBInstance_dbInstanceCount :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Int)
reservedDBInstance_dbInstanceCount = Lens.lens (\ReservedDBInstance' {dbInstanceCount} -> dbInstanceCount) (\s@ReservedDBInstance' {} a -> s {dbInstanceCount = a} :: ReservedDBInstance)

-- | The unique identifier for the reservation.
reservedDBInstance_reservedDBInstanceId :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_reservedDBInstanceId = Lens.lens (\ReservedDBInstance' {reservedDBInstanceId} -> reservedDBInstanceId) (\s@ReservedDBInstance' {} a -> s {reservedDBInstanceId = a} :: ReservedDBInstance)

-- | The recurring price charged to run this reserved DB instance.
reservedDBInstance_recurringCharges :: Lens.Lens' ReservedDBInstance (Prelude.Maybe [RecurringCharge])
reservedDBInstance_recurringCharges = Lens.lens (\ReservedDBInstance' {recurringCharges} -> recurringCharges) (\s@ReservedDBInstance' {} a -> s {recurringCharges = a} :: ReservedDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The state of the reserved DB instance.
reservedDBInstance_state :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_state = Lens.lens (\ReservedDBInstance' {state} -> state) (\s@ReservedDBInstance' {} a -> s {state = a} :: ReservedDBInstance)

-- | The offering type of this reserved DB instance.
reservedDBInstance_offeringType :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_offeringType = Lens.lens (\ReservedDBInstance' {offeringType} -> offeringType) (\s@ReservedDBInstance' {} a -> s {offeringType = a} :: ReservedDBInstance)

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
reservedDBInstance_reservedDBInstanceArn :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_reservedDBInstanceArn = Lens.lens (\ReservedDBInstance' {reservedDBInstanceArn} -> reservedDBInstanceArn) (\s@ReservedDBInstance' {} a -> s {reservedDBInstanceArn = a} :: ReservedDBInstance)

-- | The duration of the reservation in seconds.
reservedDBInstance_duration :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Int)
reservedDBInstance_duration = Lens.lens (\ReservedDBInstance' {duration} -> duration) (\s@ReservedDBInstance' {} a -> s {duration = a} :: ReservedDBInstance)

-- | The currency code for the reserved DB instance.
reservedDBInstance_currencyCode :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_currencyCode = Lens.lens (\ReservedDBInstance' {currencyCode} -> currencyCode) (\s@ReservedDBInstance' {} a -> s {currencyCode = a} :: ReservedDBInstance)

-- | The description of the reserved DB instance.
reservedDBInstance_productDescription :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_productDescription = Lens.lens (\ReservedDBInstance' {productDescription} -> productDescription) (\s@ReservedDBInstance' {} a -> s {productDescription = a} :: ReservedDBInstance)

-- | The unique identifier for the lease associated with the reserved DB
-- instance.
--
-- Amazon Web Services Support might request the lease ID for an issue
-- related to a reserved DB instance.
reservedDBInstance_leaseId :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_leaseId = Lens.lens (\ReservedDBInstance' {leaseId} -> leaseId) (\s@ReservedDBInstance' {} a -> s {leaseId = a} :: ReservedDBInstance)

-- | The offering identifier.
reservedDBInstance_reservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Text)
reservedDBInstance_reservedDBInstancesOfferingId = Lens.lens (\ReservedDBInstance' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@ReservedDBInstance' {} a -> s {reservedDBInstancesOfferingId = a} :: ReservedDBInstance)

-- | The fixed price charged for this reserved DB instance.
reservedDBInstance_fixedPrice :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Double)
reservedDBInstance_fixedPrice = Lens.lens (\ReservedDBInstance' {fixedPrice} -> fixedPrice) (\s@ReservedDBInstance' {} a -> s {fixedPrice = a} :: ReservedDBInstance)

-- | The time the reservation started.
reservedDBInstance_startTime :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.UTCTime)
reservedDBInstance_startTime = Lens.lens (\ReservedDBInstance' {startTime} -> startTime) (\s@ReservedDBInstance' {} a -> s {startTime = a} :: ReservedDBInstance) Prelude.. Lens.mapping Core._Time

-- | The hourly price charged for this reserved DB instance.
reservedDBInstance_usagePrice :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Double)
reservedDBInstance_usagePrice = Lens.lens (\ReservedDBInstance' {usagePrice} -> usagePrice) (\s@ReservedDBInstance' {} a -> s {usagePrice = a} :: ReservedDBInstance)

-- | Indicates if the reservation applies to Multi-AZ deployments.
reservedDBInstance_multiAZ :: Lens.Lens' ReservedDBInstance (Prelude.Maybe Prelude.Bool)
reservedDBInstance_multiAZ = Lens.lens (\ReservedDBInstance' {multiAZ} -> multiAZ) (\s@ReservedDBInstance' {} a -> s {multiAZ = a} :: ReservedDBInstance)

instance Core.FromXML ReservedDBInstance where
  parseXML x =
    ReservedDBInstance'
      Prelude.<$> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "DBInstanceCount")
      Prelude.<*> (x Core..@? "ReservedDBInstanceId")
      Prelude.<*> ( x Core..@? "RecurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "RecurringCharge")
                  )
      Prelude.<*> (x Core..@? "State")
      Prelude.<*> (x Core..@? "OfferingType")
      Prelude.<*> (x Core..@? "ReservedDBInstanceArn")
      Prelude.<*> (x Core..@? "Duration")
      Prelude.<*> (x Core..@? "CurrencyCode")
      Prelude.<*> (x Core..@? "ProductDescription")
      Prelude.<*> (x Core..@? "LeaseId")
      Prelude.<*> (x Core..@? "ReservedDBInstancesOfferingId")
      Prelude.<*> (x Core..@? "FixedPrice")
      Prelude.<*> (x Core..@? "StartTime")
      Prelude.<*> (x Core..@? "UsagePrice")
      Prelude.<*> (x Core..@? "MultiAZ")

instance Prelude.Hashable ReservedDBInstance where
  hashWithSalt _salt ReservedDBInstance' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceCount
      `Prelude.hashWithSalt` reservedDBInstanceId
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` reservedDBInstanceArn
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` leaseId
      `Prelude.hashWithSalt` reservedDBInstancesOfferingId
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` usagePrice
      `Prelude.hashWithSalt` multiAZ

instance Prelude.NFData ReservedDBInstance where
  rnf ReservedDBInstance' {..} =
    Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbInstanceCount
      `Prelude.seq` Prelude.rnf reservedDBInstanceId
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf reservedDBInstanceArn
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf leaseId
      `Prelude.seq` Prelude.rnf reservedDBInstancesOfferingId
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf multiAZ

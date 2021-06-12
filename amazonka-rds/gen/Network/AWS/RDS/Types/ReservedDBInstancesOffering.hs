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
-- Module      : Network.AWS.RDS.Types.ReservedDBInstancesOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReservedDBInstancesOffering where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the
-- @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'newReservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { -- | The duration of the offering in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The currency code for the reserved DB instance offering.
    currencyCode :: Core.Maybe Core.Text,
    -- | Indicates if the offering applies to Multi-AZ deployments.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The DB instance class for the reserved DB instance.
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | The fixed price charged for this offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The hourly price charged for this offering.
    usagePrice :: Core.Maybe Core.Double,
    -- | The offering type.
    offeringType :: Core.Maybe Core.Text,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Core.Maybe [RecurringCharge],
    -- | The database engine used by the offering.
    productDescription :: Core.Maybe Core.Text,
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedDBInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'reservedDBInstancesOffering_duration' - The duration of the offering in seconds.
--
-- 'currencyCode', 'reservedDBInstancesOffering_currencyCode' - The currency code for the reserved DB instance offering.
--
-- 'multiAZ', 'reservedDBInstancesOffering_multiAZ' - Indicates if the offering applies to Multi-AZ deployments.
--
-- 'dbInstanceClass', 'reservedDBInstancesOffering_dbInstanceClass' - The DB instance class for the reserved DB instance.
--
-- 'fixedPrice', 'reservedDBInstancesOffering_fixedPrice' - The fixed price charged for this offering.
--
-- 'usagePrice', 'reservedDBInstancesOffering_usagePrice' - The hourly price charged for this offering.
--
-- 'offeringType', 'reservedDBInstancesOffering_offeringType' - The offering type.
--
-- 'recurringCharges', 'reservedDBInstancesOffering_recurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- 'productDescription', 'reservedDBInstancesOffering_productDescription' - The database engine used by the offering.
--
-- 'reservedDBInstancesOfferingId', 'reservedDBInstancesOffering_reservedDBInstancesOfferingId' - The offering identifier.
newReservedDBInstancesOffering ::
  ReservedDBInstancesOffering
newReservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { duration =
        Core.Nothing,
      currencyCode = Core.Nothing,
      multiAZ = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      recurringCharges = Core.Nothing,
      productDescription = Core.Nothing,
      reservedDBInstancesOfferingId = Core.Nothing
    }

-- | The duration of the offering in seconds.
reservedDBInstancesOffering_duration :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Int)
reservedDBInstancesOffering_duration = Lens.lens (\ReservedDBInstancesOffering' {duration} -> duration) (\s@ReservedDBInstancesOffering' {} a -> s {duration = a} :: ReservedDBInstancesOffering)

-- | The currency code for the reserved DB instance offering.
reservedDBInstancesOffering_currencyCode :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
reservedDBInstancesOffering_currencyCode = Lens.lens (\ReservedDBInstancesOffering' {currencyCode} -> currencyCode) (\s@ReservedDBInstancesOffering' {} a -> s {currencyCode = a} :: ReservedDBInstancesOffering)

-- | Indicates if the offering applies to Multi-AZ deployments.
reservedDBInstancesOffering_multiAZ :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Bool)
reservedDBInstancesOffering_multiAZ = Lens.lens (\ReservedDBInstancesOffering' {multiAZ} -> multiAZ) (\s@ReservedDBInstancesOffering' {} a -> s {multiAZ = a} :: ReservedDBInstancesOffering)

-- | The DB instance class for the reserved DB instance.
reservedDBInstancesOffering_dbInstanceClass :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
reservedDBInstancesOffering_dbInstanceClass = Lens.lens (\ReservedDBInstancesOffering' {dbInstanceClass} -> dbInstanceClass) (\s@ReservedDBInstancesOffering' {} a -> s {dbInstanceClass = a} :: ReservedDBInstancesOffering)

-- | The fixed price charged for this offering.
reservedDBInstancesOffering_fixedPrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
reservedDBInstancesOffering_fixedPrice = Lens.lens (\ReservedDBInstancesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedDBInstancesOffering' {} a -> s {fixedPrice = a} :: ReservedDBInstancesOffering)

-- | The hourly price charged for this offering.
reservedDBInstancesOffering_usagePrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
reservedDBInstancesOffering_usagePrice = Lens.lens (\ReservedDBInstancesOffering' {usagePrice} -> usagePrice) (\s@ReservedDBInstancesOffering' {} a -> s {usagePrice = a} :: ReservedDBInstancesOffering)

-- | The offering type.
reservedDBInstancesOffering_offeringType :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
reservedDBInstancesOffering_offeringType = Lens.lens (\ReservedDBInstancesOffering' {offeringType} -> offeringType) (\s@ReservedDBInstancesOffering' {} a -> s {offeringType = a} :: ReservedDBInstancesOffering)

-- | The recurring price charged to run this reserved DB instance.
reservedDBInstancesOffering_recurringCharges :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe [RecurringCharge])
reservedDBInstancesOffering_recurringCharges = Lens.lens (\ReservedDBInstancesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedDBInstancesOffering' {} a -> s {recurringCharges = a} :: ReservedDBInstancesOffering) Core.. Lens.mapping Lens._Coerce

-- | The database engine used by the offering.
reservedDBInstancesOffering_productDescription :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
reservedDBInstancesOffering_productDescription = Lens.lens (\ReservedDBInstancesOffering' {productDescription} -> productDescription) (\s@ReservedDBInstancesOffering' {} a -> s {productDescription = a} :: ReservedDBInstancesOffering)

-- | The offering identifier.
reservedDBInstancesOffering_reservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
reservedDBInstancesOffering_reservedDBInstancesOfferingId = Lens.lens (\ReservedDBInstancesOffering' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@ReservedDBInstancesOffering' {} a -> s {reservedDBInstancesOfferingId = a} :: ReservedDBInstancesOffering)

instance Core.FromXML ReservedDBInstancesOffering where
  parseXML x =
    ReservedDBInstancesOffering'
      Core.<$> (x Core..@? "Duration")
      Core.<*> (x Core..@? "CurrencyCode")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "UsagePrice")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> ( x Core..@? "RecurringCharges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "RecurringCharge")
               )
      Core.<*> (x Core..@? "ProductDescription")
      Core.<*> (x Core..@? "ReservedDBInstancesOfferingId")

instance Core.Hashable ReservedDBInstancesOffering

instance Core.NFData ReservedDBInstancesOffering

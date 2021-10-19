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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the
-- @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'newReservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { -- | The currency code for the reserved DB instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The database engine used by the offering.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The DB instance class for the reserved DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the offering applies to Multi-AZ deployments.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The offering type.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The hourly price charged for this offering.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The fixed price charged for this offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The duration of the offering in seconds.
    duration :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedDBInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'reservedDBInstancesOffering_currencyCode' - The currency code for the reserved DB instance offering.
--
-- 'productDescription', 'reservedDBInstancesOffering_productDescription' - The database engine used by the offering.
--
-- 'dbInstanceClass', 'reservedDBInstancesOffering_dbInstanceClass' - The DB instance class for the reserved DB instance.
--
-- 'multiAZ', 'reservedDBInstancesOffering_multiAZ' - Indicates if the offering applies to Multi-AZ deployments.
--
-- 'reservedDBInstancesOfferingId', 'reservedDBInstancesOffering_reservedDBInstancesOfferingId' - The offering identifier.
--
-- 'recurringCharges', 'reservedDBInstancesOffering_recurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- 'offeringType', 'reservedDBInstancesOffering_offeringType' - The offering type.
--
-- 'usagePrice', 'reservedDBInstancesOffering_usagePrice' - The hourly price charged for this offering.
--
-- 'fixedPrice', 'reservedDBInstancesOffering_fixedPrice' - The fixed price charged for this offering.
--
-- 'duration', 'reservedDBInstancesOffering_duration' - The duration of the offering in seconds.
newReservedDBInstancesOffering ::
  ReservedDBInstancesOffering
newReservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { currencyCode =
        Prelude.Nothing,
      productDescription = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      reservedDBInstancesOfferingId =
        Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- | The currency code for the reserved DB instance offering.
reservedDBInstancesOffering_currencyCode :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
reservedDBInstancesOffering_currencyCode = Lens.lens (\ReservedDBInstancesOffering' {currencyCode} -> currencyCode) (\s@ReservedDBInstancesOffering' {} a -> s {currencyCode = a} :: ReservedDBInstancesOffering)

-- | The database engine used by the offering.
reservedDBInstancesOffering_productDescription :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
reservedDBInstancesOffering_productDescription = Lens.lens (\ReservedDBInstancesOffering' {productDescription} -> productDescription) (\s@ReservedDBInstancesOffering' {} a -> s {productDescription = a} :: ReservedDBInstancesOffering)

-- | The DB instance class for the reserved DB instance.
reservedDBInstancesOffering_dbInstanceClass :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
reservedDBInstancesOffering_dbInstanceClass = Lens.lens (\ReservedDBInstancesOffering' {dbInstanceClass} -> dbInstanceClass) (\s@ReservedDBInstancesOffering' {} a -> s {dbInstanceClass = a} :: ReservedDBInstancesOffering)

-- | Indicates if the offering applies to Multi-AZ deployments.
reservedDBInstancesOffering_multiAZ :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Bool)
reservedDBInstancesOffering_multiAZ = Lens.lens (\ReservedDBInstancesOffering' {multiAZ} -> multiAZ) (\s@ReservedDBInstancesOffering' {} a -> s {multiAZ = a} :: ReservedDBInstancesOffering)

-- | The offering identifier.
reservedDBInstancesOffering_reservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
reservedDBInstancesOffering_reservedDBInstancesOfferingId = Lens.lens (\ReservedDBInstancesOffering' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@ReservedDBInstancesOffering' {} a -> s {reservedDBInstancesOfferingId = a} :: ReservedDBInstancesOffering)

-- | The recurring price charged to run this reserved DB instance.
reservedDBInstancesOffering_recurringCharges :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe [RecurringCharge])
reservedDBInstancesOffering_recurringCharges = Lens.lens (\ReservedDBInstancesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedDBInstancesOffering' {} a -> s {recurringCharges = a} :: ReservedDBInstancesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The offering type.
reservedDBInstancesOffering_offeringType :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
reservedDBInstancesOffering_offeringType = Lens.lens (\ReservedDBInstancesOffering' {offeringType} -> offeringType) (\s@ReservedDBInstancesOffering' {} a -> s {offeringType = a} :: ReservedDBInstancesOffering)

-- | The hourly price charged for this offering.
reservedDBInstancesOffering_usagePrice :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Double)
reservedDBInstancesOffering_usagePrice = Lens.lens (\ReservedDBInstancesOffering' {usagePrice} -> usagePrice) (\s@ReservedDBInstancesOffering' {} a -> s {usagePrice = a} :: ReservedDBInstancesOffering)

-- | The fixed price charged for this offering.
reservedDBInstancesOffering_fixedPrice :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Double)
reservedDBInstancesOffering_fixedPrice = Lens.lens (\ReservedDBInstancesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedDBInstancesOffering' {} a -> s {fixedPrice = a} :: ReservedDBInstancesOffering)

-- | The duration of the offering in seconds.
reservedDBInstancesOffering_duration :: Lens.Lens' ReservedDBInstancesOffering (Prelude.Maybe Prelude.Int)
reservedDBInstancesOffering_duration = Lens.lens (\ReservedDBInstancesOffering' {duration} -> duration) (\s@ReservedDBInstancesOffering' {} a -> s {duration = a} :: ReservedDBInstancesOffering)

instance Core.FromXML ReservedDBInstancesOffering where
  parseXML x =
    ReservedDBInstancesOffering'
      Prelude.<$> (x Core..@? "CurrencyCode")
      Prelude.<*> (x Core..@? "ProductDescription")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "ReservedDBInstancesOfferingId")
      Prelude.<*> ( x Core..@? "RecurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "RecurringCharge")
                  )
      Prelude.<*> (x Core..@? "OfferingType")
      Prelude.<*> (x Core..@? "UsagePrice")
      Prelude.<*> (x Core..@? "FixedPrice")
      Prelude.<*> (x Core..@? "Duration")

instance Prelude.Hashable ReservedDBInstancesOffering

instance Prelude.NFData ReservedDBInstancesOffering

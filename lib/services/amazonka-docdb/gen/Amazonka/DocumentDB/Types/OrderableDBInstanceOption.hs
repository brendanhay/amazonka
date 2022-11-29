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
-- Module      : Amazonka.DocumentDB.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.OrderableDBInstanceOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocumentDB.Types.AvailabilityZone
import qualified Amazonka.Prelude as Prelude

-- | The options that are available for an instance.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | The instance class for an instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an instance is in a virtual private cloud (VPC).
    vpc :: Prelude.Maybe Prelude.Bool,
    -- | A list of Availability Zones for an instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The engine type of an instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The engine version of an instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The license model for an instance.
    licenseModel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderableDBInstanceOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The instance class for an instance.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether an instance is in a virtual private cloud (VPC).
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for an instance.
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of an instance.
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of an instance.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for an instance.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { dbInstanceClass =
        Prelude.Nothing,
      vpc = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      licenseModel = Prelude.Nothing
    }

-- | The instance class for an instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | Indicates whether an instance is in a virtual private cloud (VPC).
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for an instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | The engine type of an instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | The engine version of an instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | The license model for an instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "Vpc")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "LicenseModel")

instance Prelude.Hashable OrderableDBInstanceOption where
  hashWithSalt _salt OrderableDBInstanceOption' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` licenseModel

instance Prelude.NFData OrderableDBInstanceOption where
  rnf OrderableDBInstanceOption' {..} =
    Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf licenseModel

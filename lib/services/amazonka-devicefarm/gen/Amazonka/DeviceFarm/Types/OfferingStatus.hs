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
-- Module      : Amazonka.DeviceFarm.Types.OfferingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.OfferingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.Offering
import Amazonka.DeviceFarm.Types.OfferingTransactionType
import qualified Amazonka.Prelude as Prelude

-- | The status of the offering.
--
-- /See:/ 'newOfferingStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { -- | The date on which the offering is effective.
    effectiveOn :: Prelude.Maybe Data.POSIX,
    -- | The number of available devices in the offering.
    quantity :: Prelude.Maybe Prelude.Int,
    -- | The type specified for the offering status.
    type' :: Prelude.Maybe OfferingTransactionType,
    -- | Represents the metadata of an offering status.
    offering :: Prelude.Maybe Offering
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OfferingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveOn', 'offeringStatus_effectiveOn' - The date on which the offering is effective.
--
-- 'quantity', 'offeringStatus_quantity' - The number of available devices in the offering.
--
-- 'type'', 'offeringStatus_type' - The type specified for the offering status.
--
-- 'offering', 'offeringStatus_offering' - Represents the metadata of an offering status.
newOfferingStatus ::
  OfferingStatus
newOfferingStatus =
  OfferingStatus'
    { effectiveOn = Prelude.Nothing,
      quantity = Prelude.Nothing,
      type' = Prelude.Nothing,
      offering = Prelude.Nothing
    }

-- | The date on which the offering is effective.
offeringStatus_effectiveOn :: Lens.Lens' OfferingStatus (Prelude.Maybe Prelude.UTCTime)
offeringStatus_effectiveOn = Lens.lens (\OfferingStatus' {effectiveOn} -> effectiveOn) (\s@OfferingStatus' {} a -> s {effectiveOn = a} :: OfferingStatus) Prelude.. Lens.mapping Data._Time

-- | The number of available devices in the offering.
offeringStatus_quantity :: Lens.Lens' OfferingStatus (Prelude.Maybe Prelude.Int)
offeringStatus_quantity = Lens.lens (\OfferingStatus' {quantity} -> quantity) (\s@OfferingStatus' {} a -> s {quantity = a} :: OfferingStatus)

-- | The type specified for the offering status.
offeringStatus_type :: Lens.Lens' OfferingStatus (Prelude.Maybe OfferingTransactionType)
offeringStatus_type = Lens.lens (\OfferingStatus' {type'} -> type') (\s@OfferingStatus' {} a -> s {type' = a} :: OfferingStatus)

-- | Represents the metadata of an offering status.
offeringStatus_offering :: Lens.Lens' OfferingStatus (Prelude.Maybe Offering)
offeringStatus_offering = Lens.lens (\OfferingStatus' {offering} -> offering) (\s@OfferingStatus' {} a -> s {offering = a} :: OfferingStatus)

instance Data.FromJSON OfferingStatus where
  parseJSON =
    Data.withObject
      "OfferingStatus"
      ( \x ->
          OfferingStatus'
            Prelude.<$> (x Data..:? "effectiveOn")
            Prelude.<*> (x Data..:? "quantity")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "offering")
      )

instance Prelude.Hashable OfferingStatus where
  hashWithSalt _salt OfferingStatus' {..} =
    _salt `Prelude.hashWithSalt` effectiveOn
      `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` offering

instance Prelude.NFData OfferingStatus where
  rnf OfferingStatus' {..} =
    Prelude.rnf effectiveOn
      `Prelude.seq` Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf offering

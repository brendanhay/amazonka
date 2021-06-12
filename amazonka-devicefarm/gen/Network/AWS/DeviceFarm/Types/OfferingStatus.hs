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
-- Module      : Network.AWS.DeviceFarm.Types.OfferingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
import qualified Network.AWS.Lens as Lens

-- | The status of the offering.
--
-- /See:/ 'newOfferingStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { -- | The number of available devices in the offering.
    quantity :: Core.Maybe Core.Int,
    -- | Represents the metadata of an offering status.
    offering :: Core.Maybe Offering,
    -- | The date on which the offering is effective.
    effectiveOn :: Core.Maybe Core.POSIX,
    -- | The type specified for the offering status.
    type' :: Core.Maybe OfferingTransactionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OfferingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'offeringStatus_quantity' - The number of available devices in the offering.
--
-- 'offering', 'offeringStatus_offering' - Represents the metadata of an offering status.
--
-- 'effectiveOn', 'offeringStatus_effectiveOn' - The date on which the offering is effective.
--
-- 'type'', 'offeringStatus_type' - The type specified for the offering status.
newOfferingStatus ::
  OfferingStatus
newOfferingStatus =
  OfferingStatus'
    { quantity = Core.Nothing,
      offering = Core.Nothing,
      effectiveOn = Core.Nothing,
      type' = Core.Nothing
    }

-- | The number of available devices in the offering.
offeringStatus_quantity :: Lens.Lens' OfferingStatus (Core.Maybe Core.Int)
offeringStatus_quantity = Lens.lens (\OfferingStatus' {quantity} -> quantity) (\s@OfferingStatus' {} a -> s {quantity = a} :: OfferingStatus)

-- | Represents the metadata of an offering status.
offeringStatus_offering :: Lens.Lens' OfferingStatus (Core.Maybe Offering)
offeringStatus_offering = Lens.lens (\OfferingStatus' {offering} -> offering) (\s@OfferingStatus' {} a -> s {offering = a} :: OfferingStatus)

-- | The date on which the offering is effective.
offeringStatus_effectiveOn :: Lens.Lens' OfferingStatus (Core.Maybe Core.UTCTime)
offeringStatus_effectiveOn = Lens.lens (\OfferingStatus' {effectiveOn} -> effectiveOn) (\s@OfferingStatus' {} a -> s {effectiveOn = a} :: OfferingStatus) Core.. Lens.mapping Core._Time

-- | The type specified for the offering status.
offeringStatus_type :: Lens.Lens' OfferingStatus (Core.Maybe OfferingTransactionType)
offeringStatus_type = Lens.lens (\OfferingStatus' {type'} -> type') (\s@OfferingStatus' {} a -> s {type' = a} :: OfferingStatus)

instance Core.FromJSON OfferingStatus where
  parseJSON =
    Core.withObject
      "OfferingStatus"
      ( \x ->
          OfferingStatus'
            Core.<$> (x Core..:? "quantity")
            Core.<*> (x Core..:? "offering")
            Core.<*> (x Core..:? "effectiveOn")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable OfferingStatus

instance Core.NFData OfferingStatus

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
-- Module      : Network.AWS.DeviceFarm.Types.Offering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Offering where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.OfferingType
import Network.AWS.DeviceFarm.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens

-- | Represents the metadata of a device offering.
--
-- /See:/ 'newOffering' smart constructor.
data Offering = Offering'
  { -- | The platform of the device (for example, @ANDROID@ or @IOS@).
    platform :: Core.Maybe DevicePlatform,
    -- | The ID that corresponds to a device offering.
    id :: Core.Maybe Core.Text,
    -- | A string that describes the offering.
    description :: Core.Maybe Core.Text,
    -- | Specifies whether there are recurring charges for the offering.
    recurringCharges :: Core.Maybe [RecurringCharge],
    -- | The type of offering (for example, @RECURRING@) for a device.
    type' :: Core.Maybe OfferingType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Offering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'offering_platform' - The platform of the device (for example, @ANDROID@ or @IOS@).
--
-- 'id', 'offering_id' - The ID that corresponds to a device offering.
--
-- 'description', 'offering_description' - A string that describes the offering.
--
-- 'recurringCharges', 'offering_recurringCharges' - Specifies whether there are recurring charges for the offering.
--
-- 'type'', 'offering_type' - The type of offering (for example, @RECURRING@) for a device.
newOffering ::
  Offering
newOffering =
  Offering'
    { platform = Core.Nothing,
      id = Core.Nothing,
      description = Core.Nothing,
      recurringCharges = Core.Nothing,
      type' = Core.Nothing
    }

-- | The platform of the device (for example, @ANDROID@ or @IOS@).
offering_platform :: Lens.Lens' Offering (Core.Maybe DevicePlatform)
offering_platform = Lens.lens (\Offering' {platform} -> platform) (\s@Offering' {} a -> s {platform = a} :: Offering)

-- | The ID that corresponds to a device offering.
offering_id :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_id = Lens.lens (\Offering' {id} -> id) (\s@Offering' {} a -> s {id = a} :: Offering)

-- | A string that describes the offering.
offering_description :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_description = Lens.lens (\Offering' {description} -> description) (\s@Offering' {} a -> s {description = a} :: Offering)

-- | Specifies whether there are recurring charges for the offering.
offering_recurringCharges :: Lens.Lens' Offering (Core.Maybe [RecurringCharge])
offering_recurringCharges = Lens.lens (\Offering' {recurringCharges} -> recurringCharges) (\s@Offering' {} a -> s {recurringCharges = a} :: Offering) Core.. Lens.mapping Lens._Coerce

-- | The type of offering (for example, @RECURRING@) for a device.
offering_type :: Lens.Lens' Offering (Core.Maybe OfferingType)
offering_type = Lens.lens (\Offering' {type'} -> type') (\s@Offering' {} a -> s {type' = a} :: Offering)

instance Core.FromJSON Offering where
  parseJSON =
    Core.withObject
      "Offering"
      ( \x ->
          Offering'
            Core.<$> (x Core..:? "platform")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "recurringCharges" Core..!= Core.mempty)
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable Offering

instance Core.NFData Offering

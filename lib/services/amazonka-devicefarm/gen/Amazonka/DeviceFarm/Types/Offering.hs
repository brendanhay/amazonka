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
-- Module      : Amazonka.DeviceFarm.Types.Offering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Offering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.DevicePlatform
import Amazonka.DeviceFarm.Types.OfferingType
import Amazonka.DeviceFarm.Types.RecurringCharge
import qualified Amazonka.Prelude as Prelude

-- | Represents the metadata of a device offering.
--
-- /See:/ 'newOffering' smart constructor.
data Offering = Offering'
  { -- | A string that describes the offering.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID that corresponds to a device offering.
    id :: Prelude.Maybe Prelude.Text,
    -- | The platform of the device (for example, @ANDROID@ or @IOS@).
    platform :: Prelude.Maybe DevicePlatform,
    -- | Specifies whether there are recurring charges for the offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The type of offering (for example, @RECURRING@) for a device.
    type' :: Prelude.Maybe OfferingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Offering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'offering_description' - A string that describes the offering.
--
-- 'id', 'offering_id' - The ID that corresponds to a device offering.
--
-- 'platform', 'offering_platform' - The platform of the device (for example, @ANDROID@ or @IOS@).
--
-- 'recurringCharges', 'offering_recurringCharges' - Specifies whether there are recurring charges for the offering.
--
-- 'type'', 'offering_type' - The type of offering (for example, @RECURRING@) for a device.
newOffering ::
  Offering
newOffering =
  Offering'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      platform = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A string that describes the offering.
offering_description :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_description = Lens.lens (\Offering' {description} -> description) (\s@Offering' {} a -> s {description = a} :: Offering)

-- | The ID that corresponds to a device offering.
offering_id :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_id = Lens.lens (\Offering' {id} -> id) (\s@Offering' {} a -> s {id = a} :: Offering)

-- | The platform of the device (for example, @ANDROID@ or @IOS@).
offering_platform :: Lens.Lens' Offering (Prelude.Maybe DevicePlatform)
offering_platform = Lens.lens (\Offering' {platform} -> platform) (\s@Offering' {} a -> s {platform = a} :: Offering)

-- | Specifies whether there are recurring charges for the offering.
offering_recurringCharges :: Lens.Lens' Offering (Prelude.Maybe [RecurringCharge])
offering_recurringCharges = Lens.lens (\Offering' {recurringCharges} -> recurringCharges) (\s@Offering' {} a -> s {recurringCharges = a} :: Offering) Prelude.. Lens.mapping Lens.coerced

-- | The type of offering (for example, @RECURRING@) for a device.
offering_type :: Lens.Lens' Offering (Prelude.Maybe OfferingType)
offering_type = Lens.lens (\Offering' {type'} -> type') (\s@Offering' {} a -> s {type' = a} :: Offering)

instance Data.FromJSON Offering where
  parseJSON =
    Data.withObject
      "Offering"
      ( \x ->
          Offering'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> ( x
                            Data..:? "recurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Offering where
  hashWithSalt _salt Offering' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Offering where
  rnf Offering' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf platform `Prelude.seq`
          Prelude.rnf recurringCharges `Prelude.seq`
            Prelude.rnf type'

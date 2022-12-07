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
-- Module      : Amazonka.IoTFleetWise.Types.UpdateVehicleResponseItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.UpdateVehicleResponseItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the updated vehicle.
--
-- /See:/ 'newUpdateVehicleResponseItem' smart constructor.
data UpdateVehicleResponseItem = UpdateVehicleResponseItem'
  { -- | The Amazon Resource Name (ARN) of the updated vehicle.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the updated vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVehicleResponseItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateVehicleResponseItem_arn' - The Amazon Resource Name (ARN) of the updated vehicle.
--
-- 'vehicleName', 'updateVehicleResponseItem_vehicleName' - The unique ID of the updated vehicle.
newUpdateVehicleResponseItem ::
  UpdateVehicleResponseItem
newUpdateVehicleResponseItem =
  UpdateVehicleResponseItem'
    { arn = Prelude.Nothing,
      vehicleName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the updated vehicle.
updateVehicleResponseItem_arn :: Lens.Lens' UpdateVehicleResponseItem (Prelude.Maybe Prelude.Text)
updateVehicleResponseItem_arn = Lens.lens (\UpdateVehicleResponseItem' {arn} -> arn) (\s@UpdateVehicleResponseItem' {} a -> s {arn = a} :: UpdateVehicleResponseItem)

-- | The unique ID of the updated vehicle.
updateVehicleResponseItem_vehicleName :: Lens.Lens' UpdateVehicleResponseItem (Prelude.Maybe Prelude.Text)
updateVehicleResponseItem_vehicleName = Lens.lens (\UpdateVehicleResponseItem' {vehicleName} -> vehicleName) (\s@UpdateVehicleResponseItem' {} a -> s {vehicleName = a} :: UpdateVehicleResponseItem)

instance Data.FromJSON UpdateVehicleResponseItem where
  parseJSON =
    Data.withObject
      "UpdateVehicleResponseItem"
      ( \x ->
          UpdateVehicleResponseItem'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "vehicleName")
      )

instance Prelude.Hashable UpdateVehicleResponseItem where
  hashWithSalt _salt UpdateVehicleResponseItem' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData UpdateVehicleResponseItem where
  rnf UpdateVehicleResponseItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf vehicleName

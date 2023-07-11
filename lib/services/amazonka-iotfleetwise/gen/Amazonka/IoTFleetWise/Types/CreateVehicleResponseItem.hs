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
-- Module      : Amazonka.IoTFleetWise.Types.CreateVehicleResponseItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CreateVehicleResponseItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a created vehicle.
--
-- /See:/ 'newCreateVehicleResponseItem' smart constructor.
data CreateVehicleResponseItem = CreateVehicleResponseItem'
  { -- | The ARN of the created vehicle.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a created or validated Amazon Web Services IoT thing.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the vehicle to create.
    vehicleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVehicleResponseItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createVehicleResponseItem_arn' - The ARN of the created vehicle.
--
-- 'thingArn', 'createVehicleResponseItem_thingArn' - The ARN of a created or validated Amazon Web Services IoT thing.
--
-- 'vehicleName', 'createVehicleResponseItem_vehicleName' - The unique ID of the vehicle to create.
newCreateVehicleResponseItem ::
  CreateVehicleResponseItem
newCreateVehicleResponseItem =
  CreateVehicleResponseItem'
    { arn = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      vehicleName = Prelude.Nothing
    }

-- | The ARN of the created vehicle.
createVehicleResponseItem_arn :: Lens.Lens' CreateVehicleResponseItem (Prelude.Maybe Prelude.Text)
createVehicleResponseItem_arn = Lens.lens (\CreateVehicleResponseItem' {arn} -> arn) (\s@CreateVehicleResponseItem' {} a -> s {arn = a} :: CreateVehicleResponseItem)

-- | The ARN of a created or validated Amazon Web Services IoT thing.
createVehicleResponseItem_thingArn :: Lens.Lens' CreateVehicleResponseItem (Prelude.Maybe Prelude.Text)
createVehicleResponseItem_thingArn = Lens.lens (\CreateVehicleResponseItem' {thingArn} -> thingArn) (\s@CreateVehicleResponseItem' {} a -> s {thingArn = a} :: CreateVehicleResponseItem)

-- | The unique ID of the vehicle to create.
createVehicleResponseItem_vehicleName :: Lens.Lens' CreateVehicleResponseItem (Prelude.Maybe Prelude.Text)
createVehicleResponseItem_vehicleName = Lens.lens (\CreateVehicleResponseItem' {vehicleName} -> vehicleName) (\s@CreateVehicleResponseItem' {} a -> s {vehicleName = a} :: CreateVehicleResponseItem)

instance Data.FromJSON CreateVehicleResponseItem where
  parseJSON =
    Data.withObject
      "CreateVehicleResponseItem"
      ( \x ->
          CreateVehicleResponseItem'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "thingArn")
            Prelude.<*> (x Data..:? "vehicleName")
      )

instance Prelude.Hashable CreateVehicleResponseItem where
  hashWithSalt _salt CreateVehicleResponseItem' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData CreateVehicleResponseItem where
  rnf CreateVehicleResponseItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf vehicleName

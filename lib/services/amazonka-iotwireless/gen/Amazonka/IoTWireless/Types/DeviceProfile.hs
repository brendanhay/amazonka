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
-- Module      : Amazonka.IoTWireless.Types.DeviceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DeviceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a device profile.
--
-- /See:/ 'newDeviceProfile' smart constructor.
data DeviceProfile = DeviceProfile'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deviceProfile_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'deviceProfile_id' - The ID of the device profile.
--
-- 'name', 'deviceProfile_name' - The name of the resource.
newDeviceProfile ::
  DeviceProfile
newDeviceProfile =
  DeviceProfile'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
deviceProfile_arn :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_arn = Lens.lens (\DeviceProfile' {arn} -> arn) (\s@DeviceProfile' {} a -> s {arn = a} :: DeviceProfile)

-- | The ID of the device profile.
deviceProfile_id :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_id = Lens.lens (\DeviceProfile' {id} -> id) (\s@DeviceProfile' {} a -> s {id = a} :: DeviceProfile)

-- | The name of the resource.
deviceProfile_name :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_name = Lens.lens (\DeviceProfile' {name} -> name) (\s@DeviceProfile' {} a -> s {name = a} :: DeviceProfile)

instance Data.FromJSON DeviceProfile where
  parseJSON =
    Data.withObject
      "DeviceProfile"
      ( \x ->
          DeviceProfile'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable DeviceProfile where
  hashWithSalt _salt DeviceProfile' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeviceProfile where
  rnf DeviceProfile' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name

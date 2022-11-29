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
import qualified Amazonka.Prelude as Prelude

-- | Describes a device profile.
--
-- /See:/ 'newDeviceProfile' smart constructor.
data DeviceProfile = DeviceProfile'
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device profile.
    id :: Prelude.Maybe Prelude.Text
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
-- 'name', 'deviceProfile_name' - The name of the resource.
--
-- 'arn', 'deviceProfile_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'deviceProfile_id' - The ID of the device profile.
newDeviceProfile ::
  DeviceProfile
newDeviceProfile =
  DeviceProfile'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The name of the resource.
deviceProfile_name :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_name = Lens.lens (\DeviceProfile' {name} -> name) (\s@DeviceProfile' {} a -> s {name = a} :: DeviceProfile)

-- | The Amazon Resource Name of the resource.
deviceProfile_arn :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_arn = Lens.lens (\DeviceProfile' {arn} -> arn) (\s@DeviceProfile' {} a -> s {arn = a} :: DeviceProfile)

-- | The ID of the device profile.
deviceProfile_id :: Lens.Lens' DeviceProfile (Prelude.Maybe Prelude.Text)
deviceProfile_id = Lens.lens (\DeviceProfile' {id} -> id) (\s@DeviceProfile' {} a -> s {id = a} :: DeviceProfile)

instance Core.FromJSON DeviceProfile where
  parseJSON =
    Core.withObject
      "DeviceProfile"
      ( \x ->
          DeviceProfile'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable DeviceProfile where
  hashWithSalt _salt DeviceProfile' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeviceProfile where
  rnf DeviceProfile' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id

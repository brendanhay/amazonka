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
-- Module      : Amazonka.DeviceFarm.Types.DevicePoolCompatibilityResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.DevicePoolCompatibilityResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.Device
import Amazonka.DeviceFarm.Types.IncompatibilityMessage
import qualified Amazonka.Prelude as Prelude

-- | Represents a device pool compatibility result.
--
-- /See:/ 'newDevicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { -- | Whether the result was compatible with the device pool.
    compatible :: Prelude.Maybe Prelude.Bool,
    -- | The device (phone or tablet) to return information about.
    device :: Prelude.Maybe Device,
    -- | Information about the compatibility.
    incompatibilityMessages :: Prelude.Maybe [IncompatibilityMessage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevicePoolCompatibilityResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatible', 'devicePoolCompatibilityResult_compatible' - Whether the result was compatible with the device pool.
--
-- 'device', 'devicePoolCompatibilityResult_device' - The device (phone or tablet) to return information about.
--
-- 'incompatibilityMessages', 'devicePoolCompatibilityResult_incompatibilityMessages' - Information about the compatibility.
newDevicePoolCompatibilityResult ::
  DevicePoolCompatibilityResult
newDevicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { compatible =
        Prelude.Nothing,
      device = Prelude.Nothing,
      incompatibilityMessages = Prelude.Nothing
    }

-- | Whether the result was compatible with the device pool.
devicePoolCompatibilityResult_compatible :: Lens.Lens' DevicePoolCompatibilityResult (Prelude.Maybe Prelude.Bool)
devicePoolCompatibilityResult_compatible = Lens.lens (\DevicePoolCompatibilityResult' {compatible} -> compatible) (\s@DevicePoolCompatibilityResult' {} a -> s {compatible = a} :: DevicePoolCompatibilityResult)

-- | The device (phone or tablet) to return information about.
devicePoolCompatibilityResult_device :: Lens.Lens' DevicePoolCompatibilityResult (Prelude.Maybe Device)
devicePoolCompatibilityResult_device = Lens.lens (\DevicePoolCompatibilityResult' {device} -> device) (\s@DevicePoolCompatibilityResult' {} a -> s {device = a} :: DevicePoolCompatibilityResult)

-- | Information about the compatibility.
devicePoolCompatibilityResult_incompatibilityMessages :: Lens.Lens' DevicePoolCompatibilityResult (Prelude.Maybe [IncompatibilityMessage])
devicePoolCompatibilityResult_incompatibilityMessages = Lens.lens (\DevicePoolCompatibilityResult' {incompatibilityMessages} -> incompatibilityMessages) (\s@DevicePoolCompatibilityResult' {} a -> s {incompatibilityMessages = a} :: DevicePoolCompatibilityResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DevicePoolCompatibilityResult where
  parseJSON =
    Data.withObject
      "DevicePoolCompatibilityResult"
      ( \x ->
          DevicePoolCompatibilityResult'
            Prelude.<$> (x Data..:? "compatible")
            Prelude.<*> (x Data..:? "device")
            Prelude.<*> ( x
                            Data..:? "incompatibilityMessages"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DevicePoolCompatibilityResult
  where
  hashWithSalt _salt DevicePoolCompatibilityResult' {..} =
    _salt
      `Prelude.hashWithSalt` compatible
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` incompatibilityMessages

instance Prelude.NFData DevicePoolCompatibilityResult where
  rnf DevicePoolCompatibilityResult' {..} =
    Prelude.rnf compatible `Prelude.seq`
      Prelude.rnf device `Prelude.seq`
        Prelude.rnf incompatibilityMessages

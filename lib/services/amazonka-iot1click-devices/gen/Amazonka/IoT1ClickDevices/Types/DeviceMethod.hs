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
-- Module      : Amazonka.IoT1ClickDevices.Types.DeviceMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickDevices.Types.DeviceMethod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeviceMethod' smart constructor.
data DeviceMethod = DeviceMethod'
  { -- | The name of the method applicable to the deviceType.
    methodName :: Prelude.Maybe Prelude.Text,
    -- | The type of the device, such as \"button\".
    deviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'methodName', 'deviceMethod_methodName' - The name of the method applicable to the deviceType.
--
-- 'deviceType', 'deviceMethod_deviceType' - The type of the device, such as \"button\".
newDeviceMethod ::
  DeviceMethod
newDeviceMethod =
  DeviceMethod'
    { methodName = Prelude.Nothing,
      deviceType = Prelude.Nothing
    }

-- | The name of the method applicable to the deviceType.
deviceMethod_methodName :: Lens.Lens' DeviceMethod (Prelude.Maybe Prelude.Text)
deviceMethod_methodName = Lens.lens (\DeviceMethod' {methodName} -> methodName) (\s@DeviceMethod' {} a -> s {methodName = a} :: DeviceMethod)

-- | The type of the device, such as \"button\".
deviceMethod_deviceType :: Lens.Lens' DeviceMethod (Prelude.Maybe Prelude.Text)
deviceMethod_deviceType = Lens.lens (\DeviceMethod' {deviceType} -> deviceType) (\s@DeviceMethod' {} a -> s {deviceType = a} :: DeviceMethod)

instance Core.FromJSON DeviceMethod where
  parseJSON =
    Core.withObject
      "DeviceMethod"
      ( \x ->
          DeviceMethod'
            Prelude.<$> (x Core..:? "methodName")
            Prelude.<*> (x Core..:? "deviceType")
      )

instance Prelude.Hashable DeviceMethod where
  hashWithSalt _salt DeviceMethod' {..} =
    _salt `Prelude.hashWithSalt` methodName
      `Prelude.hashWithSalt` deviceType

instance Prelude.NFData DeviceMethod where
  rnf DeviceMethod' {..} =
    Prelude.rnf methodName
      `Prelude.seq` Prelude.rnf deviceType

instance Core.ToJSON DeviceMethod where
  toJSON DeviceMethod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("methodName" Core..=) Prelude.<$> methodName,
            ("deviceType" Core..=) Prelude.<$> deviceType
          ]
      )

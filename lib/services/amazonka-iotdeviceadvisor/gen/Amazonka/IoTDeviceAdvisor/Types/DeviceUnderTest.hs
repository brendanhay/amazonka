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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information of a test device. A thing ARN, certificate ARN or device
-- role ARN is required.
--
-- /See:/ 'newDeviceUnderTest' smart constructor.
data DeviceUnderTest = DeviceUnderTest'
  { -- | Lists device\'s certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Lists device\'s role ARN.
    deviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Lists device\'s thing ARN.
    thingArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceUnderTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'deviceUnderTest_certificateArn' - Lists device\'s certificate ARN.
--
-- 'deviceRoleArn', 'deviceUnderTest_deviceRoleArn' - Lists device\'s role ARN.
--
-- 'thingArn', 'deviceUnderTest_thingArn' - Lists device\'s thing ARN.
newDeviceUnderTest ::
  DeviceUnderTest
newDeviceUnderTest =
  DeviceUnderTest'
    { certificateArn = Prelude.Nothing,
      deviceRoleArn = Prelude.Nothing,
      thingArn = Prelude.Nothing
    }

-- | Lists device\'s certificate ARN.
deviceUnderTest_certificateArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_certificateArn = Lens.lens (\DeviceUnderTest' {certificateArn} -> certificateArn) (\s@DeviceUnderTest' {} a -> s {certificateArn = a} :: DeviceUnderTest)

-- | Lists device\'s role ARN.
deviceUnderTest_deviceRoleArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_deviceRoleArn = Lens.lens (\DeviceUnderTest' {deviceRoleArn} -> deviceRoleArn) (\s@DeviceUnderTest' {} a -> s {deviceRoleArn = a} :: DeviceUnderTest)

-- | Lists device\'s thing ARN.
deviceUnderTest_thingArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_thingArn = Lens.lens (\DeviceUnderTest' {thingArn} -> thingArn) (\s@DeviceUnderTest' {} a -> s {thingArn = a} :: DeviceUnderTest)

instance Data.FromJSON DeviceUnderTest where
  parseJSON =
    Data.withObject
      "DeviceUnderTest"
      ( \x ->
          DeviceUnderTest'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "deviceRoleArn")
            Prelude.<*> (x Data..:? "thingArn")
      )

instance Prelude.Hashable DeviceUnderTest where
  hashWithSalt _salt DeviceUnderTest' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` deviceRoleArn
      `Prelude.hashWithSalt` thingArn

instance Prelude.NFData DeviceUnderTest where
  rnf DeviceUnderTest' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf deviceRoleArn
      `Prelude.seq` Prelude.rnf thingArn

instance Data.ToJSON DeviceUnderTest where
  toJSON DeviceUnderTest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("deviceRoleArn" Data..=) Prelude.<$> deviceRoleArn,
            ("thingArn" Data..=) Prelude.<$> thingArn
          ]
      )

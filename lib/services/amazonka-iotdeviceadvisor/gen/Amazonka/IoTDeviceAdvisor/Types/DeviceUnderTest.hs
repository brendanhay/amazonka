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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information of a test device. A thing ARN or a certificate ARN is
-- required.
--
-- /See:/ 'newDeviceUnderTest' smart constructor.
data DeviceUnderTest = DeviceUnderTest'
  { -- | Lists devices thing ARN.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | Lists devices certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text
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
-- 'thingArn', 'deviceUnderTest_thingArn' - Lists devices thing ARN.
--
-- 'certificateArn', 'deviceUnderTest_certificateArn' - Lists devices certificate ARN.
newDeviceUnderTest ::
  DeviceUnderTest
newDeviceUnderTest =
  DeviceUnderTest'
    { thingArn = Prelude.Nothing,
      certificateArn = Prelude.Nothing
    }

-- | Lists devices thing ARN.
deviceUnderTest_thingArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_thingArn = Lens.lens (\DeviceUnderTest' {thingArn} -> thingArn) (\s@DeviceUnderTest' {} a -> s {thingArn = a} :: DeviceUnderTest)

-- | Lists devices certificate ARN.
deviceUnderTest_certificateArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_certificateArn = Lens.lens (\DeviceUnderTest' {certificateArn} -> certificateArn) (\s@DeviceUnderTest' {} a -> s {certificateArn = a} :: DeviceUnderTest)

instance Core.FromJSON DeviceUnderTest where
  parseJSON =
    Core.withObject
      "DeviceUnderTest"
      ( \x ->
          DeviceUnderTest'
            Prelude.<$> (x Core..:? "thingArn")
            Prelude.<*> (x Core..:? "certificateArn")
      )

instance Prelude.Hashable DeviceUnderTest where
  hashWithSalt _salt DeviceUnderTest' {..} =
    _salt `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData DeviceUnderTest where
  rnf DeviceUnderTest' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf certificateArn

instance Core.ToJSON DeviceUnderTest where
  toJSON DeviceUnderTest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("thingArn" Core..=) Prelude.<$> thingArn,
            ("certificateArn" Core..=)
              Prelude.<$> certificateArn
          ]
      )

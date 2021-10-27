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
-- Module      : Network.AWS.IoTDeviceAdvisor.Types.DeviceUnderTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTDeviceAdvisor.Types.DeviceUnderTest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists all the devices under test
--
-- /See:/ 'newDeviceUnderTest' smart constructor.
data DeviceUnderTest = DeviceUnderTest'
  { -- | Lists devices certificate arn
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Lists devices thing arn
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
-- 'certificateArn', 'deviceUnderTest_certificateArn' - Lists devices certificate arn
--
-- 'thingArn', 'deviceUnderTest_thingArn' - Lists devices thing arn
newDeviceUnderTest ::
  DeviceUnderTest
newDeviceUnderTest =
  DeviceUnderTest'
    { certificateArn = Prelude.Nothing,
      thingArn = Prelude.Nothing
    }

-- | Lists devices certificate arn
deviceUnderTest_certificateArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_certificateArn = Lens.lens (\DeviceUnderTest' {certificateArn} -> certificateArn) (\s@DeviceUnderTest' {} a -> s {certificateArn = a} :: DeviceUnderTest)

-- | Lists devices thing arn
deviceUnderTest_thingArn :: Lens.Lens' DeviceUnderTest (Prelude.Maybe Prelude.Text)
deviceUnderTest_thingArn = Lens.lens (\DeviceUnderTest' {thingArn} -> thingArn) (\s@DeviceUnderTest' {} a -> s {thingArn = a} :: DeviceUnderTest)

instance Core.FromJSON DeviceUnderTest where
  parseJSON =
    Core.withObject
      "DeviceUnderTest"
      ( \x ->
          DeviceUnderTest'
            Prelude.<$> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "thingArn")
      )

instance Prelude.Hashable DeviceUnderTest

instance Prelude.NFData DeviceUnderTest

instance Core.ToJSON DeviceUnderTest where
  toJSON DeviceUnderTest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("certificateArn" Core..=)
              Prelude.<$> certificateArn,
            ("thingArn" Core..=) Prelude.<$> thingArn
          ]
      )

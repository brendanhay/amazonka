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
-- Module      : Amazonka.AlexaBusiness.Types.DeviceStatusDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceStatusDetail where

import Amazonka.AlexaBusiness.Types.DeviceStatusDetailCode
import Amazonka.AlexaBusiness.Types.Feature
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of a device’s status.
--
-- /See:/ 'newDeviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { -- | The device status detail code.
    code :: Prelude.Maybe DeviceStatusDetailCode,
    -- | The list of available features on the device.
    feature :: Prelude.Maybe Feature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceStatusDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'deviceStatusDetail_code' - The device status detail code.
--
-- 'feature', 'deviceStatusDetail_feature' - The list of available features on the device.
newDeviceStatusDetail ::
  DeviceStatusDetail
newDeviceStatusDetail =
  DeviceStatusDetail'
    { code = Prelude.Nothing,
      feature = Prelude.Nothing
    }

-- | The device status detail code.
deviceStatusDetail_code :: Lens.Lens' DeviceStatusDetail (Prelude.Maybe DeviceStatusDetailCode)
deviceStatusDetail_code = Lens.lens (\DeviceStatusDetail' {code} -> code) (\s@DeviceStatusDetail' {} a -> s {code = a} :: DeviceStatusDetail)

-- | The list of available features on the device.
deviceStatusDetail_feature :: Lens.Lens' DeviceStatusDetail (Prelude.Maybe Feature)
deviceStatusDetail_feature = Lens.lens (\DeviceStatusDetail' {feature} -> feature) (\s@DeviceStatusDetail' {} a -> s {feature = a} :: DeviceStatusDetail)

instance Data.FromJSON DeviceStatusDetail where
  parseJSON =
    Data.withObject
      "DeviceStatusDetail"
      ( \x ->
          DeviceStatusDetail'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Feature")
      )

instance Prelude.Hashable DeviceStatusDetail where
  hashWithSalt _salt DeviceStatusDetail' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` feature

instance Prelude.NFData DeviceStatusDetail where
  rnf DeviceStatusDetail' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf feature

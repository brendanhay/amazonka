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
-- Module      : Amazonka.Braket.Types.DeviceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.DeviceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the quantum processing units (QPUs) or simulator used to
-- create and run an Amazon Braket job.
--
-- /See:/ 'newDeviceConfig' smart constructor.
data DeviceConfig = DeviceConfig'
  { -- | The primary quantum processing unit (QPU) or simulator used to create
    -- and run an Amazon Braket job.
    device :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'deviceConfig_device' - The primary quantum processing unit (QPU) or simulator used to create
-- and run an Amazon Braket job.
newDeviceConfig ::
  -- | 'device'
  Prelude.Text ->
  DeviceConfig
newDeviceConfig pDevice_ =
  DeviceConfig' {device = pDevice_}

-- | The primary quantum processing unit (QPU) or simulator used to create
-- and run an Amazon Braket job.
deviceConfig_device :: Lens.Lens' DeviceConfig Prelude.Text
deviceConfig_device = Lens.lens (\DeviceConfig' {device} -> device) (\s@DeviceConfig' {} a -> s {device = a} :: DeviceConfig)

instance Data.FromJSON DeviceConfig where
  parseJSON =
    Data.withObject
      "DeviceConfig"
      ( \x ->
          DeviceConfig' Prelude.<$> (x Data..: "device")
      )

instance Prelude.Hashable DeviceConfig where
  hashWithSalt _salt DeviceConfig' {..} =
    _salt `Prelude.hashWithSalt` device

instance Prelude.NFData DeviceConfig where
  rnf DeviceConfig' {..} = Prelude.rnf device

instance Data.ToJSON DeviceConfig where
  toJSON DeviceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("device" Data..= device)]
      )

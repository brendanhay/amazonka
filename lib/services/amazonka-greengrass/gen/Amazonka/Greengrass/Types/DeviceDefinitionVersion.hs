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
-- Module      : Amazonka.Greengrass.Types.DeviceDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.DeviceDefinitionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.Device
import qualified Amazonka.Prelude as Prelude

-- | Information about a device definition version.
--
-- /See:/ 'newDeviceDefinitionVersion' smart constructor.
data DeviceDefinitionVersion = DeviceDefinitionVersion'
  { -- | A list of devices in the definition version.
    devices :: Prelude.Maybe [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'deviceDefinitionVersion_devices' - A list of devices in the definition version.
newDeviceDefinitionVersion ::
  DeviceDefinitionVersion
newDeviceDefinitionVersion =
  DeviceDefinitionVersion' {devices = Prelude.Nothing}

-- | A list of devices in the definition version.
deviceDefinitionVersion_devices :: Lens.Lens' DeviceDefinitionVersion (Prelude.Maybe [Device])
deviceDefinitionVersion_devices = Lens.lens (\DeviceDefinitionVersion' {devices} -> devices) (\s@DeviceDefinitionVersion' {} a -> s {devices = a} :: DeviceDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DeviceDefinitionVersion where
  parseJSON =
    Data.withObject
      "DeviceDefinitionVersion"
      ( \x ->
          DeviceDefinitionVersion'
            Prelude.<$> (x Data..:? "Devices" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DeviceDefinitionVersion where
  hashWithSalt _salt DeviceDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` devices

instance Prelude.NFData DeviceDefinitionVersion where
  rnf DeviceDefinitionVersion' {..} =
    Prelude.rnf devices

instance Data.ToJSON DeviceDefinitionVersion where
  toJSON DeviceDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Devices" Data..=) Prelude.<$> devices]
      )

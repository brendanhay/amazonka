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
-- Module      : Amazonka.IoT1ClickProjects.Types.DeviceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.DeviceTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a device for a placement template (see
-- PlacementTemplate).
--
-- /See:/ 'newDeviceTemplate' smart constructor.
data DeviceTemplate = DeviceTemplate'
  { -- | An optional Lambda function to invoke instead of the default Lambda
    -- function provided by the placement template.
    callbackOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The device type, which currently must be @\"button\"@.
    deviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callbackOverrides', 'deviceTemplate_callbackOverrides' - An optional Lambda function to invoke instead of the default Lambda
-- function provided by the placement template.
--
-- 'deviceType', 'deviceTemplate_deviceType' - The device type, which currently must be @\"button\"@.
newDeviceTemplate ::
  DeviceTemplate
newDeviceTemplate =
  DeviceTemplate'
    { callbackOverrides =
        Prelude.Nothing,
      deviceType = Prelude.Nothing
    }

-- | An optional Lambda function to invoke instead of the default Lambda
-- function provided by the placement template.
deviceTemplate_callbackOverrides :: Lens.Lens' DeviceTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deviceTemplate_callbackOverrides = Lens.lens (\DeviceTemplate' {callbackOverrides} -> callbackOverrides) (\s@DeviceTemplate' {} a -> s {callbackOverrides = a} :: DeviceTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The device type, which currently must be @\"button\"@.
deviceTemplate_deviceType :: Lens.Lens' DeviceTemplate (Prelude.Maybe Prelude.Text)
deviceTemplate_deviceType = Lens.lens (\DeviceTemplate' {deviceType} -> deviceType) (\s@DeviceTemplate' {} a -> s {deviceType = a} :: DeviceTemplate)

instance Data.FromJSON DeviceTemplate where
  parseJSON =
    Data.withObject
      "DeviceTemplate"
      ( \x ->
          DeviceTemplate'
            Prelude.<$> ( x
                            Data..:? "callbackOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "deviceType")
      )

instance Prelude.Hashable DeviceTemplate where
  hashWithSalt _salt DeviceTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` callbackOverrides
      `Prelude.hashWithSalt` deviceType

instance Prelude.NFData DeviceTemplate where
  rnf DeviceTemplate' {..} =
    Prelude.rnf callbackOverrides `Prelude.seq`
      Prelude.rnf deviceType

instance Data.ToJSON DeviceTemplate where
  toJSON DeviceTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("callbackOverrides" Data..=)
              Prelude.<$> callbackOverrides,
            ("deviceType" Data..=) Prelude.<$> deviceType
          ]
      )

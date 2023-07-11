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
-- Module      : Amazonka.Panorama.Types.DeviceJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.OTAJobConfig
import qualified Amazonka.Prelude as Prelude

-- | A job\'s configuration.
--
-- /See:/ 'newDeviceJobConfig' smart constructor.
data DeviceJobConfig = DeviceJobConfig'
  { -- | A configuration for an over-the-air (OTA) upgrade. Required for OTA
    -- jobs.
    oTAJobConfig :: Prelude.Maybe OTAJobConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oTAJobConfig', 'deviceJobConfig_oTAJobConfig' - A configuration for an over-the-air (OTA) upgrade. Required for OTA
-- jobs.
newDeviceJobConfig ::
  DeviceJobConfig
newDeviceJobConfig =
  DeviceJobConfig' {oTAJobConfig = Prelude.Nothing}

-- | A configuration for an over-the-air (OTA) upgrade. Required for OTA
-- jobs.
deviceJobConfig_oTAJobConfig :: Lens.Lens' DeviceJobConfig (Prelude.Maybe OTAJobConfig)
deviceJobConfig_oTAJobConfig = Lens.lens (\DeviceJobConfig' {oTAJobConfig} -> oTAJobConfig) (\s@DeviceJobConfig' {} a -> s {oTAJobConfig = a} :: DeviceJobConfig)

instance Prelude.Hashable DeviceJobConfig where
  hashWithSalt _salt DeviceJobConfig' {..} =
    _salt `Prelude.hashWithSalt` oTAJobConfig

instance Prelude.NFData DeviceJobConfig where
  rnf DeviceJobConfig' {..} = Prelude.rnf oTAJobConfig

instance Data.ToJSON DeviceJobConfig where
  toJSON DeviceJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("OTAJobConfig" Data..=) Prelude.<$> oTAJobConfig]
      )

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
-- Module      : Amazonka.IoTSecureTunneling.Types.DestinationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.DestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The destination configuration.
--
-- /See:/ 'newDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The name of the IoT thing to which you want to connect.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | A list of service names that identify the target application. The IoT
    -- client running on the destination device reads this value and uses it to
    -- look up a port or an IP address and a port. The IoT client instantiates
    -- the local proxy, which uses this information to connect to the
    -- destination application.
    services :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'destinationConfig_thingName' - The name of the IoT thing to which you want to connect.
--
-- 'services', 'destinationConfig_services' - A list of service names that identify the target application. The IoT
-- client running on the destination device reads this value and uses it to
-- look up a port or an IP address and a port. The IoT client instantiates
-- the local proxy, which uses this information to connect to the
-- destination application.
newDestinationConfig ::
  -- | 'services'
  Prelude.NonEmpty Prelude.Text ->
  DestinationConfig
newDestinationConfig pServices_ =
  DestinationConfig'
    { thingName = Prelude.Nothing,
      services = Lens.coerced Lens.# pServices_
    }

-- | The name of the IoT thing to which you want to connect.
destinationConfig_thingName :: Lens.Lens' DestinationConfig (Prelude.Maybe Prelude.Text)
destinationConfig_thingName = Lens.lens (\DestinationConfig' {thingName} -> thingName) (\s@DestinationConfig' {} a -> s {thingName = a} :: DestinationConfig)

-- | A list of service names that identify the target application. The IoT
-- client running on the destination device reads this value and uses it to
-- look up a port or an IP address and a port. The IoT client instantiates
-- the local proxy, which uses this information to connect to the
-- destination application.
destinationConfig_services :: Lens.Lens' DestinationConfig (Prelude.NonEmpty Prelude.Text)
destinationConfig_services = Lens.lens (\DestinationConfig' {services} -> services) (\s@DestinationConfig' {} a -> s {services = a} :: DestinationConfig) Prelude.. Lens.coerced

instance Data.FromJSON DestinationConfig where
  parseJSON =
    Data.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Prelude.<$> (x Data..:? "thingName")
            Prelude.<*> (x Data..: "services")
      )

instance Prelude.Hashable DestinationConfig where
  hashWithSalt _salt DestinationConfig' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` services

instance Prelude.NFData DestinationConfig where
  rnf DestinationConfig' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf services

instance Data.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("thingName" Data..=) Prelude.<$> thingName,
            Prelude.Just ("services" Data..= services)
          ]
      )

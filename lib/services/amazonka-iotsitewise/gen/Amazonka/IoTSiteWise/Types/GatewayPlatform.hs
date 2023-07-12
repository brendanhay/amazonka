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
-- Module      : Amazonka.IoTSiteWise.Types.GatewayPlatform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.GatewayPlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Greengrass
import Amazonka.IoTSiteWise.Types.GreengrassV2
import qualified Amazonka.Prelude as Prelude

-- | Contains a gateway\'s platform information.
--
-- /See:/ 'newGatewayPlatform' smart constructor.
data GatewayPlatform = GatewayPlatform'
  { -- | A gateway that runs on IoT Greengrass.
    greengrass :: Prelude.Maybe Greengrass,
    -- | A gateway that runs on IoT Greengrass V2.
    greengrassV2 :: Prelude.Maybe GreengrassV2
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'greengrass', 'gatewayPlatform_greengrass' - A gateway that runs on IoT Greengrass.
--
-- 'greengrassV2', 'gatewayPlatform_greengrassV2' - A gateway that runs on IoT Greengrass V2.
newGatewayPlatform ::
  GatewayPlatform
newGatewayPlatform =
  GatewayPlatform'
    { greengrass = Prelude.Nothing,
      greengrassV2 = Prelude.Nothing
    }

-- | A gateway that runs on IoT Greengrass.
gatewayPlatform_greengrass :: Lens.Lens' GatewayPlatform (Prelude.Maybe Greengrass)
gatewayPlatform_greengrass = Lens.lens (\GatewayPlatform' {greengrass} -> greengrass) (\s@GatewayPlatform' {} a -> s {greengrass = a} :: GatewayPlatform)

-- | A gateway that runs on IoT Greengrass V2.
gatewayPlatform_greengrassV2 :: Lens.Lens' GatewayPlatform (Prelude.Maybe GreengrassV2)
gatewayPlatform_greengrassV2 = Lens.lens (\GatewayPlatform' {greengrassV2} -> greengrassV2) (\s@GatewayPlatform' {} a -> s {greengrassV2 = a} :: GatewayPlatform)

instance Data.FromJSON GatewayPlatform where
  parseJSON =
    Data.withObject
      "GatewayPlatform"
      ( \x ->
          GatewayPlatform'
            Prelude.<$> (x Data..:? "greengrass")
            Prelude.<*> (x Data..:? "greengrassV2")
      )

instance Prelude.Hashable GatewayPlatform where
  hashWithSalt _salt GatewayPlatform' {..} =
    _salt
      `Prelude.hashWithSalt` greengrass
      `Prelude.hashWithSalt` greengrassV2

instance Prelude.NFData GatewayPlatform where
  rnf GatewayPlatform' {..} =
    Prelude.rnf greengrass
      `Prelude.seq` Prelude.rnf greengrassV2

instance Data.ToJSON GatewayPlatform where
  toJSON GatewayPlatform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("greengrass" Data..=) Prelude.<$> greengrass,
            ("greengrassV2" Data..=) Prelude.<$> greengrassV2
          ]
      )

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
-- Module      : Amazonka.GroundStation.Types.UplinkEchoConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.UplinkEchoConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an uplink echo @Config@.
--
-- Parameters from the @AntennaUplinkConfig@, corresponding to the
-- specified @AntennaUplinkConfigArn@, are used when this
-- @UplinkEchoConfig@ is used in a contact.
--
-- /See:/ 'newUplinkEchoConfig' smart constructor.
data UplinkEchoConfig = UplinkEchoConfig'
  { -- | ARN of an uplink @Config@.
    antennaUplinkConfigArn :: Prelude.Text,
    -- | Whether or not an uplink @Config@ is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UplinkEchoConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'antennaUplinkConfigArn', 'uplinkEchoConfig_antennaUplinkConfigArn' - ARN of an uplink @Config@.
--
-- 'enabled', 'uplinkEchoConfig_enabled' - Whether or not an uplink @Config@ is enabled.
newUplinkEchoConfig ::
  -- | 'antennaUplinkConfigArn'
  Prelude.Text ->
  -- | 'enabled'
  Prelude.Bool ->
  UplinkEchoConfig
newUplinkEchoConfig
  pAntennaUplinkConfigArn_
  pEnabled_ =
    UplinkEchoConfig'
      { antennaUplinkConfigArn =
          pAntennaUplinkConfigArn_,
        enabled = pEnabled_
      }

-- | ARN of an uplink @Config@.
uplinkEchoConfig_antennaUplinkConfigArn :: Lens.Lens' UplinkEchoConfig Prelude.Text
uplinkEchoConfig_antennaUplinkConfigArn = Lens.lens (\UplinkEchoConfig' {antennaUplinkConfigArn} -> antennaUplinkConfigArn) (\s@UplinkEchoConfig' {} a -> s {antennaUplinkConfigArn = a} :: UplinkEchoConfig)

-- | Whether or not an uplink @Config@ is enabled.
uplinkEchoConfig_enabled :: Lens.Lens' UplinkEchoConfig Prelude.Bool
uplinkEchoConfig_enabled = Lens.lens (\UplinkEchoConfig' {enabled} -> enabled) (\s@UplinkEchoConfig' {} a -> s {enabled = a} :: UplinkEchoConfig)

instance Data.FromJSON UplinkEchoConfig where
  parseJSON =
    Data.withObject
      "UplinkEchoConfig"
      ( \x ->
          UplinkEchoConfig'
            Prelude.<$> (x Data..: "antennaUplinkConfigArn")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable UplinkEchoConfig where
  hashWithSalt _salt UplinkEchoConfig' {..} =
    _salt
      `Prelude.hashWithSalt` antennaUplinkConfigArn
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData UplinkEchoConfig where
  rnf UplinkEchoConfig' {..} =
    Prelude.rnf antennaUplinkConfigArn `Prelude.seq`
      Prelude.rnf enabled

instance Data.ToJSON UplinkEchoConfig where
  toJSON UplinkEchoConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "antennaUplinkConfigArn"
                  Data..= antennaUplinkConfigArn
              ),
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )

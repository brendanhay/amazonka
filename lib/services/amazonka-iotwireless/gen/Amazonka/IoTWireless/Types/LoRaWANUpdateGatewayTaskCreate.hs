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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.LoRaWANGatewayVersion
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANUpdateGatewayTaskCreate object.
--
-- /See:/ 'newLoRaWANUpdateGatewayTaskCreate' smart constructor.
data LoRaWANUpdateGatewayTaskCreate = LoRaWANUpdateGatewayTaskCreate'
  { -- | The signature used to verify the update firmware.
    updateSignature :: Prelude.Maybe Prelude.Text,
    -- | The CRC of the signature private key to check.
    sigKeyCrc :: Prelude.Maybe Prelude.Natural,
    -- | The version of the gateways that should receive the update.
    currentVersion :: Prelude.Maybe LoRaWANGatewayVersion,
    -- | The firmware version to update the gateway to.
    updateVersion :: Prelude.Maybe LoRaWANGatewayVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANUpdateGatewayTaskCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateSignature', 'loRaWANUpdateGatewayTaskCreate_updateSignature' - The signature used to verify the update firmware.
--
-- 'sigKeyCrc', 'loRaWANUpdateGatewayTaskCreate_sigKeyCrc' - The CRC of the signature private key to check.
--
-- 'currentVersion', 'loRaWANUpdateGatewayTaskCreate_currentVersion' - The version of the gateways that should receive the update.
--
-- 'updateVersion', 'loRaWANUpdateGatewayTaskCreate_updateVersion' - The firmware version to update the gateway to.
newLoRaWANUpdateGatewayTaskCreate ::
  LoRaWANUpdateGatewayTaskCreate
newLoRaWANUpdateGatewayTaskCreate =
  LoRaWANUpdateGatewayTaskCreate'
    { updateSignature =
        Prelude.Nothing,
      sigKeyCrc = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      updateVersion = Prelude.Nothing
    }

-- | The signature used to verify the update firmware.
loRaWANUpdateGatewayTaskCreate_updateSignature :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe Prelude.Text)
loRaWANUpdateGatewayTaskCreate_updateSignature = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {updateSignature} -> updateSignature) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {updateSignature = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The CRC of the signature private key to check.
loRaWANUpdateGatewayTaskCreate_sigKeyCrc :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe Prelude.Natural)
loRaWANUpdateGatewayTaskCreate_sigKeyCrc = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {sigKeyCrc} -> sigKeyCrc) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {sigKeyCrc = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The version of the gateways that should receive the update.
loRaWANUpdateGatewayTaskCreate_currentVersion :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe LoRaWANGatewayVersion)
loRaWANUpdateGatewayTaskCreate_currentVersion = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {currentVersion} -> currentVersion) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {currentVersion = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The firmware version to update the gateway to.
loRaWANUpdateGatewayTaskCreate_updateVersion :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe LoRaWANGatewayVersion)
loRaWANUpdateGatewayTaskCreate_updateVersion = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {updateVersion} -> updateVersion) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {updateVersion = a} :: LoRaWANUpdateGatewayTaskCreate)

instance Core.FromJSON LoRaWANUpdateGatewayTaskCreate where
  parseJSON =
    Core.withObject
      "LoRaWANUpdateGatewayTaskCreate"
      ( \x ->
          LoRaWANUpdateGatewayTaskCreate'
            Prelude.<$> (x Core..:? "UpdateSignature")
            Prelude.<*> (x Core..:? "SigKeyCrc")
            Prelude.<*> (x Core..:? "CurrentVersion")
            Prelude.<*> (x Core..:? "UpdateVersion")
      )

instance
  Prelude.Hashable
    LoRaWANUpdateGatewayTaskCreate
  where
  hashWithSalt
    salt'
    LoRaWANUpdateGatewayTaskCreate' {..} =
      salt' `Prelude.hashWithSalt` updateVersion
        `Prelude.hashWithSalt` currentVersion
        `Prelude.hashWithSalt` sigKeyCrc
        `Prelude.hashWithSalt` updateSignature

instance
  Prelude.NFData
    LoRaWANUpdateGatewayTaskCreate
  where
  rnf LoRaWANUpdateGatewayTaskCreate' {..} =
    Prelude.rnf updateSignature
      `Prelude.seq` Prelude.rnf updateVersion
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf sigKeyCrc

instance Core.ToJSON LoRaWANUpdateGatewayTaskCreate where
  toJSON LoRaWANUpdateGatewayTaskCreate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UpdateSignature" Core..=)
              Prelude.<$> updateSignature,
            ("SigKeyCrc" Core..=) Prelude.<$> sigKeyCrc,
            ("CurrentVersion" Core..=)
              Prelude.<$> currentVersion,
            ("UpdateVersion" Core..=) Prelude.<$> updateVersion
          ]
      )

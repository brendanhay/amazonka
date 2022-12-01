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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANFuotaTaskGetInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANFuotaTaskGetInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information returned from getting a FUOTA task.
--
-- /See:/ 'newLoRaWANFuotaTaskGetInfo' smart constructor.
data LoRaWANFuotaTaskGetInfo = LoRaWANFuotaTaskGetInfo'
  { rfRegion :: Prelude.Maybe Prelude.Text,
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANFuotaTaskGetInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rfRegion', 'loRaWANFuotaTaskGetInfo_rfRegion' - Undocumented member.
--
-- 'startTime', 'loRaWANFuotaTaskGetInfo_startTime' - Undocumented member.
newLoRaWANFuotaTaskGetInfo ::
  LoRaWANFuotaTaskGetInfo
newLoRaWANFuotaTaskGetInfo =
  LoRaWANFuotaTaskGetInfo'
    { rfRegion =
        Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANFuotaTaskGetInfo_rfRegion :: Lens.Lens' LoRaWANFuotaTaskGetInfo (Prelude.Maybe Prelude.Text)
loRaWANFuotaTaskGetInfo_rfRegion = Lens.lens (\LoRaWANFuotaTaskGetInfo' {rfRegion} -> rfRegion) (\s@LoRaWANFuotaTaskGetInfo' {} a -> s {rfRegion = a} :: LoRaWANFuotaTaskGetInfo)

-- | Undocumented member.
loRaWANFuotaTaskGetInfo_startTime :: Lens.Lens' LoRaWANFuotaTaskGetInfo (Prelude.Maybe Prelude.UTCTime)
loRaWANFuotaTaskGetInfo_startTime = Lens.lens (\LoRaWANFuotaTaskGetInfo' {startTime} -> startTime) (\s@LoRaWANFuotaTaskGetInfo' {} a -> s {startTime = a} :: LoRaWANFuotaTaskGetInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LoRaWANFuotaTaskGetInfo where
  parseJSON =
    Core.withObject
      "LoRaWANFuotaTaskGetInfo"
      ( \x ->
          LoRaWANFuotaTaskGetInfo'
            Prelude.<$> (x Core..:? "RfRegion")
            Prelude.<*> (x Core..:? "StartTime")
      )

instance Prelude.Hashable LoRaWANFuotaTaskGetInfo where
  hashWithSalt _salt LoRaWANFuotaTaskGetInfo' {..} =
    _salt `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData LoRaWANFuotaTaskGetInfo where
  rnf LoRaWANFuotaTaskGetInfo' {..} =
    Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf startTime

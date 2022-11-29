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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANMulticastGet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANMulticastGet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.DlClass
import Amazonka.IoTWireless.Types.SupportedRfRegion
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information that is to be returned from getting multicast
-- group information.
--
-- /See:/ 'newLoRaWANMulticastGet' smart constructor.
data LoRaWANMulticastGet = LoRaWANMulticastGet'
  { rfRegion :: Prelude.Maybe SupportedRfRegion,
    numberOfDevicesInGroup :: Prelude.Maybe Prelude.Int,
    dlClass :: Prelude.Maybe DlClass,
    numberOfDevicesRequested :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANMulticastGet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rfRegion', 'loRaWANMulticastGet_rfRegion' - Undocumented member.
--
-- 'numberOfDevicesInGroup', 'loRaWANMulticastGet_numberOfDevicesInGroup' - Undocumented member.
--
-- 'dlClass', 'loRaWANMulticastGet_dlClass' - Undocumented member.
--
-- 'numberOfDevicesRequested', 'loRaWANMulticastGet_numberOfDevicesRequested' - Undocumented member.
newLoRaWANMulticastGet ::
  LoRaWANMulticastGet
newLoRaWANMulticastGet =
  LoRaWANMulticastGet'
    { rfRegion = Prelude.Nothing,
      numberOfDevicesInGroup = Prelude.Nothing,
      dlClass = Prelude.Nothing,
      numberOfDevicesRequested = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANMulticastGet_rfRegion :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe SupportedRfRegion)
loRaWANMulticastGet_rfRegion = Lens.lens (\LoRaWANMulticastGet' {rfRegion} -> rfRegion) (\s@LoRaWANMulticastGet' {} a -> s {rfRegion = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_numberOfDevicesInGroup :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe Prelude.Int)
loRaWANMulticastGet_numberOfDevicesInGroup = Lens.lens (\LoRaWANMulticastGet' {numberOfDevicesInGroup} -> numberOfDevicesInGroup) (\s@LoRaWANMulticastGet' {} a -> s {numberOfDevicesInGroup = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_dlClass :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe DlClass)
loRaWANMulticastGet_dlClass = Lens.lens (\LoRaWANMulticastGet' {dlClass} -> dlClass) (\s@LoRaWANMulticastGet' {} a -> s {dlClass = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_numberOfDevicesRequested :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe Prelude.Int)
loRaWANMulticastGet_numberOfDevicesRequested = Lens.lens (\LoRaWANMulticastGet' {numberOfDevicesRequested} -> numberOfDevicesRequested) (\s@LoRaWANMulticastGet' {} a -> s {numberOfDevicesRequested = a} :: LoRaWANMulticastGet)

instance Core.FromJSON LoRaWANMulticastGet where
  parseJSON =
    Core.withObject
      "LoRaWANMulticastGet"
      ( \x ->
          LoRaWANMulticastGet'
            Prelude.<$> (x Core..:? "RfRegion")
            Prelude.<*> (x Core..:? "NumberOfDevicesInGroup")
            Prelude.<*> (x Core..:? "DlClass")
            Prelude.<*> (x Core..:? "NumberOfDevicesRequested")
      )

instance Prelude.Hashable LoRaWANMulticastGet where
  hashWithSalt _salt LoRaWANMulticastGet' {..} =
    _salt `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` numberOfDevicesInGroup
      `Prelude.hashWithSalt` dlClass
      `Prelude.hashWithSalt` numberOfDevicesRequested

instance Prelude.NFData LoRaWANMulticastGet where
  rnf LoRaWANMulticastGet' {..} =
    Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf numberOfDevicesInGroup
      `Prelude.seq` Prelude.rnf dlClass
      `Prelude.seq` Prelude.rnf numberOfDevicesRequested

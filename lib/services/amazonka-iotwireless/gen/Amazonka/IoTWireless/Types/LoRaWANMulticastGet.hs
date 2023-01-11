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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANMulticastGet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.DlClass
import Amazonka.IoTWireless.Types.SupportedRfRegion
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information that is to be returned from getting multicast
-- group information.
--
-- /See:/ 'newLoRaWANMulticastGet' smart constructor.
data LoRaWANMulticastGet = LoRaWANMulticastGet'
  { dlClass :: Prelude.Maybe DlClass,
    numberOfDevicesInGroup :: Prelude.Maybe Prelude.Int,
    numberOfDevicesRequested :: Prelude.Maybe Prelude.Int,
    rfRegion :: Prelude.Maybe SupportedRfRegion
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
-- 'dlClass', 'loRaWANMulticastGet_dlClass' - Undocumented member.
--
-- 'numberOfDevicesInGroup', 'loRaWANMulticastGet_numberOfDevicesInGroup' - Undocumented member.
--
-- 'numberOfDevicesRequested', 'loRaWANMulticastGet_numberOfDevicesRequested' - Undocumented member.
--
-- 'rfRegion', 'loRaWANMulticastGet_rfRegion' - Undocumented member.
newLoRaWANMulticastGet ::
  LoRaWANMulticastGet
newLoRaWANMulticastGet =
  LoRaWANMulticastGet'
    { dlClass = Prelude.Nothing,
      numberOfDevicesInGroup = Prelude.Nothing,
      numberOfDevicesRequested = Prelude.Nothing,
      rfRegion = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANMulticastGet_dlClass :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe DlClass)
loRaWANMulticastGet_dlClass = Lens.lens (\LoRaWANMulticastGet' {dlClass} -> dlClass) (\s@LoRaWANMulticastGet' {} a -> s {dlClass = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_numberOfDevicesInGroup :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe Prelude.Int)
loRaWANMulticastGet_numberOfDevicesInGroup = Lens.lens (\LoRaWANMulticastGet' {numberOfDevicesInGroup} -> numberOfDevicesInGroup) (\s@LoRaWANMulticastGet' {} a -> s {numberOfDevicesInGroup = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_numberOfDevicesRequested :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe Prelude.Int)
loRaWANMulticastGet_numberOfDevicesRequested = Lens.lens (\LoRaWANMulticastGet' {numberOfDevicesRequested} -> numberOfDevicesRequested) (\s@LoRaWANMulticastGet' {} a -> s {numberOfDevicesRequested = a} :: LoRaWANMulticastGet)

-- | Undocumented member.
loRaWANMulticastGet_rfRegion :: Lens.Lens' LoRaWANMulticastGet (Prelude.Maybe SupportedRfRegion)
loRaWANMulticastGet_rfRegion = Lens.lens (\LoRaWANMulticastGet' {rfRegion} -> rfRegion) (\s@LoRaWANMulticastGet' {} a -> s {rfRegion = a} :: LoRaWANMulticastGet)

instance Data.FromJSON LoRaWANMulticastGet where
  parseJSON =
    Data.withObject
      "LoRaWANMulticastGet"
      ( \x ->
          LoRaWANMulticastGet'
            Prelude.<$> (x Data..:? "DlClass")
            Prelude.<*> (x Data..:? "NumberOfDevicesInGroup")
            Prelude.<*> (x Data..:? "NumberOfDevicesRequested")
            Prelude.<*> (x Data..:? "RfRegion")
      )

instance Prelude.Hashable LoRaWANMulticastGet where
  hashWithSalt _salt LoRaWANMulticastGet' {..} =
    _salt `Prelude.hashWithSalt` dlClass
      `Prelude.hashWithSalt` numberOfDevicesInGroup
      `Prelude.hashWithSalt` numberOfDevicesRequested
      `Prelude.hashWithSalt` rfRegion

instance Prelude.NFData LoRaWANMulticastGet where
  rnf LoRaWANMulticastGet' {..} =
    Prelude.rnf dlClass
      `Prelude.seq` Prelude.rnf numberOfDevicesInGroup
      `Prelude.seq` Prelude.rnf numberOfDevicesRequested
      `Prelude.seq` Prelude.rnf rfRegion

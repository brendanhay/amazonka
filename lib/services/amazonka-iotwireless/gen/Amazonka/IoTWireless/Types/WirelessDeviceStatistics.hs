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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.FuotaDeviceStatus
import Amazonka.IoTWireless.Types.LoRaWANListDevice
import Amazonka.IoTWireless.Types.SidewalkListDevice
import Amazonka.IoTWireless.Types.WirelessDeviceType
import qualified Amazonka.Prelude as Prelude

-- | Information about a wireless device\'s operation.
--
-- /See:/ 'newWirelessDeviceStatistics' smart constructor.
data WirelessDeviceStatistics = WirelessDeviceStatistics'
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The wireless device type.
    type' :: Prelude.Maybe WirelessDeviceType,
    -- | The date and time when the most recent uplink was received.
    lastUplinkReceivedAt :: Prelude.Maybe Prelude.Text,
    -- | LoRaWAN device info.
    loRaWAN :: Prelude.Maybe LoRaWANListDevice,
    -- | The name of the destination to which the device is assigned.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless device reporting the data.
    id :: Prelude.Maybe Prelude.Text,
    mcGroupId :: Prelude.Maybe Prelude.Natural,
    -- | The status of the wireless device in the multicast group.
    multicastDeviceStatus :: Prelude.Maybe Prelude.Text,
    fuotaDeviceStatus :: Prelude.Maybe FuotaDeviceStatus,
    -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe SidewalkListDevice
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessDeviceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'wirelessDeviceStatistics_name' - The name of the resource.
--
-- 'type'', 'wirelessDeviceStatistics_type' - The wireless device type.
--
-- 'lastUplinkReceivedAt', 'wirelessDeviceStatistics_lastUplinkReceivedAt' - The date and time when the most recent uplink was received.
--
-- 'loRaWAN', 'wirelessDeviceStatistics_loRaWAN' - LoRaWAN device info.
--
-- 'destinationName', 'wirelessDeviceStatistics_destinationName' - The name of the destination to which the device is assigned.
--
-- 'arn', 'wirelessDeviceStatistics_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'wirelessDeviceStatistics_id' - The ID of the wireless device reporting the data.
--
-- 'mcGroupId', 'wirelessDeviceStatistics_mcGroupId' - Undocumented member.
--
-- 'multicastDeviceStatus', 'wirelessDeviceStatistics_multicastDeviceStatus' - The status of the wireless device in the multicast group.
--
-- 'fuotaDeviceStatus', 'wirelessDeviceStatistics_fuotaDeviceStatus' - Undocumented member.
--
-- 'sidewalk', 'wirelessDeviceStatistics_sidewalk' - The Sidewalk account credentials.
newWirelessDeviceStatistics ::
  WirelessDeviceStatistics
newWirelessDeviceStatistics =
  WirelessDeviceStatistics'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastUplinkReceivedAt = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      mcGroupId = Prelude.Nothing,
      multicastDeviceStatus = Prelude.Nothing,
      fuotaDeviceStatus = Prelude.Nothing,
      sidewalk = Prelude.Nothing
    }

-- | The name of the resource.
wirelessDeviceStatistics_name :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_name = Lens.lens (\WirelessDeviceStatistics' {name} -> name) (\s@WirelessDeviceStatistics' {} a -> s {name = a} :: WirelessDeviceStatistics)

-- | The wireless device type.
wirelessDeviceStatistics_type :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe WirelessDeviceType)
wirelessDeviceStatistics_type = Lens.lens (\WirelessDeviceStatistics' {type'} -> type') (\s@WirelessDeviceStatistics' {} a -> s {type' = a} :: WirelessDeviceStatistics)

-- | The date and time when the most recent uplink was received.
wirelessDeviceStatistics_lastUplinkReceivedAt :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_lastUplinkReceivedAt = Lens.lens (\WirelessDeviceStatistics' {lastUplinkReceivedAt} -> lastUplinkReceivedAt) (\s@WirelessDeviceStatistics' {} a -> s {lastUplinkReceivedAt = a} :: WirelessDeviceStatistics)

-- | LoRaWAN device info.
wirelessDeviceStatistics_loRaWAN :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe LoRaWANListDevice)
wirelessDeviceStatistics_loRaWAN = Lens.lens (\WirelessDeviceStatistics' {loRaWAN} -> loRaWAN) (\s@WirelessDeviceStatistics' {} a -> s {loRaWAN = a} :: WirelessDeviceStatistics)

-- | The name of the destination to which the device is assigned.
wirelessDeviceStatistics_destinationName :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_destinationName = Lens.lens (\WirelessDeviceStatistics' {destinationName} -> destinationName) (\s@WirelessDeviceStatistics' {} a -> s {destinationName = a} :: WirelessDeviceStatistics)

-- | The Amazon Resource Name of the resource.
wirelessDeviceStatistics_arn :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_arn = Lens.lens (\WirelessDeviceStatistics' {arn} -> arn) (\s@WirelessDeviceStatistics' {} a -> s {arn = a} :: WirelessDeviceStatistics)

-- | The ID of the wireless device reporting the data.
wirelessDeviceStatistics_id :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_id = Lens.lens (\WirelessDeviceStatistics' {id} -> id) (\s@WirelessDeviceStatistics' {} a -> s {id = a} :: WirelessDeviceStatistics)

-- | Undocumented member.
wirelessDeviceStatistics_mcGroupId :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Natural)
wirelessDeviceStatistics_mcGroupId = Lens.lens (\WirelessDeviceStatistics' {mcGroupId} -> mcGroupId) (\s@WirelessDeviceStatistics' {} a -> s {mcGroupId = a} :: WirelessDeviceStatistics)

-- | The status of the wireless device in the multicast group.
wirelessDeviceStatistics_multicastDeviceStatus :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe Prelude.Text)
wirelessDeviceStatistics_multicastDeviceStatus = Lens.lens (\WirelessDeviceStatistics' {multicastDeviceStatus} -> multicastDeviceStatus) (\s@WirelessDeviceStatistics' {} a -> s {multicastDeviceStatus = a} :: WirelessDeviceStatistics)

-- | Undocumented member.
wirelessDeviceStatistics_fuotaDeviceStatus :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe FuotaDeviceStatus)
wirelessDeviceStatistics_fuotaDeviceStatus = Lens.lens (\WirelessDeviceStatistics' {fuotaDeviceStatus} -> fuotaDeviceStatus) (\s@WirelessDeviceStatistics' {} a -> s {fuotaDeviceStatus = a} :: WirelessDeviceStatistics)

-- | The Sidewalk account credentials.
wirelessDeviceStatistics_sidewalk :: Lens.Lens' WirelessDeviceStatistics (Prelude.Maybe SidewalkListDevice)
wirelessDeviceStatistics_sidewalk = Lens.lens (\WirelessDeviceStatistics' {sidewalk} -> sidewalk) (\s@WirelessDeviceStatistics' {} a -> s {sidewalk = a} :: WirelessDeviceStatistics)

instance Data.FromJSON WirelessDeviceStatistics where
  parseJSON =
    Data.withObject
      "WirelessDeviceStatistics"
      ( \x ->
          WirelessDeviceStatistics'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "LastUplinkReceivedAt")
            Prelude.<*> (x Data..:? "LoRaWAN")
            Prelude.<*> (x Data..:? "DestinationName")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "McGroupId")
            Prelude.<*> (x Data..:? "MulticastDeviceStatus")
            Prelude.<*> (x Data..:? "FuotaDeviceStatus")
            Prelude.<*> (x Data..:? "Sidewalk")
      )

instance Prelude.Hashable WirelessDeviceStatistics where
  hashWithSalt _salt WirelessDeviceStatistics' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` lastUplinkReceivedAt
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` mcGroupId
      `Prelude.hashWithSalt` multicastDeviceStatus
      `Prelude.hashWithSalt` fuotaDeviceStatus
      `Prelude.hashWithSalt` sidewalk

instance Prelude.NFData WirelessDeviceStatistics where
  rnf WirelessDeviceStatistics' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf lastUplinkReceivedAt
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf mcGroupId
      `Prelude.seq` Prelude.rnf multicastDeviceStatus
      `Prelude.seq` Prelude.rnf fuotaDeviceStatus
      `Prelude.seq` Prelude.rnf sidewalk

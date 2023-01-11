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
-- Module      : Amazonka.IoTWireless.Types.ParticipatingGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ParticipatingGateways where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.DownlinkMode
import Amazonka.IoTWireless.Types.GatewayListItem
import qualified Amazonka.Prelude as Prelude

-- | Specify the list of gateways to which you want to send downlink data
-- traffic when the wireless device is running in class B or class C mode.
--
-- /See:/ 'newParticipatingGateways' smart constructor.
data ParticipatingGateways = ParticipatingGateways'
  { -- | Indicates whether to send the downlink message in sequential mode or
    -- concurrent mode, or to use only the chosen gateways from the previous
    -- uplink message transmission.
    downlinkMode :: DownlinkMode,
    -- | The list of gateways that you want to use for sending the downlink data
    -- traffic.
    gatewayList :: [GatewayListItem],
    -- | The duration of time for which AWS IoT Core for LoRaWAN will wait before
    -- transmitting the payload to the next gateway.
    transmissionInterval :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipatingGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downlinkMode', 'participatingGateways_downlinkMode' - Indicates whether to send the downlink message in sequential mode or
-- concurrent mode, or to use only the chosen gateways from the previous
-- uplink message transmission.
--
-- 'gatewayList', 'participatingGateways_gatewayList' - The list of gateways that you want to use for sending the downlink data
-- traffic.
--
-- 'transmissionInterval', 'participatingGateways_transmissionInterval' - The duration of time for which AWS IoT Core for LoRaWAN will wait before
-- transmitting the payload to the next gateway.
newParticipatingGateways ::
  -- | 'downlinkMode'
  DownlinkMode ->
  -- | 'transmissionInterval'
  Prelude.Natural ->
  ParticipatingGateways
newParticipatingGateways
  pDownlinkMode_
  pTransmissionInterval_ =
    ParticipatingGateways'
      { downlinkMode =
          pDownlinkMode_,
        gatewayList = Prelude.mempty,
        transmissionInterval = pTransmissionInterval_
      }

-- | Indicates whether to send the downlink message in sequential mode or
-- concurrent mode, or to use only the chosen gateways from the previous
-- uplink message transmission.
participatingGateways_downlinkMode :: Lens.Lens' ParticipatingGateways DownlinkMode
participatingGateways_downlinkMode = Lens.lens (\ParticipatingGateways' {downlinkMode} -> downlinkMode) (\s@ParticipatingGateways' {} a -> s {downlinkMode = a} :: ParticipatingGateways)

-- | The list of gateways that you want to use for sending the downlink data
-- traffic.
participatingGateways_gatewayList :: Lens.Lens' ParticipatingGateways [GatewayListItem]
participatingGateways_gatewayList = Lens.lens (\ParticipatingGateways' {gatewayList} -> gatewayList) (\s@ParticipatingGateways' {} a -> s {gatewayList = a} :: ParticipatingGateways) Prelude.. Lens.coerced

-- | The duration of time for which AWS IoT Core for LoRaWAN will wait before
-- transmitting the payload to the next gateway.
participatingGateways_transmissionInterval :: Lens.Lens' ParticipatingGateways Prelude.Natural
participatingGateways_transmissionInterval = Lens.lens (\ParticipatingGateways' {transmissionInterval} -> transmissionInterval) (\s@ParticipatingGateways' {} a -> s {transmissionInterval = a} :: ParticipatingGateways)

instance Data.FromJSON ParticipatingGateways where
  parseJSON =
    Data.withObject
      "ParticipatingGateways"
      ( \x ->
          ParticipatingGateways'
            Prelude.<$> (x Data..: "DownlinkMode")
            Prelude.<*> (x Data..:? "GatewayList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "TransmissionInterval")
      )

instance Prelude.Hashable ParticipatingGateways where
  hashWithSalt _salt ParticipatingGateways' {..} =
    _salt `Prelude.hashWithSalt` downlinkMode
      `Prelude.hashWithSalt` gatewayList
      `Prelude.hashWithSalt` transmissionInterval

instance Prelude.NFData ParticipatingGateways where
  rnf ParticipatingGateways' {..} =
    Prelude.rnf downlinkMode
      `Prelude.seq` Prelude.rnf gatewayList
      `Prelude.seq` Prelude.rnf transmissionInterval

instance Data.ToJSON ParticipatingGateways where
  toJSON ParticipatingGateways' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DownlinkMode" Data..= downlinkMode),
            Prelude.Just ("GatewayList" Data..= gatewayList),
            Prelude.Just
              ( "TransmissionInterval"
                  Data..= transmissionInterval
              )
          ]
      )

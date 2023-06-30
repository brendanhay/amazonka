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
-- Module      : Amazonka.IoTWireless.Types.GatewayListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.GatewayListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Gateway list item object that specifies the frequency and list of
-- gateways for which the downlink message should be sent.
--
-- /See:/ 'newGatewayListItem' smart constructor.
data GatewayListItem = GatewayListItem'
  { -- | The ID of the wireless gateways that you want to add to the list of
    -- gateways when sending downlink messages.
    gatewayId :: Prelude.Text,
    -- | The frequency to use for the gateways when sending a downlink message to
    -- the wireless device.
    downlinkFrequency :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'gatewayListItem_gatewayId' - The ID of the wireless gateways that you want to add to the list of
-- gateways when sending downlink messages.
--
-- 'downlinkFrequency', 'gatewayListItem_downlinkFrequency' - The frequency to use for the gateways when sending a downlink message to
-- the wireless device.
newGatewayListItem ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'downlinkFrequency'
  Prelude.Natural ->
  GatewayListItem
newGatewayListItem pGatewayId_ pDownlinkFrequency_ =
  GatewayListItem'
    { gatewayId = pGatewayId_,
      downlinkFrequency = pDownlinkFrequency_
    }

-- | The ID of the wireless gateways that you want to add to the list of
-- gateways when sending downlink messages.
gatewayListItem_gatewayId :: Lens.Lens' GatewayListItem Prelude.Text
gatewayListItem_gatewayId = Lens.lens (\GatewayListItem' {gatewayId} -> gatewayId) (\s@GatewayListItem' {} a -> s {gatewayId = a} :: GatewayListItem)

-- | The frequency to use for the gateways when sending a downlink message to
-- the wireless device.
gatewayListItem_downlinkFrequency :: Lens.Lens' GatewayListItem Prelude.Natural
gatewayListItem_downlinkFrequency = Lens.lens (\GatewayListItem' {downlinkFrequency} -> downlinkFrequency) (\s@GatewayListItem' {} a -> s {downlinkFrequency = a} :: GatewayListItem)

instance Data.FromJSON GatewayListItem where
  parseJSON =
    Data.withObject
      "GatewayListItem"
      ( \x ->
          GatewayListItem'
            Prelude.<$> (x Data..: "GatewayId")
            Prelude.<*> (x Data..: "DownlinkFrequency")
      )

instance Prelude.Hashable GatewayListItem where
  hashWithSalt _salt GatewayListItem' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` downlinkFrequency

instance Prelude.NFData GatewayListItem where
  rnf GatewayListItem' {..} =
    Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf downlinkFrequency

instance Data.ToJSON GatewayListItem where
  toJSON GatewayListItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayId" Data..= gatewayId),
            Prelude.Just
              ("DownlinkFrequency" Data..= downlinkFrequency)
          ]
      )

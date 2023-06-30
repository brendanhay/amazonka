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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.Beaconing
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANGateway object.
--
-- /See:/ 'newLoRaWANGateway' smart constructor.
data LoRaWANGateway = LoRaWANGateway'
  { -- | Beaconing object information, which consists of the data rate and
    -- frequency parameters.
    beaconing :: Prelude.Maybe Beaconing,
    -- | The gateway\'s EUI value.
    gatewayEui :: Prelude.Maybe Prelude.Text,
    joinEuiFilters :: Prelude.Maybe [Prelude.NonEmpty Prelude.Text],
    netIdFilters :: Prelude.Maybe [Prelude.Text],
    -- | The frequency band (RFRegion) value.
    rfRegion :: Prelude.Maybe Prelude.Text,
    subBands :: Prelude.Maybe [Prelude.Natural]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beaconing', 'loRaWANGateway_beaconing' - Beaconing object information, which consists of the data rate and
-- frequency parameters.
--
-- 'gatewayEui', 'loRaWANGateway_gatewayEui' - The gateway\'s EUI value.
--
-- 'joinEuiFilters', 'loRaWANGateway_joinEuiFilters' - Undocumented member.
--
-- 'netIdFilters', 'loRaWANGateway_netIdFilters' - Undocumented member.
--
-- 'rfRegion', 'loRaWANGateway_rfRegion' - The frequency band (RFRegion) value.
--
-- 'subBands', 'loRaWANGateway_subBands' - Undocumented member.
newLoRaWANGateway ::
  LoRaWANGateway
newLoRaWANGateway =
  LoRaWANGateway'
    { beaconing = Prelude.Nothing,
      gatewayEui = Prelude.Nothing,
      joinEuiFilters = Prelude.Nothing,
      netIdFilters = Prelude.Nothing,
      rfRegion = Prelude.Nothing,
      subBands = Prelude.Nothing
    }

-- | Beaconing object information, which consists of the data rate and
-- frequency parameters.
loRaWANGateway_beaconing :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Beaconing)
loRaWANGateway_beaconing = Lens.lens (\LoRaWANGateway' {beaconing} -> beaconing) (\s@LoRaWANGateway' {} a -> s {beaconing = a} :: LoRaWANGateway)

-- | The gateway\'s EUI value.
loRaWANGateway_gatewayEui :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Prelude.Text)
loRaWANGateway_gatewayEui = Lens.lens (\LoRaWANGateway' {gatewayEui} -> gatewayEui) (\s@LoRaWANGateway' {} a -> s {gatewayEui = a} :: LoRaWANGateway)

-- | Undocumented member.
loRaWANGateway_joinEuiFilters :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.NonEmpty Prelude.Text])
loRaWANGateway_joinEuiFilters = Lens.lens (\LoRaWANGateway' {joinEuiFilters} -> joinEuiFilters) (\s@LoRaWANGateway' {} a -> s {joinEuiFilters = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
loRaWANGateway_netIdFilters :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.Text])
loRaWANGateway_netIdFilters = Lens.lens (\LoRaWANGateway' {netIdFilters} -> netIdFilters) (\s@LoRaWANGateway' {} a -> s {netIdFilters = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

-- | The frequency band (RFRegion) value.
loRaWANGateway_rfRegion :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Prelude.Text)
loRaWANGateway_rfRegion = Lens.lens (\LoRaWANGateway' {rfRegion} -> rfRegion) (\s@LoRaWANGateway' {} a -> s {rfRegion = a} :: LoRaWANGateway)

-- | Undocumented member.
loRaWANGateway_subBands :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.Natural])
loRaWANGateway_subBands = Lens.lens (\LoRaWANGateway' {subBands} -> subBands) (\s@LoRaWANGateway' {} a -> s {subBands = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LoRaWANGateway where
  parseJSON =
    Data.withObject
      "LoRaWANGateway"
      ( \x ->
          LoRaWANGateway'
            Prelude.<$> (x Data..:? "Beaconing")
            Prelude.<*> (x Data..:? "GatewayEui")
            Prelude.<*> (x Data..:? "JoinEuiFilters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NetIdFilters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RfRegion")
            Prelude.<*> (x Data..:? "SubBands" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LoRaWANGateway where
  hashWithSalt _salt LoRaWANGateway' {..} =
    _salt
      `Prelude.hashWithSalt` beaconing
      `Prelude.hashWithSalt` gatewayEui
      `Prelude.hashWithSalt` joinEuiFilters
      `Prelude.hashWithSalt` netIdFilters
      `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` subBands

instance Prelude.NFData LoRaWANGateway where
  rnf LoRaWANGateway' {..} =
    Prelude.rnf beaconing
      `Prelude.seq` Prelude.rnf gatewayEui
      `Prelude.seq` Prelude.rnf joinEuiFilters
      `Prelude.seq` Prelude.rnf netIdFilters
      `Prelude.seq` Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf subBands

instance Data.ToJSON LoRaWANGateway where
  toJSON LoRaWANGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Beaconing" Data..=) Prelude.<$> beaconing,
            ("GatewayEui" Data..=) Prelude.<$> gatewayEui,
            ("JoinEuiFilters" Data..=)
              Prelude.<$> joinEuiFilters,
            ("NetIdFilters" Data..=) Prelude.<$> netIdFilters,
            ("RfRegion" Data..=) Prelude.<$> rfRegion,
            ("SubBands" Data..=) Prelude.<$> subBands
          ]
      )

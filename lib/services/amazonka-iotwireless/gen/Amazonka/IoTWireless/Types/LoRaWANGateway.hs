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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.Beaconing
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANGateway object.
--
-- /See:/ 'newLoRaWANGateway' smart constructor.
data LoRaWANGateway = LoRaWANGateway'
  { -- | The frequency band (RFRegion) value.
    rfRegion :: Prelude.Maybe Prelude.Text,
    subBands :: Prelude.Maybe [Prelude.Natural],
    -- | Beaconing object information, which consists of the data rate and
    -- frequency parameters.
    beaconing :: Prelude.Maybe Beaconing,
    -- | The gateway\'s EUI value.
    gatewayEui :: Prelude.Maybe Prelude.Text,
    netIdFilters :: Prelude.Maybe [Prelude.Text],
    joinEuiFilters :: Prelude.Maybe [Prelude.NonEmpty Prelude.Text]
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
-- 'rfRegion', 'loRaWANGateway_rfRegion' - The frequency band (RFRegion) value.
--
-- 'subBands', 'loRaWANGateway_subBands' - Undocumented member.
--
-- 'beaconing', 'loRaWANGateway_beaconing' - Beaconing object information, which consists of the data rate and
-- frequency parameters.
--
-- 'gatewayEui', 'loRaWANGateway_gatewayEui' - The gateway\'s EUI value.
--
-- 'netIdFilters', 'loRaWANGateway_netIdFilters' - Undocumented member.
--
-- 'joinEuiFilters', 'loRaWANGateway_joinEuiFilters' - Undocumented member.
newLoRaWANGateway ::
  LoRaWANGateway
newLoRaWANGateway =
  LoRaWANGateway'
    { rfRegion = Prelude.Nothing,
      subBands = Prelude.Nothing,
      beaconing = Prelude.Nothing,
      gatewayEui = Prelude.Nothing,
      netIdFilters = Prelude.Nothing,
      joinEuiFilters = Prelude.Nothing
    }

-- | The frequency band (RFRegion) value.
loRaWANGateway_rfRegion :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Prelude.Text)
loRaWANGateway_rfRegion = Lens.lens (\LoRaWANGateway' {rfRegion} -> rfRegion) (\s@LoRaWANGateway' {} a -> s {rfRegion = a} :: LoRaWANGateway)

-- | Undocumented member.
loRaWANGateway_subBands :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.Natural])
loRaWANGateway_subBands = Lens.lens (\LoRaWANGateway' {subBands} -> subBands) (\s@LoRaWANGateway' {} a -> s {subBands = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

-- | Beaconing object information, which consists of the data rate and
-- frequency parameters.
loRaWANGateway_beaconing :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Beaconing)
loRaWANGateway_beaconing = Lens.lens (\LoRaWANGateway' {beaconing} -> beaconing) (\s@LoRaWANGateway' {} a -> s {beaconing = a} :: LoRaWANGateway)

-- | The gateway\'s EUI value.
loRaWANGateway_gatewayEui :: Lens.Lens' LoRaWANGateway (Prelude.Maybe Prelude.Text)
loRaWANGateway_gatewayEui = Lens.lens (\LoRaWANGateway' {gatewayEui} -> gatewayEui) (\s@LoRaWANGateway' {} a -> s {gatewayEui = a} :: LoRaWANGateway)

-- | Undocumented member.
loRaWANGateway_netIdFilters :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.Text])
loRaWANGateway_netIdFilters = Lens.lens (\LoRaWANGateway' {netIdFilters} -> netIdFilters) (\s@LoRaWANGateway' {} a -> s {netIdFilters = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
loRaWANGateway_joinEuiFilters :: Lens.Lens' LoRaWANGateway (Prelude.Maybe [Prelude.NonEmpty Prelude.Text])
loRaWANGateway_joinEuiFilters = Lens.lens (\LoRaWANGateway' {joinEuiFilters} -> joinEuiFilters) (\s@LoRaWANGateway' {} a -> s {joinEuiFilters = a} :: LoRaWANGateway) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LoRaWANGateway where
  parseJSON =
    Core.withObject
      "LoRaWANGateway"
      ( \x ->
          LoRaWANGateway'
            Prelude.<$> (x Core..:? "RfRegion")
            Prelude.<*> (x Core..:? "SubBands" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Beaconing")
            Prelude.<*> (x Core..:? "GatewayEui")
            Prelude.<*> (x Core..:? "NetIdFilters" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "JoinEuiFilters"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LoRaWANGateway where
  hashWithSalt _salt LoRaWANGateway' {..} =
    _salt `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` subBands
      `Prelude.hashWithSalt` beaconing
      `Prelude.hashWithSalt` gatewayEui
      `Prelude.hashWithSalt` netIdFilters
      `Prelude.hashWithSalt` joinEuiFilters

instance Prelude.NFData LoRaWANGateway where
  rnf LoRaWANGateway' {..} =
    Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf subBands
      `Prelude.seq` Prelude.rnf beaconing
      `Prelude.seq` Prelude.rnf gatewayEui
      `Prelude.seq` Prelude.rnf netIdFilters
      `Prelude.seq` Prelude.rnf joinEuiFilters

instance Core.ToJSON LoRaWANGateway where
  toJSON LoRaWANGateway' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RfRegion" Core..=) Prelude.<$> rfRegion,
            ("SubBands" Core..=) Prelude.<$> subBands,
            ("Beaconing" Core..=) Prelude.<$> beaconing,
            ("GatewayEui" Core..=) Prelude.<$> gatewayEui,
            ("NetIdFilters" Core..=) Prelude.<$> netIdFilters,
            ("JoinEuiFilters" Core..=)
              Prelude.<$> joinEuiFilters
          ]
      )

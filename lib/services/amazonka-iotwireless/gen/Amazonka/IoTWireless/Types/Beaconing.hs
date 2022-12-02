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
-- Module      : Amazonka.IoTWireless.Types.Beaconing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Beaconing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Beaconing parameters for configuring the wireless gateways.
--
-- /See:/ 'newBeaconing' smart constructor.
data Beaconing = Beaconing'
  { -- | The frequency list for the gateways to send the beacons.
    frequencies :: Prelude.Maybe [Prelude.Natural],
    -- | The data rate for gateways that are sending the beacons.
    dataRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Beaconing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequencies', 'beaconing_frequencies' - The frequency list for the gateways to send the beacons.
--
-- 'dataRate', 'beaconing_dataRate' - The data rate for gateways that are sending the beacons.
newBeaconing ::
  Beaconing
newBeaconing =
  Beaconing'
    { frequencies = Prelude.Nothing,
      dataRate = Prelude.Nothing
    }

-- | The frequency list for the gateways to send the beacons.
beaconing_frequencies :: Lens.Lens' Beaconing (Prelude.Maybe [Prelude.Natural])
beaconing_frequencies = Lens.lens (\Beaconing' {frequencies} -> frequencies) (\s@Beaconing' {} a -> s {frequencies = a} :: Beaconing) Prelude.. Lens.mapping Lens.coerced

-- | The data rate for gateways that are sending the beacons.
beaconing_dataRate :: Lens.Lens' Beaconing (Prelude.Maybe Prelude.Natural)
beaconing_dataRate = Lens.lens (\Beaconing' {dataRate} -> dataRate) (\s@Beaconing' {} a -> s {dataRate = a} :: Beaconing)

instance Data.FromJSON Beaconing where
  parseJSON =
    Data.withObject
      "Beaconing"
      ( \x ->
          Beaconing'
            Prelude.<$> (x Data..:? "Frequencies" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DataRate")
      )

instance Prelude.Hashable Beaconing where
  hashWithSalt _salt Beaconing' {..} =
    _salt `Prelude.hashWithSalt` frequencies
      `Prelude.hashWithSalt` dataRate

instance Prelude.NFData Beaconing where
  rnf Beaconing' {..} =
    Prelude.rnf frequencies
      `Prelude.seq` Prelude.rnf dataRate

instance Data.ToJSON Beaconing where
  toJSON Beaconing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Frequencies" Data..=) Prelude.<$> frequencies,
            ("DataRate" Data..=) Prelude.<$> dataRate
          ]
      )

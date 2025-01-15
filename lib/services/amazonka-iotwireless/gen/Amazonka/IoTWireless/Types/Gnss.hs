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
-- Module      : Amazonka.IoTWireless.Types.Gnss
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Gnss where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Global navigation satellite system (GNSS) object used for positioning.
--
-- /See:/ 'newGnss' smart constructor.
data Gnss = Gnss'
  { -- | Optional assistance altitude, which is the altitude of the device at
    -- capture time, specified in meters above the WGS84 reference ellipsoid.
    assistAltitude :: Prelude.Maybe Prelude.Double,
    -- | Optional assistance position information, specified using latitude and
    -- longitude values in degrees. The co-ordinates are inside the WGS84
    -- reference frame.
    assistPosition :: Prelude.Maybe (Prelude.NonEmpty Prelude.Double),
    -- | Optional parameter that gives an estimate of the time when the GNSS scan
    -- information is taken, in seconds GPS time (GPST). If capture time is not
    -- specified, the local server time is used.
    captureTime :: Prelude.Maybe Prelude.Double,
    -- | Optional value that gives the capture time estimate accuracy, in
    -- seconds. If capture time accuracy is not specified, default value of 300
    -- is used.
    captureTimeAccuracy :: Prelude.Maybe Prelude.Double,
    -- | Optional parameter that forces 2D solve, which modifies the positioning
    -- algorithm to a 2D solution problem. When this parameter is specified,
    -- the assistance altitude should have an accuracy of at least 10 meters.
    use2DSolver :: Prelude.Maybe Prelude.Bool,
    -- | Payload that contains the GNSS scan result, or NAV message, in
    -- hexadecimal notation.
    payload :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Gnss' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistAltitude', 'gnss_assistAltitude' - Optional assistance altitude, which is the altitude of the device at
-- capture time, specified in meters above the WGS84 reference ellipsoid.
--
-- 'assistPosition', 'gnss_assistPosition' - Optional assistance position information, specified using latitude and
-- longitude values in degrees. The co-ordinates are inside the WGS84
-- reference frame.
--
-- 'captureTime', 'gnss_captureTime' - Optional parameter that gives an estimate of the time when the GNSS scan
-- information is taken, in seconds GPS time (GPST). If capture time is not
-- specified, the local server time is used.
--
-- 'captureTimeAccuracy', 'gnss_captureTimeAccuracy' - Optional value that gives the capture time estimate accuracy, in
-- seconds. If capture time accuracy is not specified, default value of 300
-- is used.
--
-- 'use2DSolver', 'gnss_use2DSolver' - Optional parameter that forces 2D solve, which modifies the positioning
-- algorithm to a 2D solution problem. When this parameter is specified,
-- the assistance altitude should have an accuracy of at least 10 meters.
--
-- 'payload', 'gnss_payload' - Payload that contains the GNSS scan result, or NAV message, in
-- hexadecimal notation.
newGnss ::
  -- | 'payload'
  Prelude.Text ->
  Gnss
newGnss pPayload_ =
  Gnss'
    { assistAltitude = Prelude.Nothing,
      assistPosition = Prelude.Nothing,
      captureTime = Prelude.Nothing,
      captureTimeAccuracy = Prelude.Nothing,
      use2DSolver = Prelude.Nothing,
      payload = pPayload_
    }

-- | Optional assistance altitude, which is the altitude of the device at
-- capture time, specified in meters above the WGS84 reference ellipsoid.
gnss_assistAltitude :: Lens.Lens' Gnss (Prelude.Maybe Prelude.Double)
gnss_assistAltitude = Lens.lens (\Gnss' {assistAltitude} -> assistAltitude) (\s@Gnss' {} a -> s {assistAltitude = a} :: Gnss)

-- | Optional assistance position information, specified using latitude and
-- longitude values in degrees. The co-ordinates are inside the WGS84
-- reference frame.
gnss_assistPosition :: Lens.Lens' Gnss (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
gnss_assistPosition = Lens.lens (\Gnss' {assistPosition} -> assistPosition) (\s@Gnss' {} a -> s {assistPosition = a} :: Gnss) Prelude.. Lens.mapping Lens.coerced

-- | Optional parameter that gives an estimate of the time when the GNSS scan
-- information is taken, in seconds GPS time (GPST). If capture time is not
-- specified, the local server time is used.
gnss_captureTime :: Lens.Lens' Gnss (Prelude.Maybe Prelude.Double)
gnss_captureTime = Lens.lens (\Gnss' {captureTime} -> captureTime) (\s@Gnss' {} a -> s {captureTime = a} :: Gnss)

-- | Optional value that gives the capture time estimate accuracy, in
-- seconds. If capture time accuracy is not specified, default value of 300
-- is used.
gnss_captureTimeAccuracy :: Lens.Lens' Gnss (Prelude.Maybe Prelude.Double)
gnss_captureTimeAccuracy = Lens.lens (\Gnss' {captureTimeAccuracy} -> captureTimeAccuracy) (\s@Gnss' {} a -> s {captureTimeAccuracy = a} :: Gnss)

-- | Optional parameter that forces 2D solve, which modifies the positioning
-- algorithm to a 2D solution problem. When this parameter is specified,
-- the assistance altitude should have an accuracy of at least 10 meters.
gnss_use2DSolver :: Lens.Lens' Gnss (Prelude.Maybe Prelude.Bool)
gnss_use2DSolver = Lens.lens (\Gnss' {use2DSolver} -> use2DSolver) (\s@Gnss' {} a -> s {use2DSolver = a} :: Gnss)

-- | Payload that contains the GNSS scan result, or NAV message, in
-- hexadecimal notation.
gnss_payload :: Lens.Lens' Gnss Prelude.Text
gnss_payload = Lens.lens (\Gnss' {payload} -> payload) (\s@Gnss' {} a -> s {payload = a} :: Gnss)

instance Prelude.Hashable Gnss where
  hashWithSalt _salt Gnss' {..} =
    _salt
      `Prelude.hashWithSalt` assistAltitude
      `Prelude.hashWithSalt` assistPosition
      `Prelude.hashWithSalt` captureTime
      `Prelude.hashWithSalt` captureTimeAccuracy
      `Prelude.hashWithSalt` use2DSolver
      `Prelude.hashWithSalt` payload

instance Prelude.NFData Gnss where
  rnf Gnss' {..} =
    Prelude.rnf assistAltitude `Prelude.seq`
      Prelude.rnf assistPosition `Prelude.seq`
        Prelude.rnf captureTime `Prelude.seq`
          Prelude.rnf captureTimeAccuracy `Prelude.seq`
            Prelude.rnf use2DSolver `Prelude.seq`
              Prelude.rnf payload

instance Data.ToJSON Gnss where
  toJSON Gnss' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssistAltitude" Data..=)
              Prelude.<$> assistAltitude,
            ("AssistPosition" Data..=)
              Prelude.<$> assistPosition,
            ("CaptureTime" Data..=) Prelude.<$> captureTime,
            ("CaptureTimeAccuracy" Data..=)
              Prelude.<$> captureTimeAccuracy,
            ("Use2DSolver" Data..=) Prelude.<$> use2DSolver,
            Prelude.Just ("Payload" Data..= payload)
          ]
      )

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
-- Module      : Amazonka.IoTFleetWise.Types.ObdSignal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.ObdSignal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about signal messages using the on-board diagnostics (OBD)
-- II protocol in a vehicle.
--
-- /See:/ 'newObdSignal' smart constructor.
data ObdSignal = ObdSignal'
  { -- | The number of bits to mask in a message.
    bitMaskLength :: Prelude.Maybe Prelude.Natural,
    -- | The number of positions to shift bits in the message.
    bitRightShift :: Prelude.Maybe Prelude.Natural,
    -- | The length of the requested data.
    pidResponseLength :: Prelude.Natural,
    -- | The mode of operation (diagnostic service) in a message.
    serviceMode :: Prelude.Natural,
    -- | The diagnostic code used to request data from a vehicle for this signal.
    pid :: Prelude.Natural,
    -- | A multiplier used to decode the message.
    scaling :: Prelude.Double,
    -- | Indicates where data appears in the message.
    offset :: Prelude.Double,
    -- | Indicates the beginning of the message.
    startByte :: Prelude.Natural,
    -- | The length of a message.
    byteLength :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObdSignal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitMaskLength', 'obdSignal_bitMaskLength' - The number of bits to mask in a message.
--
-- 'bitRightShift', 'obdSignal_bitRightShift' - The number of positions to shift bits in the message.
--
-- 'pidResponseLength', 'obdSignal_pidResponseLength' - The length of the requested data.
--
-- 'serviceMode', 'obdSignal_serviceMode' - The mode of operation (diagnostic service) in a message.
--
-- 'pid', 'obdSignal_pid' - The diagnostic code used to request data from a vehicle for this signal.
--
-- 'scaling', 'obdSignal_scaling' - A multiplier used to decode the message.
--
-- 'offset', 'obdSignal_offset' - Indicates where data appears in the message.
--
-- 'startByte', 'obdSignal_startByte' - Indicates the beginning of the message.
--
-- 'byteLength', 'obdSignal_byteLength' - The length of a message.
newObdSignal ::
  -- | 'pidResponseLength'
  Prelude.Natural ->
  -- | 'serviceMode'
  Prelude.Natural ->
  -- | 'pid'
  Prelude.Natural ->
  -- | 'scaling'
  Prelude.Double ->
  -- | 'offset'
  Prelude.Double ->
  -- | 'startByte'
  Prelude.Natural ->
  -- | 'byteLength'
  Prelude.Natural ->
  ObdSignal
newObdSignal
  pPidResponseLength_
  pServiceMode_
  pPid_
  pScaling_
  pOffset_
  pStartByte_
  pByteLength_ =
    ObdSignal'
      { bitMaskLength = Prelude.Nothing,
        bitRightShift = Prelude.Nothing,
        pidResponseLength = pPidResponseLength_,
        serviceMode = pServiceMode_,
        pid = pPid_,
        scaling = pScaling_,
        offset = pOffset_,
        startByte = pStartByte_,
        byteLength = pByteLength_
      }

-- | The number of bits to mask in a message.
obdSignal_bitMaskLength :: Lens.Lens' ObdSignal (Prelude.Maybe Prelude.Natural)
obdSignal_bitMaskLength = Lens.lens (\ObdSignal' {bitMaskLength} -> bitMaskLength) (\s@ObdSignal' {} a -> s {bitMaskLength = a} :: ObdSignal)

-- | The number of positions to shift bits in the message.
obdSignal_bitRightShift :: Lens.Lens' ObdSignal (Prelude.Maybe Prelude.Natural)
obdSignal_bitRightShift = Lens.lens (\ObdSignal' {bitRightShift} -> bitRightShift) (\s@ObdSignal' {} a -> s {bitRightShift = a} :: ObdSignal)

-- | The length of the requested data.
obdSignal_pidResponseLength :: Lens.Lens' ObdSignal Prelude.Natural
obdSignal_pidResponseLength = Lens.lens (\ObdSignal' {pidResponseLength} -> pidResponseLength) (\s@ObdSignal' {} a -> s {pidResponseLength = a} :: ObdSignal)

-- | The mode of operation (diagnostic service) in a message.
obdSignal_serviceMode :: Lens.Lens' ObdSignal Prelude.Natural
obdSignal_serviceMode = Lens.lens (\ObdSignal' {serviceMode} -> serviceMode) (\s@ObdSignal' {} a -> s {serviceMode = a} :: ObdSignal)

-- | The diagnostic code used to request data from a vehicle for this signal.
obdSignal_pid :: Lens.Lens' ObdSignal Prelude.Natural
obdSignal_pid = Lens.lens (\ObdSignal' {pid} -> pid) (\s@ObdSignal' {} a -> s {pid = a} :: ObdSignal)

-- | A multiplier used to decode the message.
obdSignal_scaling :: Lens.Lens' ObdSignal Prelude.Double
obdSignal_scaling = Lens.lens (\ObdSignal' {scaling} -> scaling) (\s@ObdSignal' {} a -> s {scaling = a} :: ObdSignal)

-- | Indicates where data appears in the message.
obdSignal_offset :: Lens.Lens' ObdSignal Prelude.Double
obdSignal_offset = Lens.lens (\ObdSignal' {offset} -> offset) (\s@ObdSignal' {} a -> s {offset = a} :: ObdSignal)

-- | Indicates the beginning of the message.
obdSignal_startByte :: Lens.Lens' ObdSignal Prelude.Natural
obdSignal_startByte = Lens.lens (\ObdSignal' {startByte} -> startByte) (\s@ObdSignal' {} a -> s {startByte = a} :: ObdSignal)

-- | The length of a message.
obdSignal_byteLength :: Lens.Lens' ObdSignal Prelude.Natural
obdSignal_byteLength = Lens.lens (\ObdSignal' {byteLength} -> byteLength) (\s@ObdSignal' {} a -> s {byteLength = a} :: ObdSignal)

instance Core.FromJSON ObdSignal where
  parseJSON =
    Core.withObject
      "ObdSignal"
      ( \x ->
          ObdSignal'
            Prelude.<$> (x Core..:? "bitMaskLength")
            Prelude.<*> (x Core..:? "bitRightShift")
            Prelude.<*> (x Core..: "pidResponseLength")
            Prelude.<*> (x Core..: "serviceMode")
            Prelude.<*> (x Core..: "pid")
            Prelude.<*> (x Core..: "scaling")
            Prelude.<*> (x Core..: "offset")
            Prelude.<*> (x Core..: "startByte")
            Prelude.<*> (x Core..: "byteLength")
      )

instance Prelude.Hashable ObdSignal where
  hashWithSalt _salt ObdSignal' {..} =
    _salt `Prelude.hashWithSalt` bitMaskLength
      `Prelude.hashWithSalt` bitRightShift
      `Prelude.hashWithSalt` pidResponseLength
      `Prelude.hashWithSalt` serviceMode
      `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` scaling
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` startByte
      `Prelude.hashWithSalt` byteLength

instance Prelude.NFData ObdSignal where
  rnf ObdSignal' {..} =
    Prelude.rnf bitMaskLength
      `Prelude.seq` Prelude.rnf bitRightShift
      `Prelude.seq` Prelude.rnf pidResponseLength
      `Prelude.seq` Prelude.rnf serviceMode
      `Prelude.seq` Prelude.rnf pid
      `Prelude.seq` Prelude.rnf scaling
      `Prelude.seq` Prelude.rnf offset
      `Prelude.seq` Prelude.rnf startByte
      `Prelude.seq` Prelude.rnf byteLength

instance Core.ToJSON ObdSignal where
  toJSON ObdSignal' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bitMaskLength" Core..=) Prelude.<$> bitMaskLength,
            ("bitRightShift" Core..=) Prelude.<$> bitRightShift,
            Prelude.Just
              ("pidResponseLength" Core..= pidResponseLength),
            Prelude.Just ("serviceMode" Core..= serviceMode),
            Prelude.Just ("pid" Core..= pid),
            Prelude.Just ("scaling" Core..= scaling),
            Prelude.Just ("offset" Core..= offset),
            Prelude.Just ("startByte" Core..= startByte),
            Prelude.Just ("byteLength" Core..= byteLength)
          ]
      )

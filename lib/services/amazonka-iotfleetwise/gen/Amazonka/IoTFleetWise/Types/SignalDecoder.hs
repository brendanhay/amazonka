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
-- Module      : Amazonka.IoTFleetWise.Types.SignalDecoder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.SignalDecoder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types.CanSignal
import Amazonka.IoTFleetWise.Types.ObdSignal
import Amazonka.IoTFleetWise.Types.SignalDecoderType
import qualified Amazonka.Prelude as Prelude

-- | Information about a signal decoder.
--
-- /See:/ 'newSignalDecoder' smart constructor.
data SignalDecoder = SignalDecoder'
  { -- | Information about signal decoder using the Controller Area Network (CAN)
    -- protocol.
    canSignal :: Prelude.Maybe CanSignal,
    -- | Information about signal decoder using the On-board diagnostic (OBD) II
    -- protocol.
    obdSignal :: Prelude.Maybe ObdSignal,
    -- | The fully qualified name of a signal decoder as defined in a vehicle
    -- model.
    fullyQualifiedName :: Prelude.Text,
    -- | The network protocol for the vehicle. For example, @CAN_SIGNAL@
    -- specifies a protocol that defines how data is communicated between
    -- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
    -- defines how self-diagnostic data is communicated between ECUs.
    type' :: SignalDecoderType,
    -- | The ID of a network interface that specifies what network protocol a
    -- vehicle follows.
    interfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalDecoder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canSignal', 'signalDecoder_canSignal' - Information about signal decoder using the Controller Area Network (CAN)
-- protocol.
--
-- 'obdSignal', 'signalDecoder_obdSignal' - Information about signal decoder using the On-board diagnostic (OBD) II
-- protocol.
--
-- 'fullyQualifiedName', 'signalDecoder_fullyQualifiedName' - The fully qualified name of a signal decoder as defined in a vehicle
-- model.
--
-- 'type'', 'signalDecoder_type' - The network protocol for the vehicle. For example, @CAN_SIGNAL@
-- specifies a protocol that defines how data is communicated between
-- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
-- defines how self-diagnostic data is communicated between ECUs.
--
-- 'interfaceId', 'signalDecoder_interfaceId' - The ID of a network interface that specifies what network protocol a
-- vehicle follows.
newSignalDecoder ::
  -- | 'fullyQualifiedName'
  Prelude.Text ->
  -- | 'type''
  SignalDecoderType ->
  -- | 'interfaceId'
  Prelude.Text ->
  SignalDecoder
newSignalDecoder
  pFullyQualifiedName_
  pType_
  pInterfaceId_ =
    SignalDecoder'
      { canSignal = Prelude.Nothing,
        obdSignal = Prelude.Nothing,
        fullyQualifiedName = pFullyQualifiedName_,
        type' = pType_,
        interfaceId = pInterfaceId_
      }

-- | Information about signal decoder using the Controller Area Network (CAN)
-- protocol.
signalDecoder_canSignal :: Lens.Lens' SignalDecoder (Prelude.Maybe CanSignal)
signalDecoder_canSignal = Lens.lens (\SignalDecoder' {canSignal} -> canSignal) (\s@SignalDecoder' {} a -> s {canSignal = a} :: SignalDecoder)

-- | Information about signal decoder using the On-board diagnostic (OBD) II
-- protocol.
signalDecoder_obdSignal :: Lens.Lens' SignalDecoder (Prelude.Maybe ObdSignal)
signalDecoder_obdSignal = Lens.lens (\SignalDecoder' {obdSignal} -> obdSignal) (\s@SignalDecoder' {} a -> s {obdSignal = a} :: SignalDecoder)

-- | The fully qualified name of a signal decoder as defined in a vehicle
-- model.
signalDecoder_fullyQualifiedName :: Lens.Lens' SignalDecoder Prelude.Text
signalDecoder_fullyQualifiedName = Lens.lens (\SignalDecoder' {fullyQualifiedName} -> fullyQualifiedName) (\s@SignalDecoder' {} a -> s {fullyQualifiedName = a} :: SignalDecoder)

-- | The network protocol for the vehicle. For example, @CAN_SIGNAL@
-- specifies a protocol that defines how data is communicated between
-- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
-- defines how self-diagnostic data is communicated between ECUs.
signalDecoder_type :: Lens.Lens' SignalDecoder SignalDecoderType
signalDecoder_type = Lens.lens (\SignalDecoder' {type'} -> type') (\s@SignalDecoder' {} a -> s {type' = a} :: SignalDecoder)

-- | The ID of a network interface that specifies what network protocol a
-- vehicle follows.
signalDecoder_interfaceId :: Lens.Lens' SignalDecoder Prelude.Text
signalDecoder_interfaceId = Lens.lens (\SignalDecoder' {interfaceId} -> interfaceId) (\s@SignalDecoder' {} a -> s {interfaceId = a} :: SignalDecoder)

instance Core.FromJSON SignalDecoder where
  parseJSON =
    Core.withObject
      "SignalDecoder"
      ( \x ->
          SignalDecoder'
            Prelude.<$> (x Core..:? "canSignal")
            Prelude.<*> (x Core..:? "obdSignal")
            Prelude.<*> (x Core..: "fullyQualifiedName")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "interfaceId")
      )

instance Prelude.Hashable SignalDecoder where
  hashWithSalt _salt SignalDecoder' {..} =
    _salt `Prelude.hashWithSalt` canSignal
      `Prelude.hashWithSalt` obdSignal
      `Prelude.hashWithSalt` fullyQualifiedName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` interfaceId

instance Prelude.NFData SignalDecoder where
  rnf SignalDecoder' {..} =
    Prelude.rnf canSignal
      `Prelude.seq` Prelude.rnf obdSignal
      `Prelude.seq` Prelude.rnf fullyQualifiedName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf interfaceId

instance Core.ToJSON SignalDecoder where
  toJSON SignalDecoder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("canSignal" Core..=) Prelude.<$> canSignal,
            ("obdSignal" Core..=) Prelude.<$> obdSignal,
            Prelude.Just
              ("fullyQualifiedName" Core..= fullyQualifiedName),
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("interfaceId" Core..= interfaceId)
          ]
      )

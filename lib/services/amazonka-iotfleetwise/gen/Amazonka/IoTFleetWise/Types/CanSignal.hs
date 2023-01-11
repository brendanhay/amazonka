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
-- Module      : Amazonka.IoTFleetWise.Types.CanSignal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CanSignal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a single controller area network (CAN) signal and the
-- messages it receives and transmits.
--
-- /See:/ 'newCanSignal' smart constructor.
data CanSignal = CanSignal'
  { -- | The name of the signal.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message.
    messageId :: Prelude.Natural,
    -- | Whether the byte ordering of a CAN message is big-endian.
    isBigEndian :: Prelude.Bool,
    -- | Whether the message data is specified as a signed value.
    isSigned :: Prelude.Bool,
    -- | Indicates the beginning of the CAN message.
    startBit :: Prelude.Natural,
    -- | Indicates where data appears in the CAN message.
    offset :: Prelude.Double,
    -- | A multiplier used to decode the CAN message.
    factor :: Prelude.Double,
    -- | How many bytes of data are in the message.
    length :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanSignal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'canSignal_name' - The name of the signal.
--
-- 'messageId', 'canSignal_messageId' - The ID of the message.
--
-- 'isBigEndian', 'canSignal_isBigEndian' - Whether the byte ordering of a CAN message is big-endian.
--
-- 'isSigned', 'canSignal_isSigned' - Whether the message data is specified as a signed value.
--
-- 'startBit', 'canSignal_startBit' - Indicates the beginning of the CAN message.
--
-- 'offset', 'canSignal_offset' - Indicates where data appears in the CAN message.
--
-- 'factor', 'canSignal_factor' - A multiplier used to decode the CAN message.
--
-- 'length', 'canSignal_length' - How many bytes of data are in the message.
newCanSignal ::
  -- | 'messageId'
  Prelude.Natural ->
  -- | 'isBigEndian'
  Prelude.Bool ->
  -- | 'isSigned'
  Prelude.Bool ->
  -- | 'startBit'
  Prelude.Natural ->
  -- | 'offset'
  Prelude.Double ->
  -- | 'factor'
  Prelude.Double ->
  -- | 'length'
  Prelude.Natural ->
  CanSignal
newCanSignal
  pMessageId_
  pIsBigEndian_
  pIsSigned_
  pStartBit_
  pOffset_
  pFactor_
  pLength_ =
    CanSignal'
      { name = Prelude.Nothing,
        messageId = pMessageId_,
        isBigEndian = pIsBigEndian_,
        isSigned = pIsSigned_,
        startBit = pStartBit_,
        offset = pOffset_,
        factor = pFactor_,
        length = pLength_
      }

-- | The name of the signal.
canSignal_name :: Lens.Lens' CanSignal (Prelude.Maybe Prelude.Text)
canSignal_name = Lens.lens (\CanSignal' {name} -> name) (\s@CanSignal' {} a -> s {name = a} :: CanSignal)

-- | The ID of the message.
canSignal_messageId :: Lens.Lens' CanSignal Prelude.Natural
canSignal_messageId = Lens.lens (\CanSignal' {messageId} -> messageId) (\s@CanSignal' {} a -> s {messageId = a} :: CanSignal)

-- | Whether the byte ordering of a CAN message is big-endian.
canSignal_isBigEndian :: Lens.Lens' CanSignal Prelude.Bool
canSignal_isBigEndian = Lens.lens (\CanSignal' {isBigEndian} -> isBigEndian) (\s@CanSignal' {} a -> s {isBigEndian = a} :: CanSignal)

-- | Whether the message data is specified as a signed value.
canSignal_isSigned :: Lens.Lens' CanSignal Prelude.Bool
canSignal_isSigned = Lens.lens (\CanSignal' {isSigned} -> isSigned) (\s@CanSignal' {} a -> s {isSigned = a} :: CanSignal)

-- | Indicates the beginning of the CAN message.
canSignal_startBit :: Lens.Lens' CanSignal Prelude.Natural
canSignal_startBit = Lens.lens (\CanSignal' {startBit} -> startBit) (\s@CanSignal' {} a -> s {startBit = a} :: CanSignal)

-- | Indicates where data appears in the CAN message.
canSignal_offset :: Lens.Lens' CanSignal Prelude.Double
canSignal_offset = Lens.lens (\CanSignal' {offset} -> offset) (\s@CanSignal' {} a -> s {offset = a} :: CanSignal)

-- | A multiplier used to decode the CAN message.
canSignal_factor :: Lens.Lens' CanSignal Prelude.Double
canSignal_factor = Lens.lens (\CanSignal' {factor} -> factor) (\s@CanSignal' {} a -> s {factor = a} :: CanSignal)

-- | How many bytes of data are in the message.
canSignal_length :: Lens.Lens' CanSignal Prelude.Natural
canSignal_length = Lens.lens (\CanSignal' {length} -> length) (\s@CanSignal' {} a -> s {length = a} :: CanSignal)

instance Data.FromJSON CanSignal where
  parseJSON =
    Data.withObject
      "CanSignal"
      ( \x ->
          CanSignal'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..: "messageId")
            Prelude.<*> (x Data..: "isBigEndian")
            Prelude.<*> (x Data..: "isSigned")
            Prelude.<*> (x Data..: "startBit")
            Prelude.<*> (x Data..: "offset")
            Prelude.<*> (x Data..: "factor")
            Prelude.<*> (x Data..: "length")
      )

instance Prelude.Hashable CanSignal where
  hashWithSalt _salt CanSignal' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` isBigEndian
      `Prelude.hashWithSalt` isSigned
      `Prelude.hashWithSalt` startBit
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` factor
      `Prelude.hashWithSalt` length

instance Prelude.NFData CanSignal where
  rnf CanSignal' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf isBigEndian
      `Prelude.seq` Prelude.rnf isSigned
      `Prelude.seq` Prelude.rnf startBit
      `Prelude.seq` Prelude.rnf offset
      `Prelude.seq` Prelude.rnf factor
      `Prelude.seq` Prelude.rnf length

instance Data.ToJSON CanSignal where
  toJSON CanSignal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("messageId" Data..= messageId),
            Prelude.Just ("isBigEndian" Data..= isBigEndian),
            Prelude.Just ("isSigned" Data..= isSigned),
            Prelude.Just ("startBit" Data..= startBit),
            Prelude.Just ("offset" Data..= offset),
            Prelude.Just ("factor" Data..= factor),
            Prelude.Just ("length" Data..= length)
          ]
      )

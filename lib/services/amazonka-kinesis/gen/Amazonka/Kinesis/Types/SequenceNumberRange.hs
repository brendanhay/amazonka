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
-- Module      : Amazonka.Kinesis.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.SequenceNumberRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The range of possible sequence numbers for the shard.
--
-- /See:/ 'newSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The ending sequence number for the range. Shards that are in the OPEN
    -- state have an ending sequence number of @null@.
    endingSequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The starting sequence number for the range.
    startingSequenceNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SequenceNumberRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endingSequenceNumber', 'sequenceNumberRange_endingSequenceNumber' - The ending sequence number for the range. Shards that are in the OPEN
-- state have an ending sequence number of @null@.
--
-- 'startingSequenceNumber', 'sequenceNumberRange_startingSequenceNumber' - The starting sequence number for the range.
newSequenceNumberRange ::
  -- | 'startingSequenceNumber'
  Prelude.Text ->
  SequenceNumberRange
newSequenceNumberRange pStartingSequenceNumber_ =
  SequenceNumberRange'
    { endingSequenceNumber =
        Prelude.Nothing,
      startingSequenceNumber = pStartingSequenceNumber_
    }

-- | The ending sequence number for the range. Shards that are in the OPEN
-- state have an ending sequence number of @null@.
sequenceNumberRange_endingSequenceNumber :: Lens.Lens' SequenceNumberRange (Prelude.Maybe Prelude.Text)
sequenceNumberRange_endingSequenceNumber = Lens.lens (\SequenceNumberRange' {endingSequenceNumber} -> endingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {endingSequenceNumber = a} :: SequenceNumberRange)

-- | The starting sequence number for the range.
sequenceNumberRange_startingSequenceNumber :: Lens.Lens' SequenceNumberRange Prelude.Text
sequenceNumberRange_startingSequenceNumber = Lens.lens (\SequenceNumberRange' {startingSequenceNumber} -> startingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {startingSequenceNumber = a} :: SequenceNumberRange)

instance Data.FromJSON SequenceNumberRange where
  parseJSON =
    Data.withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            Prelude.<$> (x Data..:? "EndingSequenceNumber")
            Prelude.<*> (x Data..: "StartingSequenceNumber")
      )

instance Prelude.Hashable SequenceNumberRange where
  hashWithSalt _salt SequenceNumberRange' {..} =
    _salt
      `Prelude.hashWithSalt` endingSequenceNumber
      `Prelude.hashWithSalt` startingSequenceNumber

instance Prelude.NFData SequenceNumberRange where
  rnf SequenceNumberRange' {..} =
    Prelude.rnf endingSequenceNumber
      `Prelude.seq` Prelude.rnf startingSequenceNumber

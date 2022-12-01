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
-- Module      : Amazonka.DynamoDBStreams.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.SequenceNumberRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

-- | The beginning and ending sequence numbers for the stream records
-- contained within a shard.
--
-- /See:/ 'newSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The last sequence number for the stream records contained within a
    -- shard. String contains numeric characters only.
    endingSequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The first sequence number for the stream records contained within a
    -- shard. String contains numeric characters only.
    startingSequenceNumber :: Prelude.Maybe Prelude.Text
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
-- 'endingSequenceNumber', 'sequenceNumberRange_endingSequenceNumber' - The last sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
--
-- 'startingSequenceNumber', 'sequenceNumberRange_startingSequenceNumber' - The first sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
newSequenceNumberRange ::
  SequenceNumberRange
newSequenceNumberRange =
  SequenceNumberRange'
    { endingSequenceNumber =
        Prelude.Nothing,
      startingSequenceNumber = Prelude.Nothing
    }

-- | The last sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
sequenceNumberRange_endingSequenceNumber :: Lens.Lens' SequenceNumberRange (Prelude.Maybe Prelude.Text)
sequenceNumberRange_endingSequenceNumber = Lens.lens (\SequenceNumberRange' {endingSequenceNumber} -> endingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {endingSequenceNumber = a} :: SequenceNumberRange)

-- | The first sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
sequenceNumberRange_startingSequenceNumber :: Lens.Lens' SequenceNumberRange (Prelude.Maybe Prelude.Text)
sequenceNumberRange_startingSequenceNumber = Lens.lens (\SequenceNumberRange' {startingSequenceNumber} -> startingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {startingSequenceNumber = a} :: SequenceNumberRange)

instance Core.FromJSON SequenceNumberRange where
  parseJSON =
    Core.withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            Prelude.<$> (x Core..:? "EndingSequenceNumber")
            Prelude.<*> (x Core..:? "StartingSequenceNumber")
      )

instance Prelude.Hashable SequenceNumberRange where
  hashWithSalt _salt SequenceNumberRange' {..} =
    _salt `Prelude.hashWithSalt` endingSequenceNumber
      `Prelude.hashWithSalt` startingSequenceNumber

instance Prelude.NFData SequenceNumberRange where
  rnf SequenceNumberRange' {..} =
    Prelude.rnf endingSequenceNumber
      `Prelude.seq` Prelude.rnf startingSequenceNumber

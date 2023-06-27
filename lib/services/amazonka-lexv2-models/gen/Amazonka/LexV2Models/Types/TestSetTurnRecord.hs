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
-- Module      : Amazonka.LexV2Models.Types.TestSetTurnRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetTurnRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TurnSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a turn in a test set.
--
-- /See:/ 'newTestSetTurnRecord' smart constructor.
data TestSetTurnRecord = TestSetTurnRecord'
  { -- | The unique identifier for the conversation associated with the turn.
    conversationId :: Prelude.Maybe Prelude.Text,
    -- | The number of turns that has elapsed up to that turn.
    turnNumber :: Prelude.Maybe Prelude.Natural,
    -- | The record number associated with the turn.
    recordNumber :: Prelude.Natural,
    -- | Contains information about the agent or user turn depending upon type of
    -- turn.
    turnSpecification :: TurnSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetTurnRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationId', 'testSetTurnRecord_conversationId' - The unique identifier for the conversation associated with the turn.
--
-- 'turnNumber', 'testSetTurnRecord_turnNumber' - The number of turns that has elapsed up to that turn.
--
-- 'recordNumber', 'testSetTurnRecord_recordNumber' - The record number associated with the turn.
--
-- 'turnSpecification', 'testSetTurnRecord_turnSpecification' - Contains information about the agent or user turn depending upon type of
-- turn.
newTestSetTurnRecord ::
  -- | 'recordNumber'
  Prelude.Natural ->
  -- | 'turnSpecification'
  TurnSpecification ->
  TestSetTurnRecord
newTestSetTurnRecord
  pRecordNumber_
  pTurnSpecification_ =
    TestSetTurnRecord'
      { conversationId =
          Prelude.Nothing,
        turnNumber = Prelude.Nothing,
        recordNumber = pRecordNumber_,
        turnSpecification = pTurnSpecification_
      }

-- | The unique identifier for the conversation associated with the turn.
testSetTurnRecord_conversationId :: Lens.Lens' TestSetTurnRecord (Prelude.Maybe Prelude.Text)
testSetTurnRecord_conversationId = Lens.lens (\TestSetTurnRecord' {conversationId} -> conversationId) (\s@TestSetTurnRecord' {} a -> s {conversationId = a} :: TestSetTurnRecord)

-- | The number of turns that has elapsed up to that turn.
testSetTurnRecord_turnNumber :: Lens.Lens' TestSetTurnRecord (Prelude.Maybe Prelude.Natural)
testSetTurnRecord_turnNumber = Lens.lens (\TestSetTurnRecord' {turnNumber} -> turnNumber) (\s@TestSetTurnRecord' {} a -> s {turnNumber = a} :: TestSetTurnRecord)

-- | The record number associated with the turn.
testSetTurnRecord_recordNumber :: Lens.Lens' TestSetTurnRecord Prelude.Natural
testSetTurnRecord_recordNumber = Lens.lens (\TestSetTurnRecord' {recordNumber} -> recordNumber) (\s@TestSetTurnRecord' {} a -> s {recordNumber = a} :: TestSetTurnRecord)

-- | Contains information about the agent or user turn depending upon type of
-- turn.
testSetTurnRecord_turnSpecification :: Lens.Lens' TestSetTurnRecord TurnSpecification
testSetTurnRecord_turnSpecification = Lens.lens (\TestSetTurnRecord' {turnSpecification} -> turnSpecification) (\s@TestSetTurnRecord' {} a -> s {turnSpecification = a} :: TestSetTurnRecord)

instance Data.FromJSON TestSetTurnRecord where
  parseJSON =
    Data.withObject
      "TestSetTurnRecord"
      ( \x ->
          TestSetTurnRecord'
            Prelude.<$> (x Data..:? "conversationId")
            Prelude.<*> (x Data..:? "turnNumber")
            Prelude.<*> (x Data..: "recordNumber")
            Prelude.<*> (x Data..: "turnSpecification")
      )

instance Prelude.Hashable TestSetTurnRecord where
  hashWithSalt _salt TestSetTurnRecord' {..} =
    _salt
      `Prelude.hashWithSalt` conversationId
      `Prelude.hashWithSalt` turnNumber
      `Prelude.hashWithSalt` recordNumber
      `Prelude.hashWithSalt` turnSpecification

instance Prelude.NFData TestSetTurnRecord where
  rnf TestSetTurnRecord' {..} =
    Prelude.rnf conversationId
      `Prelude.seq` Prelude.rnf turnNumber
      `Prelude.seq` Prelude.rnf recordNumber
      `Prelude.seq` Prelude.rnf turnSpecification

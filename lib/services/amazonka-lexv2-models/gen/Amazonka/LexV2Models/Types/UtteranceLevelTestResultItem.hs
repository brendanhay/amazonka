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
-- Module      : Amazonka.LexV2Models.Types.UtteranceLevelTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UtteranceLevelTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestSetTurnResult
import qualified Amazonka.Prelude as Prelude

-- | Contains information about multiple utterances in the results of a test
-- set execution.
--
-- /See:/ 'newUtteranceLevelTestResultItem' smart constructor.
data UtteranceLevelTestResultItem = UtteranceLevelTestResultItem'
  { -- | The unique identifier for the conversation associated with the result.
    conversationId :: Prelude.Maybe Prelude.Text,
    -- | The record number of the result.
    recordNumber :: Prelude.Natural,
    -- | Contains information about the turn associated with the result.
    turnResult :: TestSetTurnResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceLevelTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationId', 'utteranceLevelTestResultItem_conversationId' - The unique identifier for the conversation associated with the result.
--
-- 'recordNumber', 'utteranceLevelTestResultItem_recordNumber' - The record number of the result.
--
-- 'turnResult', 'utteranceLevelTestResultItem_turnResult' - Contains information about the turn associated with the result.
newUtteranceLevelTestResultItem ::
  -- | 'recordNumber'
  Prelude.Natural ->
  -- | 'turnResult'
  TestSetTurnResult ->
  UtteranceLevelTestResultItem
newUtteranceLevelTestResultItem
  pRecordNumber_
  pTurnResult_ =
    UtteranceLevelTestResultItem'
      { conversationId =
          Prelude.Nothing,
        recordNumber = pRecordNumber_,
        turnResult = pTurnResult_
      }

-- | The unique identifier for the conversation associated with the result.
utteranceLevelTestResultItem_conversationId :: Lens.Lens' UtteranceLevelTestResultItem (Prelude.Maybe Prelude.Text)
utteranceLevelTestResultItem_conversationId = Lens.lens (\UtteranceLevelTestResultItem' {conversationId} -> conversationId) (\s@UtteranceLevelTestResultItem' {} a -> s {conversationId = a} :: UtteranceLevelTestResultItem)

-- | The record number of the result.
utteranceLevelTestResultItem_recordNumber :: Lens.Lens' UtteranceLevelTestResultItem Prelude.Natural
utteranceLevelTestResultItem_recordNumber = Lens.lens (\UtteranceLevelTestResultItem' {recordNumber} -> recordNumber) (\s@UtteranceLevelTestResultItem' {} a -> s {recordNumber = a} :: UtteranceLevelTestResultItem)

-- | Contains information about the turn associated with the result.
utteranceLevelTestResultItem_turnResult :: Lens.Lens' UtteranceLevelTestResultItem TestSetTurnResult
utteranceLevelTestResultItem_turnResult = Lens.lens (\UtteranceLevelTestResultItem' {turnResult} -> turnResult) (\s@UtteranceLevelTestResultItem' {} a -> s {turnResult = a} :: UtteranceLevelTestResultItem)

instance Data.FromJSON UtteranceLevelTestResultItem where
  parseJSON =
    Data.withObject
      "UtteranceLevelTestResultItem"
      ( \x ->
          UtteranceLevelTestResultItem'
            Prelude.<$> (x Data..:? "conversationId")
            Prelude.<*> (x Data..: "recordNumber")
            Prelude.<*> (x Data..: "turnResult")
      )

instance
  Prelude.Hashable
    UtteranceLevelTestResultItem
  where
  hashWithSalt _salt UtteranceLevelTestResultItem' {..} =
    _salt
      `Prelude.hashWithSalt` conversationId
      `Prelude.hashWithSalt` recordNumber
      `Prelude.hashWithSalt` turnResult

instance Prelude.NFData UtteranceLevelTestResultItem where
  rnf UtteranceLevelTestResultItem' {..} =
    Prelude.rnf conversationId
      `Prelude.seq` Prelude.rnf recordNumber
      `Prelude.seq` Prelude.rnf turnResult

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
-- Module      : Amazonka.LexV2Models.Types.ConversationLogsDataSourceFilterBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLogsDataSourceFilterBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLogsInputModeFilter
import qualified Amazonka.Prelude as Prelude

-- | The selected data source to filter the conversation log.
--
-- /See:/ 'newConversationLogsDataSourceFilterBy' smart constructor.
data ConversationLogsDataSourceFilterBy = ConversationLogsDataSourceFilterBy'
  { -- | The start time for the conversation log.
    startTime :: Data.POSIX,
    -- | The end time for the conversation log.
    endTime :: Data.POSIX,
    -- | The selection to filter by input mode for the conversation logs.
    inputMode :: ConversationLogsInputModeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLogsDataSourceFilterBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'conversationLogsDataSourceFilterBy_startTime' - The start time for the conversation log.
--
-- 'endTime', 'conversationLogsDataSourceFilterBy_endTime' - The end time for the conversation log.
--
-- 'inputMode', 'conversationLogsDataSourceFilterBy_inputMode' - The selection to filter by input mode for the conversation logs.
newConversationLogsDataSourceFilterBy ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'inputMode'
  ConversationLogsInputModeFilter ->
  ConversationLogsDataSourceFilterBy
newConversationLogsDataSourceFilterBy
  pStartTime_
  pEndTime_
  pInputMode_ =
    ConversationLogsDataSourceFilterBy'
      { startTime =
          Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        inputMode = pInputMode_
      }

-- | The start time for the conversation log.
conversationLogsDataSourceFilterBy_startTime :: Lens.Lens' ConversationLogsDataSourceFilterBy Prelude.UTCTime
conversationLogsDataSourceFilterBy_startTime = Lens.lens (\ConversationLogsDataSourceFilterBy' {startTime} -> startTime) (\s@ConversationLogsDataSourceFilterBy' {} a -> s {startTime = a} :: ConversationLogsDataSourceFilterBy) Prelude.. Data._Time

-- | The end time for the conversation log.
conversationLogsDataSourceFilterBy_endTime :: Lens.Lens' ConversationLogsDataSourceFilterBy Prelude.UTCTime
conversationLogsDataSourceFilterBy_endTime = Lens.lens (\ConversationLogsDataSourceFilterBy' {endTime} -> endTime) (\s@ConversationLogsDataSourceFilterBy' {} a -> s {endTime = a} :: ConversationLogsDataSourceFilterBy) Prelude.. Data._Time

-- | The selection to filter by input mode for the conversation logs.
conversationLogsDataSourceFilterBy_inputMode :: Lens.Lens' ConversationLogsDataSourceFilterBy ConversationLogsInputModeFilter
conversationLogsDataSourceFilterBy_inputMode = Lens.lens (\ConversationLogsDataSourceFilterBy' {inputMode} -> inputMode) (\s@ConversationLogsDataSourceFilterBy' {} a -> s {inputMode = a} :: ConversationLogsDataSourceFilterBy)

instance
  Data.FromJSON
    ConversationLogsDataSourceFilterBy
  where
  parseJSON =
    Data.withObject
      "ConversationLogsDataSourceFilterBy"
      ( \x ->
          ConversationLogsDataSourceFilterBy'
            Prelude.<$> (x Data..: "startTime")
            Prelude.<*> (x Data..: "endTime")
            Prelude.<*> (x Data..: "inputMode")
      )

instance
  Prelude.Hashable
    ConversationLogsDataSourceFilterBy
  where
  hashWithSalt
    _salt
    ConversationLogsDataSourceFilterBy' {..} =
      _salt
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputMode

instance
  Prelude.NFData
    ConversationLogsDataSourceFilterBy
  where
  rnf ConversationLogsDataSourceFilterBy' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputMode

instance
  Data.ToJSON
    ConversationLogsDataSourceFilterBy
  where
  toJSON ConversationLogsDataSourceFilterBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("inputMode" Data..= inputMode)
          ]
      )

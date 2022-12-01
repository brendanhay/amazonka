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
-- Module      : Amazonka.LexModels.Types.UtteranceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.UtteranceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a single utterance that was made to your bot.
--
-- /See:/ 'newUtteranceData' smart constructor.
data UtteranceData = UtteranceData'
  { -- | The date that the utterance was first recorded.
    firstUtteredDate :: Prelude.Maybe Core.POSIX,
    -- | The number of times that the utterance was processed.
    count :: Prelude.Maybe Prelude.Int,
    -- | The text that was entered by the user or the text representation of an
    -- audio clip.
    utteranceString :: Prelude.Maybe Prelude.Text,
    -- | The total number of individuals that used the utterance.
    distinctUsers :: Prelude.Maybe Prelude.Int,
    -- | The date that the utterance was last recorded.
    lastUtteredDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstUtteredDate', 'utteranceData_firstUtteredDate' - The date that the utterance was first recorded.
--
-- 'count', 'utteranceData_count' - The number of times that the utterance was processed.
--
-- 'utteranceString', 'utteranceData_utteranceString' - The text that was entered by the user or the text representation of an
-- audio clip.
--
-- 'distinctUsers', 'utteranceData_distinctUsers' - The total number of individuals that used the utterance.
--
-- 'lastUtteredDate', 'utteranceData_lastUtteredDate' - The date that the utterance was last recorded.
newUtteranceData ::
  UtteranceData
newUtteranceData =
  UtteranceData'
    { firstUtteredDate = Prelude.Nothing,
      count = Prelude.Nothing,
      utteranceString = Prelude.Nothing,
      distinctUsers = Prelude.Nothing,
      lastUtteredDate = Prelude.Nothing
    }

-- | The date that the utterance was first recorded.
utteranceData_firstUtteredDate :: Lens.Lens' UtteranceData (Prelude.Maybe Prelude.UTCTime)
utteranceData_firstUtteredDate = Lens.lens (\UtteranceData' {firstUtteredDate} -> firstUtteredDate) (\s@UtteranceData' {} a -> s {firstUtteredDate = a} :: UtteranceData) Prelude.. Lens.mapping Core._Time

-- | The number of times that the utterance was processed.
utteranceData_count :: Lens.Lens' UtteranceData (Prelude.Maybe Prelude.Int)
utteranceData_count = Lens.lens (\UtteranceData' {count} -> count) (\s@UtteranceData' {} a -> s {count = a} :: UtteranceData)

-- | The text that was entered by the user or the text representation of an
-- audio clip.
utteranceData_utteranceString :: Lens.Lens' UtteranceData (Prelude.Maybe Prelude.Text)
utteranceData_utteranceString = Lens.lens (\UtteranceData' {utteranceString} -> utteranceString) (\s@UtteranceData' {} a -> s {utteranceString = a} :: UtteranceData)

-- | The total number of individuals that used the utterance.
utteranceData_distinctUsers :: Lens.Lens' UtteranceData (Prelude.Maybe Prelude.Int)
utteranceData_distinctUsers = Lens.lens (\UtteranceData' {distinctUsers} -> distinctUsers) (\s@UtteranceData' {} a -> s {distinctUsers = a} :: UtteranceData)

-- | The date that the utterance was last recorded.
utteranceData_lastUtteredDate :: Lens.Lens' UtteranceData (Prelude.Maybe Prelude.UTCTime)
utteranceData_lastUtteredDate = Lens.lens (\UtteranceData' {lastUtteredDate} -> lastUtteredDate) (\s@UtteranceData' {} a -> s {lastUtteredDate = a} :: UtteranceData) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON UtteranceData where
  parseJSON =
    Core.withObject
      "UtteranceData"
      ( \x ->
          UtteranceData'
            Prelude.<$> (x Core..:? "firstUtteredDate")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "utteranceString")
            Prelude.<*> (x Core..:? "distinctUsers")
            Prelude.<*> (x Core..:? "lastUtteredDate")
      )

instance Prelude.Hashable UtteranceData where
  hashWithSalt _salt UtteranceData' {..} =
    _salt `Prelude.hashWithSalt` firstUtteredDate
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` utteranceString
      `Prelude.hashWithSalt` distinctUsers
      `Prelude.hashWithSalt` lastUtteredDate

instance Prelude.NFData UtteranceData where
  rnf UtteranceData' {..} =
    Prelude.rnf firstUtteredDate
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf utteranceString
      `Prelude.seq` Prelude.rnf distinctUsers
      `Prelude.seq` Prelude.rnf lastUtteredDate

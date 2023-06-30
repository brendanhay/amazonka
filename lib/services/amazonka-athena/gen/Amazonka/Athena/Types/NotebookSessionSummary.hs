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
-- Module      : Amazonka.Athena.Types.NotebookSessionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.NotebookSessionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the notebook session ID and notebook session creation time.
--
-- /See:/ 'newNotebookSessionSummary' smart constructor.
data NotebookSessionSummary = NotebookSessionSummary'
  { -- | The time when the notebook session was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The notebook session ID.
    sessionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotebookSessionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'notebookSessionSummary_creationTime' - The time when the notebook session was created.
--
-- 'sessionId', 'notebookSessionSummary_sessionId' - The notebook session ID.
newNotebookSessionSummary ::
  NotebookSessionSummary
newNotebookSessionSummary =
  NotebookSessionSummary'
    { creationTime =
        Prelude.Nothing,
      sessionId = Prelude.Nothing
    }

-- | The time when the notebook session was created.
notebookSessionSummary_creationTime :: Lens.Lens' NotebookSessionSummary (Prelude.Maybe Prelude.UTCTime)
notebookSessionSummary_creationTime = Lens.lens (\NotebookSessionSummary' {creationTime} -> creationTime) (\s@NotebookSessionSummary' {} a -> s {creationTime = a} :: NotebookSessionSummary) Prelude.. Lens.mapping Data._Time

-- | The notebook session ID.
notebookSessionSummary_sessionId :: Lens.Lens' NotebookSessionSummary (Prelude.Maybe Prelude.Text)
notebookSessionSummary_sessionId = Lens.lens (\NotebookSessionSummary' {sessionId} -> sessionId) (\s@NotebookSessionSummary' {} a -> s {sessionId = a} :: NotebookSessionSummary)

instance Data.FromJSON NotebookSessionSummary where
  parseJSON =
    Data.withObject
      "NotebookSessionSummary"
      ( \x ->
          NotebookSessionSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "SessionId")
      )

instance Prelude.Hashable NotebookSessionSummary where
  hashWithSalt _salt NotebookSessionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData NotebookSessionSummary where
  rnf NotebookSessionSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sessionId

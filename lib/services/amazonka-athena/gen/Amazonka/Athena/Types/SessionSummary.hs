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
-- Module      : Amazonka.Athena.Types.SessionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.SessionSummary where

import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.SessionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a notebook session.
--
-- /See:/ 'newSessionSummary' smart constructor.
data SessionSummary = SessionSummary'
  { -- | The session description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The engine version used by the session (for example,
    -- @PySpark engine version 3@).
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | The notebook version.
    notebookVersion :: Prelude.Maybe Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the session status.
    status :: Prelude.Maybe SessionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'sessionSummary_description' - The session description.
--
-- 'engineVersion', 'sessionSummary_engineVersion' - The engine version used by the session (for example,
-- @PySpark engine version 3@).
--
-- 'notebookVersion', 'sessionSummary_notebookVersion' - The notebook version.
--
-- 'sessionId', 'sessionSummary_sessionId' - The session ID.
--
-- 'status', 'sessionSummary_status' - Contains information about the session status.
newSessionSummary ::
  SessionSummary
newSessionSummary =
  SessionSummary'
    { description = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      notebookVersion = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The session description.
sessionSummary_description :: Lens.Lens' SessionSummary (Prelude.Maybe Prelude.Text)
sessionSummary_description = Lens.lens (\SessionSummary' {description} -> description) (\s@SessionSummary' {} a -> s {description = a} :: SessionSummary)

-- | The engine version used by the session (for example,
-- @PySpark engine version 3@).
sessionSummary_engineVersion :: Lens.Lens' SessionSummary (Prelude.Maybe EngineVersion)
sessionSummary_engineVersion = Lens.lens (\SessionSummary' {engineVersion} -> engineVersion) (\s@SessionSummary' {} a -> s {engineVersion = a} :: SessionSummary)

-- | The notebook version.
sessionSummary_notebookVersion :: Lens.Lens' SessionSummary (Prelude.Maybe Prelude.Text)
sessionSummary_notebookVersion = Lens.lens (\SessionSummary' {notebookVersion} -> notebookVersion) (\s@SessionSummary' {} a -> s {notebookVersion = a} :: SessionSummary)

-- | The session ID.
sessionSummary_sessionId :: Lens.Lens' SessionSummary (Prelude.Maybe Prelude.Text)
sessionSummary_sessionId = Lens.lens (\SessionSummary' {sessionId} -> sessionId) (\s@SessionSummary' {} a -> s {sessionId = a} :: SessionSummary)

-- | Contains information about the session status.
sessionSummary_status :: Lens.Lens' SessionSummary (Prelude.Maybe SessionStatus)
sessionSummary_status = Lens.lens (\SessionSummary' {status} -> status) (\s@SessionSummary' {} a -> s {status = a} :: SessionSummary)

instance Data.FromJSON SessionSummary where
  parseJSON =
    Data.withObject
      "SessionSummary"
      ( \x ->
          SessionSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "NotebookVersion")
            Prelude.<*> (x Data..:? "SessionId")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable SessionSummary where
  hashWithSalt _salt SessionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` notebookVersion
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` status

instance Prelude.NFData SessionSummary where
  rnf SessionSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf notebookVersion
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf status

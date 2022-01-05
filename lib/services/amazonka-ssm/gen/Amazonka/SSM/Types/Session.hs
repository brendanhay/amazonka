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
-- Module      : Amazonka.SSM.Types.Session
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.SessionManagerOutputUrl
import Amazonka.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to an instance.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The status of the session. For example, \"Connected\" or \"Terminated\".
    status :: Prelude.Maybe SessionStatus,
    -- | Reserved for future use.
    outputUrl :: Prelude.Maybe SessionManagerOutputUrl,
    -- | The name of the Session Manager SSM document used to define the
    -- parameters and plugin settings for the session. For example,
    -- @SSM-SessionManagerRunShell@.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session was
    -- terminated.
    endDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the Amazon Web Services user account that started the session.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | Reserved for future use.
    details :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The instance that the Session Manager session connected to.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'session_status' - The status of the session. For example, \"Connected\" or \"Terminated\".
--
-- 'outputUrl', 'session_outputUrl' - Reserved for future use.
--
-- 'documentName', 'session_documentName' - The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
--
-- 'endDate', 'session_endDate' - The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
--
-- 'owner', 'session_owner' - The ID of the Amazon Web Services user account that started the session.
--
-- 'startDate', 'session_startDate' - The date and time, in ISO-8601 Extended format, when the session began.
--
-- 'details', 'session_details' - Reserved for future use.
--
-- 'sessionId', 'session_sessionId' - The ID of the session.
--
-- 'target', 'session_target' - The instance that the Session Manager session connected to.
newSession ::
  Session
newSession =
  Session'
    { status = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      documentName = Prelude.Nothing,
      endDate = Prelude.Nothing,
      owner = Prelude.Nothing,
      startDate = Prelude.Nothing,
      details = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The status of the session. For example, \"Connected\" or \"Terminated\".
session_status :: Lens.Lens' Session (Prelude.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

-- | Reserved for future use.
session_outputUrl :: Lens.Lens' Session (Prelude.Maybe SessionManagerOutputUrl)
session_outputUrl = Lens.lens (\Session' {outputUrl} -> outputUrl) (\s@Session' {} a -> s {outputUrl = a} :: Session)

-- | The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
session_documentName :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_documentName = Lens.lens (\Session' {documentName} -> documentName) (\s@Session' {} a -> s {documentName = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
session_endDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_endDate = Lens.lens (\Session' {endDate} -> endDate) (\s@Session' {} a -> s {endDate = a} :: Session) Prelude.. Lens.mapping Core._Time

-- | The ID of the Amazon Web Services user account that started the session.
session_owner :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_owner = Lens.lens (\Session' {owner} -> owner) (\s@Session' {} a -> s {owner = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session began.
session_startDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startDate = Lens.lens (\Session' {startDate} -> startDate) (\s@Session' {} a -> s {startDate = a} :: Session) Prelude.. Lens.mapping Core._Time

-- | Reserved for future use.
session_details :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_details = Lens.lens (\Session' {details} -> details) (\s@Session' {} a -> s {details = a} :: Session)

-- | The ID of the session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | The instance that the Session Manager session connected to.
session_target :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_target = Lens.lens (\Session' {target} -> target) (\s@Session' {} a -> s {target = a} :: Session)

instance Core.FromJSON Session where
  parseJSON =
    Core.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OutputUrl")
            Prelude.<*> (x Core..:? "DocumentName")
            Prelude.<*> (x Core..:? "EndDate")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "StartDate")
            Prelude.<*> (x Core..:? "Details")
            Prelude.<*> (x Core..:? "SessionId")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` outputUrl
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` target

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf outputUrl
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf target

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.SessionManagerOutputUrl
import Amazonka.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to a managed node.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | Reserved for future use.
    details :: Prelude.Maybe Prelude.Text,
    -- | The name of the Session Manager SSM document used to define the
    -- parameters and plugin settings for the session. For example,
    -- @SSM-SessionManagerRunShell@.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session was
    -- terminated.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | The maximum duration of a session before it terminates.
    maxSessionDuration :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    outputUrl :: Prelude.Maybe SessionManagerOutputUrl,
    -- | The ID of the Amazon Web Services user that started the session.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The reason for connecting to the instance.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | The status of the session. For example, \"Connected\" or \"Terminated\".
    status :: Prelude.Maybe SessionStatus,
    -- | The managed node that the Session Manager session connected to.
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
-- 'details', 'session_details' - Reserved for future use.
--
-- 'documentName', 'session_documentName' - The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
--
-- 'endDate', 'session_endDate' - The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
--
-- 'maxSessionDuration', 'session_maxSessionDuration' - The maximum duration of a session before it terminates.
--
-- 'outputUrl', 'session_outputUrl' - Reserved for future use.
--
-- 'owner', 'session_owner' - The ID of the Amazon Web Services user that started the session.
--
-- 'reason', 'session_reason' - The reason for connecting to the instance.
--
-- 'sessionId', 'session_sessionId' - The ID of the session.
--
-- 'startDate', 'session_startDate' - The date and time, in ISO-8601 Extended format, when the session began.
--
-- 'status', 'session_status' - The status of the session. For example, \"Connected\" or \"Terminated\".
--
-- 'target', 'session_target' - The managed node that the Session Manager session connected to.
newSession ::
  Session
newSession =
  Session'
    { details = Prelude.Nothing,
      documentName = Prelude.Nothing,
      endDate = Prelude.Nothing,
      maxSessionDuration = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      owner = Prelude.Nothing,
      reason = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      status = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | Reserved for future use.
session_details :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_details = Lens.lens (\Session' {details} -> details) (\s@Session' {} a -> s {details = a} :: Session)

-- | The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
session_documentName :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_documentName = Lens.lens (\Session' {documentName} -> documentName) (\s@Session' {} a -> s {documentName = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
session_endDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_endDate = Lens.lens (\Session' {endDate} -> endDate) (\s@Session' {} a -> s {endDate = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | The maximum duration of a session before it terminates.
session_maxSessionDuration :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_maxSessionDuration = Lens.lens (\Session' {maxSessionDuration} -> maxSessionDuration) (\s@Session' {} a -> s {maxSessionDuration = a} :: Session)

-- | Reserved for future use.
session_outputUrl :: Lens.Lens' Session (Prelude.Maybe SessionManagerOutputUrl)
session_outputUrl = Lens.lens (\Session' {outputUrl} -> outputUrl) (\s@Session' {} a -> s {outputUrl = a} :: Session)

-- | The ID of the Amazon Web Services user that started the session.
session_owner :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_owner = Lens.lens (\Session' {owner} -> owner) (\s@Session' {} a -> s {owner = a} :: Session)

-- | The reason for connecting to the instance.
session_reason :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_reason = Lens.lens (\Session' {reason} -> reason) (\s@Session' {} a -> s {reason = a} :: Session)

-- | The ID of the session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session began.
session_startDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startDate = Lens.lens (\Session' {startDate} -> startDate) (\s@Session' {} a -> s {startDate = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | The status of the session. For example, \"Connected\" or \"Terminated\".
session_status :: Lens.Lens' Session (Prelude.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

-- | The managed node that the Session Manager session connected to.
session_target :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_target = Lens.lens (\Session' {target} -> target) (\s@Session' {} a -> s {target = a} :: Session)

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "Details")
            Prelude.<*> (x Data..:? "DocumentName")
            Prelude.<*> (x Data..:? "EndDate")
            Prelude.<*> (x Data..:? "MaxSessionDuration")
            Prelude.<*> (x Data..:? "OutputUrl")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "SessionId")
            Prelude.<*> (x Data..:? "StartDate")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Target")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` maxSessionDuration
      `Prelude.hashWithSalt` outputUrl
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` target

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf maxSessionDuration
      `Prelude.seq` Prelude.rnf outputUrl
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf target

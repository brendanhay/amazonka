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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The date and time, in ISO-8601 Extended format, when the session was
    -- terminated.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | Reserved for future use.
    outputUrl :: Prelude.Maybe SessionManagerOutputUrl,
    -- | The ID of the Amazon Web Services user account that started the session.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The status of the session. For example, \"Connected\" or \"Terminated\".
    status :: Prelude.Maybe SessionStatus,
    -- | The managed node that the Session Manager session connected to.
    target :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    details :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the Session Manager SSM document used to define the
    -- parameters and plugin settings for the session. For example,
    -- @SSM-SessionManagerRunShell@.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The reason for connecting to the instance.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration of a session before it terminates.
    maxSessionDuration :: Prelude.Maybe Prelude.Text
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
-- 'endDate', 'session_endDate' - The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
--
-- 'outputUrl', 'session_outputUrl' - Reserved for future use.
--
-- 'owner', 'session_owner' - The ID of the Amazon Web Services user account that started the session.
--
-- 'status', 'session_status' - The status of the session. For example, \"Connected\" or \"Terminated\".
--
-- 'target', 'session_target' - The managed node that the Session Manager session connected to.
--
-- 'details', 'session_details' - Reserved for future use.
--
-- 'startDate', 'session_startDate' - The date and time, in ISO-8601 Extended format, when the session began.
--
-- 'documentName', 'session_documentName' - The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
--
-- 'sessionId', 'session_sessionId' - The ID of the session.
--
-- 'reason', 'session_reason' - The reason for connecting to the instance.
--
-- 'maxSessionDuration', 'session_maxSessionDuration' - The maximum duration of a session before it terminates.
newSession ::
  Session
newSession =
  Session'
    { endDate = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      owner = Prelude.Nothing,
      status = Prelude.Nothing,
      target = Prelude.Nothing,
      details = Prelude.Nothing,
      startDate = Prelude.Nothing,
      documentName = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      reason = Prelude.Nothing,
      maxSessionDuration = Prelude.Nothing
    }

-- | The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
session_endDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_endDate = Lens.lens (\Session' {endDate} -> endDate) (\s@Session' {} a -> s {endDate = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | Reserved for future use.
session_outputUrl :: Lens.Lens' Session (Prelude.Maybe SessionManagerOutputUrl)
session_outputUrl = Lens.lens (\Session' {outputUrl} -> outputUrl) (\s@Session' {} a -> s {outputUrl = a} :: Session)

-- | The ID of the Amazon Web Services user account that started the session.
session_owner :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_owner = Lens.lens (\Session' {owner} -> owner) (\s@Session' {} a -> s {owner = a} :: Session)

-- | The status of the session. For example, \"Connected\" or \"Terminated\".
session_status :: Lens.Lens' Session (Prelude.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

-- | The managed node that the Session Manager session connected to.
session_target :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_target = Lens.lens (\Session' {target} -> target) (\s@Session' {} a -> s {target = a} :: Session)

-- | Reserved for future use.
session_details :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_details = Lens.lens (\Session' {details} -> details) (\s@Session' {} a -> s {details = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session began.
session_startDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startDate = Lens.lens (\Session' {startDate} -> startDate) (\s@Session' {} a -> s {startDate = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
session_documentName :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_documentName = Lens.lens (\Session' {documentName} -> documentName) (\s@Session' {} a -> s {documentName = a} :: Session)

-- | The ID of the session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | The reason for connecting to the instance.
session_reason :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_reason = Lens.lens (\Session' {reason} -> reason) (\s@Session' {} a -> s {reason = a} :: Session)

-- | The maximum duration of a session before it terminates.
session_maxSessionDuration :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_maxSessionDuration = Lens.lens (\Session' {maxSessionDuration} -> maxSessionDuration) (\s@Session' {} a -> s {maxSessionDuration = a} :: Session)

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "EndDate")
            Prelude.<*> (x Data..:? "OutputUrl")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Target")
            Prelude.<*> (x Data..:? "Details")
            Prelude.<*> (x Data..:? "StartDate")
            Prelude.<*> (x Data..:? "DocumentName")
            Prelude.<*> (x Data..:? "SessionId")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "MaxSessionDuration")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` outputUrl
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` maxSessionDuration

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf outputUrl
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf maxSessionDuration

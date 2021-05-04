{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.Session
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Session where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.SessionManagerOutputUrl
import Network.AWS.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to an instance.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The status of the session. For example, \"Connected\" or \"Terminated\".
    status :: Prelude.Maybe SessionStatus,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Session Manager SSM document used to define the
    -- parameters and plugin settings for the session. For example,
    -- @SSM-SessionManagerRunShell@.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    details :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    outputUrl :: Prelude.Maybe SessionManagerOutputUrl,
    -- | The instance that the Session Manager session connected to.
    target :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS user account that started the session.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session was
    -- terminated.
    endDate :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'startDate', 'session_startDate' - The date and time, in ISO-8601 Extended format, when the session began.
--
-- 'sessionId', 'session_sessionId' - The ID of the session.
--
-- 'documentName', 'session_documentName' - The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
--
-- 'details', 'session_details' - Reserved for future use.
--
-- 'outputUrl', 'session_outputUrl' - Reserved for future use.
--
-- 'target', 'session_target' - The instance that the Session Manager session connected to.
--
-- 'owner', 'session_owner' - The ID of the AWS user account that started the session.
--
-- 'endDate', 'session_endDate' - The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
newSession ::
  Session
newSession =
  Session'
    { status = Prelude.Nothing,
      startDate = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      documentName = Prelude.Nothing,
      details = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      target = Prelude.Nothing,
      owner = Prelude.Nothing,
      endDate = Prelude.Nothing
    }

-- | The status of the session. For example, \"Connected\" or \"Terminated\".
session_status :: Lens.Lens' Session (Prelude.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session began.
session_startDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startDate = Lens.lens (\Session' {startDate} -> startDate) (\s@Session' {} a -> s {startDate = a} :: Session) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
session_documentName :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_documentName = Lens.lens (\Session' {documentName} -> documentName) (\s@Session' {} a -> s {documentName = a} :: Session)

-- | Reserved for future use.
session_details :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_details = Lens.lens (\Session' {details} -> details) (\s@Session' {} a -> s {details = a} :: Session)

-- | Reserved for future use.
session_outputUrl :: Lens.Lens' Session (Prelude.Maybe SessionManagerOutputUrl)
session_outputUrl = Lens.lens (\Session' {outputUrl} -> outputUrl) (\s@Session' {} a -> s {outputUrl = a} :: Session)

-- | The instance that the Session Manager session connected to.
session_target :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_target = Lens.lens (\Session' {target} -> target) (\s@Session' {} a -> s {target = a} :: Session)

-- | The ID of the AWS user account that started the session.
session_owner :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_owner = Lens.lens (\Session' {owner} -> owner) (\s@Session' {} a -> s {owner = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
session_endDate :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_endDate = Lens.lens (\Session' {endDate} -> endDate) (\s@Session' {} a -> s {endDate = a} :: Session) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Session where
  parseJSON =
    Prelude.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StartDate")
            Prelude.<*> (x Prelude..:? "SessionId")
            Prelude.<*> (x Prelude..:? "DocumentName")
            Prelude.<*> (x Prelude..:? "Details")
            Prelude.<*> (x Prelude..:? "OutputUrl")
            Prelude.<*> (x Prelude..:? "Target")
            Prelude.<*> (x Prelude..:? "Owner")
            Prelude.<*> (x Prelude..:? "EndDate")
      )

instance Prelude.Hashable Session

instance Prelude.NFData Session

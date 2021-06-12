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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.SessionManagerOutputUrl
import Network.AWS.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to an instance.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The status of the session. For example, \"Connected\" or \"Terminated\".
    status :: Core.Maybe SessionStatus,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Core.Maybe Core.POSIX,
    -- | The ID of the session.
    sessionId :: Core.Maybe Core.Text,
    -- | The name of the Session Manager SSM document used to define the
    -- parameters and plugin settings for the session. For example,
    -- @SSM-SessionManagerRunShell@.
    documentName :: Core.Maybe Core.Text,
    -- | Reserved for future use.
    details :: Core.Maybe Core.Text,
    -- | Reserved for future use.
    outputUrl :: Core.Maybe SessionManagerOutputUrl,
    -- | The instance that the Session Manager session connected to.
    target :: Core.Maybe Core.Text,
    -- | The ID of the AWS user account that started the session.
    owner :: Core.Maybe Core.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session was
    -- terminated.
    endDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      startDate = Core.Nothing,
      sessionId = Core.Nothing,
      documentName = Core.Nothing,
      details = Core.Nothing,
      outputUrl = Core.Nothing,
      target = Core.Nothing,
      owner = Core.Nothing,
      endDate = Core.Nothing
    }

-- | The status of the session. For example, \"Connected\" or \"Terminated\".
session_status :: Lens.Lens' Session (Core.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session began.
session_startDate :: Lens.Lens' Session (Core.Maybe Core.UTCTime)
session_startDate = Lens.lens (\Session' {startDate} -> startDate) (\s@Session' {} a -> s {startDate = a} :: Session) Core.. Lens.mapping Core._Time

-- | The ID of the session.
session_sessionId :: Lens.Lens' Session (Core.Maybe Core.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | The name of the Session Manager SSM document used to define the
-- parameters and plugin settings for the session. For example,
-- @SSM-SessionManagerRunShell@.
session_documentName :: Lens.Lens' Session (Core.Maybe Core.Text)
session_documentName = Lens.lens (\Session' {documentName} -> documentName) (\s@Session' {} a -> s {documentName = a} :: Session)

-- | Reserved for future use.
session_details :: Lens.Lens' Session (Core.Maybe Core.Text)
session_details = Lens.lens (\Session' {details} -> details) (\s@Session' {} a -> s {details = a} :: Session)

-- | Reserved for future use.
session_outputUrl :: Lens.Lens' Session (Core.Maybe SessionManagerOutputUrl)
session_outputUrl = Lens.lens (\Session' {outputUrl} -> outputUrl) (\s@Session' {} a -> s {outputUrl = a} :: Session)

-- | The instance that the Session Manager session connected to.
session_target :: Lens.Lens' Session (Core.Maybe Core.Text)
session_target = Lens.lens (\Session' {target} -> target) (\s@Session' {} a -> s {target = a} :: Session)

-- | The ID of the AWS user account that started the session.
session_owner :: Lens.Lens' Session (Core.Maybe Core.Text)
session_owner = Lens.lens (\Session' {owner} -> owner) (\s@Session' {} a -> s {owner = a} :: Session)

-- | The date and time, in ISO-8601 Extended format, when the session was
-- terminated.
session_endDate :: Lens.Lens' Session (Core.Maybe Core.UTCTime)
session_endDate = Lens.lens (\Session' {endDate} -> endDate) (\s@Session' {} a -> s {endDate = a} :: Session) Core.. Lens.mapping Core._Time

instance Core.FromJSON Session where
  parseJSON =
    Core.withObject
      "Session"
      ( \x ->
          Session'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartDate")
            Core.<*> (x Core..:? "SessionId")
            Core.<*> (x Core..:? "DocumentName")
            Core.<*> (x Core..:? "Details")
            Core.<*> (x Core..:? "OutputUrl")
            Core.<*> (x Core..:? "Target")
            Core.<*> (x Core..:? "Owner")
            Core.<*> (x Core..:? "EndDate")
      )

instance Core.Hashable Session

instance Core.NFData Session

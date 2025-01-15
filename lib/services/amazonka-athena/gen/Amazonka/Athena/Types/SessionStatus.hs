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
-- Module      : Amazonka.Athena.Types.SessionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.SessionStatus where

import Amazonka.Athena.Types.SessionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the status of a notebook session.
--
-- /See:/ 'newSessionStatus' smart constructor.
data SessionStatus = SessionStatus'
  { -- | The date and time that the session ended.
    endDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time starting at which the session became idle. Can be
    -- empty if the session is not currently idle.
    idleSinceDateTime :: Prelude.Maybe Data.POSIX,
    -- | The most recent date and time that the session was modified.
    lastModifiedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the session started.
    startDateTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the session. A description of each state follows.
    --
    -- @CREATING@ - The session is being started, including acquiring
    -- resources.
    --
    -- @CREATED@ - The session has been started.
    --
    -- @IDLE@ - The session is able to accept a calculation.
    --
    -- @BUSY@ - The session is processing another task and is unable to accept
    -- a calculation.
    --
    -- @TERMINATING@ - The session is in the process of shutting down.
    --
    -- @TERMINATED@ - The session and its resources are no longer running.
    --
    -- @DEGRADED@ - The session has no healthy coordinators.
    --
    -- @FAILED@ - Due to a failure, the session and its resources are no longer
    -- running.
    state :: Prelude.Maybe SessionState,
    -- | The reason for the session state change (for example, canceled because
    -- the session was terminated).
    stateChangeReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDateTime', 'sessionStatus_endDateTime' - The date and time that the session ended.
--
-- 'idleSinceDateTime', 'sessionStatus_idleSinceDateTime' - The date and time starting at which the session became idle. Can be
-- empty if the session is not currently idle.
--
-- 'lastModifiedDateTime', 'sessionStatus_lastModifiedDateTime' - The most recent date and time that the session was modified.
--
-- 'startDateTime', 'sessionStatus_startDateTime' - The date and time that the session started.
--
-- 'state', 'sessionStatus_state' - The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
--
-- 'stateChangeReason', 'sessionStatus_stateChangeReason' - The reason for the session state change (for example, canceled because
-- the session was terminated).
newSessionStatus ::
  SessionStatus
newSessionStatus =
  SessionStatus'
    { endDateTime = Prelude.Nothing,
      idleSinceDateTime = Prelude.Nothing,
      lastModifiedDateTime = Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing
    }

-- | The date and time that the session ended.
sessionStatus_endDateTime :: Lens.Lens' SessionStatus (Prelude.Maybe Prelude.UTCTime)
sessionStatus_endDateTime = Lens.lens (\SessionStatus' {endDateTime} -> endDateTime) (\s@SessionStatus' {} a -> s {endDateTime = a} :: SessionStatus) Prelude.. Lens.mapping Data._Time

-- | The date and time starting at which the session became idle. Can be
-- empty if the session is not currently idle.
sessionStatus_idleSinceDateTime :: Lens.Lens' SessionStatus (Prelude.Maybe Prelude.UTCTime)
sessionStatus_idleSinceDateTime = Lens.lens (\SessionStatus' {idleSinceDateTime} -> idleSinceDateTime) (\s@SessionStatus' {} a -> s {idleSinceDateTime = a} :: SessionStatus) Prelude.. Lens.mapping Data._Time

-- | The most recent date and time that the session was modified.
sessionStatus_lastModifiedDateTime :: Lens.Lens' SessionStatus (Prelude.Maybe Prelude.UTCTime)
sessionStatus_lastModifiedDateTime = Lens.lens (\SessionStatus' {lastModifiedDateTime} -> lastModifiedDateTime) (\s@SessionStatus' {} a -> s {lastModifiedDateTime = a} :: SessionStatus) Prelude.. Lens.mapping Data._Time

-- | The date and time that the session started.
sessionStatus_startDateTime :: Lens.Lens' SessionStatus (Prelude.Maybe Prelude.UTCTime)
sessionStatus_startDateTime = Lens.lens (\SessionStatus' {startDateTime} -> startDateTime) (\s@SessionStatus' {} a -> s {startDateTime = a} :: SessionStatus) Prelude.. Lens.mapping Data._Time

-- | The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
sessionStatus_state :: Lens.Lens' SessionStatus (Prelude.Maybe SessionState)
sessionStatus_state = Lens.lens (\SessionStatus' {state} -> state) (\s@SessionStatus' {} a -> s {state = a} :: SessionStatus)

-- | The reason for the session state change (for example, canceled because
-- the session was terminated).
sessionStatus_stateChangeReason :: Lens.Lens' SessionStatus (Prelude.Maybe Prelude.Text)
sessionStatus_stateChangeReason = Lens.lens (\SessionStatus' {stateChangeReason} -> stateChangeReason) (\s@SessionStatus' {} a -> s {stateChangeReason = a} :: SessionStatus)

instance Data.FromJSON SessionStatus where
  parseJSON =
    Data.withObject
      "SessionStatus"
      ( \x ->
          SessionStatus'
            Prelude.<$> (x Data..:? "EndDateTime")
            Prelude.<*> (x Data..:? "IdleSinceDateTime")
            Prelude.<*> (x Data..:? "LastModifiedDateTime")
            Prelude.<*> (x Data..:? "StartDateTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateChangeReason")
      )

instance Prelude.Hashable SessionStatus where
  hashWithSalt _salt SessionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` idleSinceDateTime
      `Prelude.hashWithSalt` lastModifiedDateTime
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateChangeReason

instance Prelude.NFData SessionStatus where
  rnf SessionStatus' {..} =
    Prelude.rnf endDateTime `Prelude.seq`
      Prelude.rnf idleSinceDateTime `Prelude.seq`
        Prelude.rnf lastModifiedDateTime `Prelude.seq`
          Prelude.rnf startDateTime `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf stateChangeReason

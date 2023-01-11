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
-- Module      : Amazonka.AppStream.Types.Session
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Session where

import Amazonka.AppStream.Types.AuthenticationType
import Amazonka.AppStream.Types.NetworkAccessConfiguration
import Amazonka.AppStream.Types.SessionConnectionState
import Amazonka.AppStream.Types.SessionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a streaming session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The authentication method. The user is authenticated using a streaming
    -- URL (@API@) or SAML 2.0 federation (@SAML@).
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | Specifies whether a user is connected to the streaming session.
    connectionState :: Prelude.Maybe SessionConnectionState,
    -- | The time when the streaming session is set to expire. This time is based
    -- on the @MaxUserDurationinSeconds@ value, which determines the maximum
    -- length of time that a streaming session can run. A streaming session
    -- might end earlier than the time specified in @SessionMaxExpirationTime@,
    -- when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end
    -- his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the
    -- user chooses to end his or her session, the streaming instance is
    -- terminated and the streaming session ends.
    maxExpirationTime :: Prelude.Maybe Data.POSIX,
    -- | The network details for the streaming session.
    networkAccessConfiguration :: Prelude.Maybe NetworkAccessConfiguration,
    -- | The time when a streaming instance is dedicated for the user.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the streaming session.
    id :: Prelude.Text,
    -- | The identifier of the user for whom the session was created.
    userId :: Prelude.Text,
    -- | The name of the stack for the streaming session.
    stackName :: Prelude.Text,
    -- | The name of the fleet for the streaming session.
    fleetName :: Prelude.Text,
    -- | The current state of the streaming session.
    state :: SessionState
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
-- 'authenticationType', 'session_authenticationType' - The authentication method. The user is authenticated using a streaming
-- URL (@API@) or SAML 2.0 federation (@SAML@).
--
-- 'connectionState', 'session_connectionState' - Specifies whether a user is connected to the streaming session.
--
-- 'maxExpirationTime', 'session_maxExpirationTime' - The time when the streaming session is set to expire. This time is based
-- on the @MaxUserDurationinSeconds@ value, which determines the maximum
-- length of time that a streaming session can run. A streaming session
-- might end earlier than the time specified in @SessionMaxExpirationTime@,
-- when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end
-- his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the
-- user chooses to end his or her session, the streaming instance is
-- terminated and the streaming session ends.
--
-- 'networkAccessConfiguration', 'session_networkAccessConfiguration' - The network details for the streaming session.
--
-- 'startTime', 'session_startTime' - The time when a streaming instance is dedicated for the user.
--
-- 'id', 'session_id' - The identifier of the streaming session.
--
-- 'userId', 'session_userId' - The identifier of the user for whom the session was created.
--
-- 'stackName', 'session_stackName' - The name of the stack for the streaming session.
--
-- 'fleetName', 'session_fleetName' - The name of the fleet for the streaming session.
--
-- 'state', 'session_state' - The current state of the streaming session.
newSession ::
  -- | 'id'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'state'
  SessionState ->
  Session
newSession
  pId_
  pUserId_
  pStackName_
  pFleetName_
  pState_ =
    Session'
      { authenticationType = Prelude.Nothing,
        connectionState = Prelude.Nothing,
        maxExpirationTime = Prelude.Nothing,
        networkAccessConfiguration = Prelude.Nothing,
        startTime = Prelude.Nothing,
        id = pId_,
        userId = pUserId_,
        stackName = pStackName_,
        fleetName = pFleetName_,
        state = pState_
      }

-- | The authentication method. The user is authenticated using a streaming
-- URL (@API@) or SAML 2.0 federation (@SAML@).
session_authenticationType :: Lens.Lens' Session (Prelude.Maybe AuthenticationType)
session_authenticationType = Lens.lens (\Session' {authenticationType} -> authenticationType) (\s@Session' {} a -> s {authenticationType = a} :: Session)

-- | Specifies whether a user is connected to the streaming session.
session_connectionState :: Lens.Lens' Session (Prelude.Maybe SessionConnectionState)
session_connectionState = Lens.lens (\Session' {connectionState} -> connectionState) (\s@Session' {} a -> s {connectionState = a} :: Session)

-- | The time when the streaming session is set to expire. This time is based
-- on the @MaxUserDurationinSeconds@ value, which determines the maximum
-- length of time that a streaming session can run. A streaming session
-- might end earlier than the time specified in @SessionMaxExpirationTime@,
-- when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end
-- his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the
-- user chooses to end his or her session, the streaming instance is
-- terminated and the streaming session ends.
session_maxExpirationTime :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_maxExpirationTime = Lens.lens (\Session' {maxExpirationTime} -> maxExpirationTime) (\s@Session' {} a -> s {maxExpirationTime = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | The network details for the streaming session.
session_networkAccessConfiguration :: Lens.Lens' Session (Prelude.Maybe NetworkAccessConfiguration)
session_networkAccessConfiguration = Lens.lens (\Session' {networkAccessConfiguration} -> networkAccessConfiguration) (\s@Session' {} a -> s {networkAccessConfiguration = a} :: Session)

-- | The time when a streaming instance is dedicated for the user.
session_startTime :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startTime = Lens.lens (\Session' {startTime} -> startTime) (\s@Session' {} a -> s {startTime = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | The identifier of the streaming session.
session_id :: Lens.Lens' Session Prelude.Text
session_id = Lens.lens (\Session' {id} -> id) (\s@Session' {} a -> s {id = a} :: Session)

-- | The identifier of the user for whom the session was created.
session_userId :: Lens.Lens' Session Prelude.Text
session_userId = Lens.lens (\Session' {userId} -> userId) (\s@Session' {} a -> s {userId = a} :: Session)

-- | The name of the stack for the streaming session.
session_stackName :: Lens.Lens' Session Prelude.Text
session_stackName = Lens.lens (\Session' {stackName} -> stackName) (\s@Session' {} a -> s {stackName = a} :: Session)

-- | The name of the fleet for the streaming session.
session_fleetName :: Lens.Lens' Session Prelude.Text
session_fleetName = Lens.lens (\Session' {fleetName} -> fleetName) (\s@Session' {} a -> s {fleetName = a} :: Session)

-- | The current state of the streaming session.
session_state :: Lens.Lens' Session SessionState
session_state = Lens.lens (\Session' {state} -> state) (\s@Session' {} a -> s {state = a} :: Session)

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "ConnectionState")
            Prelude.<*> (x Data..:? "MaxExpirationTime")
            Prelude.<*> (x Data..:? "NetworkAccessConfiguration")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "UserId")
            Prelude.<*> (x Data..: "StackName")
            Prelude.<*> (x Data..: "FleetName")
            Prelude.<*> (x Data..: "State")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` connectionState
      `Prelude.hashWithSalt` maxExpirationTime
      `Prelude.hashWithSalt` networkAccessConfiguration
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` state

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf maxExpirationTime
      `Prelude.seq` Prelude.rnf networkAccessConfiguration
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf state

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
-- Module      : Network.AWS.AppStream.Types.Session
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Session where

import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.SessionConnectionState
import Network.AWS.AppStream.Types.SessionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a streaming session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | Specifies whether a user is connected to the streaming session.
    connectionState :: Prelude.Maybe SessionConnectionState,
    -- | The time when a streaming instance is dedicated for the user.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The network details for the streaming session.
    networkAccessConfiguration :: Prelude.Maybe NetworkAccessConfiguration,
    -- | The authentication method. The user is authenticated using a streaming
    -- URL (@API@) or SAML 2.0 federation (@SAML@).
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The time when the streaming session is set to expire. This time is based
    -- on the @MaxUserDurationinSeconds@ value, which determines the maximum
    -- length of time that a streaming session can run. A streaming session
    -- might end earlier than the time specified in @SessionMaxExpirationTime@,
    -- when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end
    -- his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the
    -- user chooses to end his or her session, the streaming instance is
    -- terminated and the streaming session ends.
    maxExpirationTime :: Prelude.Maybe Prelude.POSIX,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionState', 'session_connectionState' - Specifies whether a user is connected to the streaming session.
--
-- 'startTime', 'session_startTime' - The time when a streaming instance is dedicated for the user.
--
-- 'networkAccessConfiguration', 'session_networkAccessConfiguration' - The network details for the streaming session.
--
-- 'authenticationType', 'session_authenticationType' - The authentication method. The user is authenticated using a streaming
-- URL (@API@) or SAML 2.0 federation (@SAML@).
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
      { connectionState = Prelude.Nothing,
        startTime = Prelude.Nothing,
        networkAccessConfiguration = Prelude.Nothing,
        authenticationType = Prelude.Nothing,
        maxExpirationTime = Prelude.Nothing,
        id = pId_,
        userId = pUserId_,
        stackName = pStackName_,
        fleetName = pFleetName_,
        state = pState_
      }

-- | Specifies whether a user is connected to the streaming session.
session_connectionState :: Lens.Lens' Session (Prelude.Maybe SessionConnectionState)
session_connectionState = Lens.lens (\Session' {connectionState} -> connectionState) (\s@Session' {} a -> s {connectionState = a} :: Session)

-- | The time when a streaming instance is dedicated for the user.
session_startTime :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_startTime = Lens.lens (\Session' {startTime} -> startTime) (\s@Session' {} a -> s {startTime = a} :: Session) Prelude.. Lens.mapping Prelude._Time

-- | The network details for the streaming session.
session_networkAccessConfiguration :: Lens.Lens' Session (Prelude.Maybe NetworkAccessConfiguration)
session_networkAccessConfiguration = Lens.lens (\Session' {networkAccessConfiguration} -> networkAccessConfiguration) (\s@Session' {} a -> s {networkAccessConfiguration = a} :: Session)

-- | The authentication method. The user is authenticated using a streaming
-- URL (@API@) or SAML 2.0 federation (@SAML@).
session_authenticationType :: Lens.Lens' Session (Prelude.Maybe AuthenticationType)
session_authenticationType = Lens.lens (\Session' {authenticationType} -> authenticationType) (\s@Session' {} a -> s {authenticationType = a} :: Session)

-- | The time when the streaming session is set to expire. This time is based
-- on the @MaxUserDurationinSeconds@ value, which determines the maximum
-- length of time that a streaming session can run. A streaming session
-- might end earlier than the time specified in @SessionMaxExpirationTime@,
-- when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end
-- his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the
-- user chooses to end his or her session, the streaming instance is
-- terminated and the streaming session ends.
session_maxExpirationTime :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_maxExpirationTime = Lens.lens (\Session' {maxExpirationTime} -> maxExpirationTime) (\s@Session' {} a -> s {maxExpirationTime = a} :: Session) Prelude.. Lens.mapping Prelude._Time

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

instance Prelude.FromJSON Session where
  parseJSON =
    Prelude.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Prelude..:? "ConnectionState")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "NetworkAccessConfiguration")
            Prelude.<*> (x Prelude..:? "AuthenticationType")
            Prelude.<*> (x Prelude..:? "MaxExpirationTime")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "UserId")
            Prelude.<*> (x Prelude..: "StackName")
            Prelude.<*> (x Prelude..: "FleetName")
            Prelude.<*> (x Prelude..: "State")
      )

instance Prelude.Hashable Session

instance Prelude.NFData Session

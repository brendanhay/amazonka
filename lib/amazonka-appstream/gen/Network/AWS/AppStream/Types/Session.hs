{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Session
  ( Session (..),

    -- * Smart constructor
    mkSession,

    -- * Lenses
    sNetworkAccessConfiguration,
    sMaxExpirationTime,
    sStartTime,
    sAuthenticationType,
    sConnectionState,
    sId,
    sUserId,
    sStackName,
    sFleetName,
    sState,
  )
where

import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.SessionConnectionState
import Network.AWS.AppStream.Types.SessionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a streaming session.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { networkAccessConfiguration ::
      Lude.Maybe NetworkAccessConfiguration,
    maxExpirationTime :: Lude.Maybe Lude.Timestamp,
    startTime :: Lude.Maybe Lude.Timestamp,
    authenticationType :: Lude.Maybe AuthenticationType,
    connectionState :: Lude.Maybe SessionConnectionState,
    id :: Lude.Text,
    userId :: Lude.Text,
    stackName :: Lude.Text,
    fleetName :: Lude.Text,
    state :: SessionState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
-- * 'connectionState' - Specifies whether a user is connected to the streaming session.
-- * 'fleetName' - The name of the fleet for the streaming session.
-- * 'id' - The identifier of the streaming session.
-- * 'maxExpirationTime' - The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
-- * 'networkAccessConfiguration' - The network details for the streaming session.
-- * 'stackName' - The name of the stack for the streaming session.
-- * 'startTime' - The time when a streaming instance is dedicated for the user.
-- * 'state' - The current state of the streaming session.
-- * 'userId' - The identifier of the user for whom the session was created.
mkSession ::
  -- | 'id'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  -- | 'fleetName'
  Lude.Text ->
  -- | 'state'
  SessionState ->
  Session
mkSession pId_ pUserId_ pStackName_ pFleetName_ pState_ =
  Session'
    { networkAccessConfiguration = Lude.Nothing,
      maxExpirationTime = Lude.Nothing,
      startTime = Lude.Nothing,
      authenticationType = Lude.Nothing,
      connectionState = Lude.Nothing,
      id = pId_,
      userId = pUserId_,
      stackName = pStackName_,
      fleetName = pFleetName_,
      state = pState_
    }

-- | The network details for the streaming session.
--
-- /Note:/ Consider using 'networkAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNetworkAccessConfiguration :: Lens.Lens' Session (Lude.Maybe NetworkAccessConfiguration)
sNetworkAccessConfiguration = Lens.lens (networkAccessConfiguration :: Session -> Lude.Maybe NetworkAccessConfiguration) (\s a -> s {networkAccessConfiguration = a} :: Session)
{-# DEPRECATED sNetworkAccessConfiguration "Use generic-lens or generic-optics with 'networkAccessConfiguration' instead." #-}

-- | The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
--
-- /Note:/ Consider using 'maxExpirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxExpirationTime :: Lens.Lens' Session (Lude.Maybe Lude.Timestamp)
sMaxExpirationTime = Lens.lens (maxExpirationTime :: Session -> Lude.Maybe Lude.Timestamp) (\s a -> s {maxExpirationTime = a} :: Session)
{-# DEPRECATED sMaxExpirationTime "Use generic-lens or generic-optics with 'maxExpirationTime' instead." #-}

-- | The time when a streaming instance is dedicated for the user.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Session (Lude.Maybe Lude.Timestamp)
sStartTime = Lens.lens (startTime :: Session -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Session)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAuthenticationType :: Lens.Lens' Session (Lude.Maybe AuthenticationType)
sAuthenticationType = Lens.lens (authenticationType :: Session -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: Session)
{-# DEPRECATED sAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | Specifies whether a user is connected to the streaming session.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConnectionState :: Lens.Lens' Session (Lude.Maybe SessionConnectionState)
sConnectionState = Lens.lens (connectionState :: Session -> Lude.Maybe SessionConnectionState) (\s a -> s {connectionState = a} :: Session)
{-# DEPRECATED sConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The identifier of the streaming session.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Session Lude.Text
sId = Lens.lens (id :: Session -> Lude.Text) (\s a -> s {id = a} :: Session)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identifier of the user for whom the session was created.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUserId :: Lens.Lens' Session Lude.Text
sUserId = Lens.lens (userId :: Session -> Lude.Text) (\s a -> s {userId = a} :: Session)
{-# DEPRECATED sUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The name of the stack for the streaming session.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackName :: Lens.Lens' Session Lude.Text
sStackName = Lens.lens (stackName :: Session -> Lude.Text) (\s a -> s {stackName = a} :: Session)
{-# DEPRECATED sStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the fleet for the streaming session.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFleetName :: Lens.Lens' Session Lude.Text
sFleetName = Lens.lens (fleetName :: Session -> Lude.Text) (\s a -> s {fleetName = a} :: Session)
{-# DEPRECATED sFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The current state of the streaming session.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Session SessionState
sState = Lens.lens (state :: Session -> SessionState) (\s a -> s {state = a} :: Session)
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromJSON Session where
  parseJSON =
    Lude.withObject
      "Session"
      ( \x ->
          Session'
            Lude.<$> (x Lude..:? "NetworkAccessConfiguration")
            Lude.<*> (x Lude..:? "MaxExpirationTime")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "AuthenticationType")
            Lude.<*> (x Lude..:? "ConnectionState")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "UserId")
            Lude.<*> (x Lude..: "StackName")
            Lude.<*> (x Lude..: "FleetName")
            Lude.<*> (x Lude..: "State")
      )

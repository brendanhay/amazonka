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
    sId,
    sUserId,
    sStackName,
    sFleetName,
    sState,
    sAuthenticationType,
    sConnectionState,
    sMaxExpirationTime,
    sNetworkAccessConfiguration,
    sStartTime,
  )
where

import qualified Network.AWS.AppStream.Types.AuthenticationType as Types
import qualified Network.AWS.AppStream.Types.NetworkAccessConfiguration as Types
import qualified Network.AWS.AppStream.Types.SessionConnectionState as Types
import qualified Network.AWS.AppStream.Types.SessionState as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a streaming session.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { -- | The identifier of the streaming session.
    id :: Types.String,
    -- | The identifier of the user for whom the session was created.
    userId :: Types.UserId,
    -- | The name of the stack for the streaming session.
    stackName :: Types.String,
    -- | The name of the fleet for the streaming session.
    fleetName :: Types.String,
    -- | The current state of the streaming session.
    state :: Types.SessionState,
    -- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
    authenticationType :: Core.Maybe Types.AuthenticationType,
    -- | Specifies whether a user is connected to the streaming session.
    connectionState :: Core.Maybe Types.SessionConnectionState,
    -- | The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
    maxExpirationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The network details for the streaming session.
    networkAccessConfiguration :: Core.Maybe Types.NetworkAccessConfiguration,
    -- | The time when a streaming instance is dedicated for the user.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Session' value with any optional fields omitted.
mkSession ::
  -- | 'id'
  Types.String ->
  -- | 'userId'
  Types.UserId ->
  -- | 'stackName'
  Types.String ->
  -- | 'fleetName'
  Types.String ->
  -- | 'state'
  Types.SessionState ->
  Session
mkSession id userId stackName fleetName state =
  Session'
    { id,
      userId,
      stackName,
      fleetName,
      state,
      authenticationType = Core.Nothing,
      connectionState = Core.Nothing,
      maxExpirationTime = Core.Nothing,
      networkAccessConfiguration = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The identifier of the streaming session.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Session Types.String
sId = Lens.field @"id"
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identifier of the user for whom the session was created.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUserId :: Lens.Lens' Session Types.UserId
sUserId = Lens.field @"userId"
{-# DEPRECATED sUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The name of the stack for the streaming session.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackName :: Lens.Lens' Session Types.String
sStackName = Lens.field @"stackName"
{-# DEPRECATED sStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the fleet for the streaming session.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFleetName :: Lens.Lens' Session Types.String
sFleetName = Lens.field @"fleetName"
{-# DEPRECATED sFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The current state of the streaming session.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Session Types.SessionState
sState = Lens.field @"state"
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAuthenticationType :: Lens.Lens' Session (Core.Maybe Types.AuthenticationType)
sAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED sAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | Specifies whether a user is connected to the streaming session.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConnectionState :: Lens.Lens' Session (Core.Maybe Types.SessionConnectionState)
sConnectionState = Lens.field @"connectionState"
{-# DEPRECATED sConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
--
-- /Note:/ Consider using 'maxExpirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxExpirationTime :: Lens.Lens' Session (Core.Maybe Core.NominalDiffTime)
sMaxExpirationTime = Lens.field @"maxExpirationTime"
{-# DEPRECATED sMaxExpirationTime "Use generic-lens or generic-optics with 'maxExpirationTime' instead." #-}

-- | The network details for the streaming session.
--
-- /Note:/ Consider using 'networkAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNetworkAccessConfiguration :: Lens.Lens' Session (Core.Maybe Types.NetworkAccessConfiguration)
sNetworkAccessConfiguration = Lens.field @"networkAccessConfiguration"
{-# DEPRECATED sNetworkAccessConfiguration "Use generic-lens or generic-optics with 'networkAccessConfiguration' instead." #-}

-- | The time when a streaming instance is dedicated for the user.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Session (Core.Maybe Core.NominalDiffTime)
sStartTime = Lens.field @"startTime"
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON Session where
  parseJSON =
    Core.withObject "Session" Core.$
      \x ->
        Session'
          Core.<$> (x Core..: "Id")
          Core.<*> (x Core..: "UserId")
          Core.<*> (x Core..: "StackName")
          Core.<*> (x Core..: "FleetName")
          Core.<*> (x Core..: "State")
          Core.<*> (x Core..:? "AuthenticationType")
          Core.<*> (x Core..:? "ConnectionState")
          Core.<*> (x Core..:? "MaxExpirationTime")
          Core.<*> (x Core..:? "NetworkAccessConfiguration")
          Core.<*> (x Core..:? "StartTime")

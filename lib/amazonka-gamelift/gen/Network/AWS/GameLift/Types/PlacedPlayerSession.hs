{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlacedPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.PlacedPlayerSession
  ( PlacedPlayerSession (..)
  -- * Smart constructor
  , mkPlacedPlayerSession
  -- * Lenses
  , ppsPlayerId
  , ppsPlayerSessionId
  ) where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlayerSessionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a player session that was created as part of a 'StartGameSessionPlacement' request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call 'DescribePlayerSessions' with the player session ID.
--
--
--     * 'CreatePlayerSession' 
--
--
--     * 'CreatePlayerSessions' 
--
--
--     * 'DescribePlayerSessions' 
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement' 
--
--
--     * 'DescribeGameSessionPlacement' 
--
--
--     * 'StopGameSessionPlacement' 
--
--
--
--
--
-- /See:/ 'mkPlacedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { playerId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for a player that is associated with this player session.
  , playerSessionId :: Core.Maybe Types.PlayerSessionId
    -- ^ A unique identifier for a player session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacedPlayerSession' value with any optional fields omitted.
mkPlacedPlayerSession
    :: PlacedPlayerSession
mkPlacedPlayerSession
  = PlacedPlayerSession'{playerId = Core.Nothing,
                         playerSessionId = Core.Nothing}

-- | A unique identifier for a player that is associated with this player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppsPlayerId :: Lens.Lens' PlacedPlayerSession (Core.Maybe Types.NonZeroAndMaxString)
ppsPlayerId = Lens.field @"playerId"
{-# INLINEABLE ppsPlayerId #-}
{-# DEPRECATED playerId "Use generic-lens or generic-optics with 'playerId' instead"  #-}

-- | A unique identifier for a player session.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppsPlayerSessionId :: Lens.Lens' PlacedPlayerSession (Core.Maybe Types.PlayerSessionId)
ppsPlayerSessionId = Lens.field @"playerSessionId"
{-# INLINEABLE ppsPlayerSessionId #-}
{-# DEPRECATED playerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead"  #-}

instance Core.FromJSON PlacedPlayerSession where
        parseJSON
          = Core.withObject "PlacedPlayerSession" Core.$
              \ x ->
                PlacedPlayerSession' Core.<$>
                  (x Core..:? "PlayerId") Core.<*> x Core..:? "PlayerSessionId"

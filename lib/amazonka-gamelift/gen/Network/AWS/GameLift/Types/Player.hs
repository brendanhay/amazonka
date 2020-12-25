{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Player
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Player
  ( Player (..),

    -- * Smart constructor
    mkPlayer,

    -- * Lenses
    pLatencyInMs,
    pPlayerAttributes,
    pPlayerId,
    pTeam,
  )
where

import qualified Network.AWS.GameLift.Types.AttributeValue as Types
import qualified Network.AWS.GameLift.Types.NonEmptyString as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.
--
-- /See:/ 'mkPlayer' smart constructor.
data Player = Player'
  { -- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.
    --
    -- If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
    latencyInMs :: Core.Maybe (Core.HashMap Types.NonEmptyString Core.Natural),
    -- | A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
    playerAttributes :: Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Types.AttributeValue),
    -- | A unique identifier for a player
    playerId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
    team :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Player' value with any optional fields omitted.
mkPlayer ::
  Player
mkPlayer =
  Player'
    { latencyInMs = Core.Nothing,
      playerAttributes = Core.Nothing,
      playerId = Core.Nothing,
      team = Core.Nothing
    }

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
--
-- /Note:/ Consider using 'latencyInMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLatencyInMs :: Lens.Lens' Player (Core.Maybe (Core.HashMap Types.NonEmptyString Core.Natural))
pLatencyInMs = Lens.field @"latencyInMs"
{-# DEPRECATED pLatencyInMs "Use generic-lens or generic-optics with 'latencyInMs' instead." #-}

-- | A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
--
-- /Note:/ Consider using 'playerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayerAttributes :: Lens.Lens' Player (Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Types.AttributeValue))
pPlayerAttributes = Lens.field @"playerAttributes"
{-# DEPRECATED pPlayerAttributes "Use generic-lens or generic-optics with 'playerAttributes' instead." #-}

-- | A unique identifier for a player
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayerId :: Lens.Lens' Player (Core.Maybe Types.NonZeroAndMaxString)
pPlayerId = Lens.field @"playerId"
{-# DEPRECATED pPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
--
-- /Note:/ Consider using 'team' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTeam :: Lens.Lens' Player (Core.Maybe Types.NonZeroAndMaxString)
pTeam = Lens.field @"team"
{-# DEPRECATED pTeam "Use generic-lens or generic-optics with 'team' instead." #-}

instance Core.FromJSON Player where
  toJSON Player {..} =
    Core.object
      ( Core.catMaybes
          [ ("LatencyInMs" Core..=) Core.<$> latencyInMs,
            ("PlayerAttributes" Core..=) Core.<$> playerAttributes,
            ("PlayerId" Core..=) Core.<$> playerId,
            ("Team" Core..=) Core.<$> team
          ]
      )

instance Core.FromJSON Player where
  parseJSON =
    Core.withObject "Player" Core.$
      \x ->
        Player'
          Core.<$> (x Core..:? "LatencyInMs")
          Core.<*> (x Core..:? "PlayerAttributes")
          Core.<*> (x Core..:? "PlayerId")
          Core.<*> (x Core..:? "Team")

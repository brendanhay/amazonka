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
    pPlayerAttributes,
    pTeam,
    pPlayerId,
    pLatencyInMs,
  )
where

import Network.AWS.GameLift.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.
--
-- /See:/ 'mkPlayer' smart constructor.
data Player = Player'
  { -- | A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
    playerAttributes :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
    team :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a player
    playerId :: Lude.Maybe Lude.Text,
    -- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.
    --
    -- If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
    latencyInMs :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Natural))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Player' with the minimum fields required to make a request.
--
-- * 'playerAttributes' - A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
-- * 'team' - Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
-- * 'playerId' - A unique identifier for a player
-- * 'latencyInMs' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
mkPlayer ::
  Player
mkPlayer =
  Player'
    { playerAttributes = Lude.Nothing,
      team = Lude.Nothing,
      playerId = Lude.Nothing,
      latencyInMs = Lude.Nothing
    }

-- | A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
--
-- /Note:/ Consider using 'playerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayerAttributes :: Lens.Lens' Player (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
pPlayerAttributes = Lens.lens (playerAttributes :: Player -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {playerAttributes = a} :: Player)
{-# DEPRECATED pPlayerAttributes "Use generic-lens or generic-optics with 'playerAttributes' instead." #-}

-- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
--
-- /Note:/ Consider using 'team' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTeam :: Lens.Lens' Player (Lude.Maybe Lude.Text)
pTeam = Lens.lens (team :: Player -> Lude.Maybe Lude.Text) (\s a -> s {team = a} :: Player)
{-# DEPRECATED pTeam "Use generic-lens or generic-optics with 'team' instead." #-}

-- | A unique identifier for a player
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayerId :: Lens.Lens' Player (Lude.Maybe Lude.Text)
pPlayerId = Lens.lens (playerId :: Player -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: Player)
{-# DEPRECATED pPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
--
-- /Note:/ Consider using 'latencyInMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLatencyInMs :: Lens.Lens' Player (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Natural)))
pLatencyInMs = Lens.lens (latencyInMs :: Player -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Natural))) (\s a -> s {latencyInMs = a} :: Player)
{-# DEPRECATED pLatencyInMs "Use generic-lens or generic-optics with 'latencyInMs' instead." #-}

instance Lude.FromJSON Player where
  parseJSON =
    Lude.withObject
      "Player"
      ( \x ->
          Player'
            Lude.<$> (x Lude..:? "PlayerAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Team")
            Lude.<*> (x Lude..:? "PlayerId")
            Lude.<*> (x Lude..:? "LatencyInMs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Player where
  toJSON Player' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayerAttributes" Lude..=) Lude.<$> playerAttributes,
            ("Team" Lude..=) Lude.<$> team,
            ("PlayerId" Lude..=) Lude.<$> playerId,
            ("LatencyInMs" Lude..=) Lude.<$> latencyInMs
          ]
      )

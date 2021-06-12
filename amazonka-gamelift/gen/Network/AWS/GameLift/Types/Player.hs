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
-- Module      : Network.AWS.GameLift.Types.Player
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Player where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.AttributeValue
import qualified Network.AWS.Lens as Lens

-- | Represents a player in matchmaking. When starting a matchmaking request,
-- a player has a player ID, attributes, and may have latency data. Team
-- information is added after a match has been successfully completed.
--
-- /See:/ 'newPlayer' smart constructor.
data Player = Player'
  { -- | A collection of key:value pairs containing player information for use in
    -- matchmaking. Player attribute keys must match the /playerAttributes/
    -- used in a matchmaking rule set. Example:
    -- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
    playerAttributes :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | Set of values, expressed in milliseconds, indicating the amount of
    -- latency that a player experiences when connected to AWS Regions. If this
    -- property is present, FlexMatch considers placing the match only in
    -- Regions for which latency is reported.
    --
    -- If a matchmaker has a rule that evaluates player latency, players must
    -- report latency in order to be matched. If no latency is reported in this
    -- scenario, FlexMatch assumes that no Regions are available to the player
    -- and the ticket is not matchable.
    latencyInMs :: Core.Maybe (Core.HashMap Core.Text Core.Natural),
    -- | A unique identifier for a player
    playerId :: Core.Maybe Core.Text,
    -- | Name of the team that the player is assigned to in a match. Team names
    -- are defined in a matchmaking rule set.
    team :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Player' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerAttributes', 'player_playerAttributes' - A collection of key:value pairs containing player information for use in
-- matchmaking. Player attribute keys must match the /playerAttributes/
-- used in a matchmaking rule set. Example:
-- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
--
-- 'latencyInMs', 'player_latencyInMs' - Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions. If this
-- property is present, FlexMatch considers placing the match only in
-- Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must
-- report latency in order to be matched. If no latency is reported in this
-- scenario, FlexMatch assumes that no Regions are available to the player
-- and the ticket is not matchable.
--
-- 'playerId', 'player_playerId' - A unique identifier for a player
--
-- 'team', 'player_team' - Name of the team that the player is assigned to in a match. Team names
-- are defined in a matchmaking rule set.
newPlayer ::
  Player
newPlayer =
  Player'
    { playerAttributes = Core.Nothing,
      latencyInMs = Core.Nothing,
      playerId = Core.Nothing,
      team = Core.Nothing
    }

-- | A collection of key:value pairs containing player information for use in
-- matchmaking. Player attribute keys must match the /playerAttributes/
-- used in a matchmaking rule set. Example:
-- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
player_playerAttributes :: Lens.Lens' Player (Core.Maybe (Core.HashMap Core.Text AttributeValue))
player_playerAttributes = Lens.lens (\Player' {playerAttributes} -> playerAttributes) (\s@Player' {} a -> s {playerAttributes = a} :: Player) Core.. Lens.mapping Lens._Coerce

-- | Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions. If this
-- property is present, FlexMatch considers placing the match only in
-- Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must
-- report latency in order to be matched. If no latency is reported in this
-- scenario, FlexMatch assumes that no Regions are available to the player
-- and the ticket is not matchable.
player_latencyInMs :: Lens.Lens' Player (Core.Maybe (Core.HashMap Core.Text Core.Natural))
player_latencyInMs = Lens.lens (\Player' {latencyInMs} -> latencyInMs) (\s@Player' {} a -> s {latencyInMs = a} :: Player) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for a player
player_playerId :: Lens.Lens' Player (Core.Maybe Core.Text)
player_playerId = Lens.lens (\Player' {playerId} -> playerId) (\s@Player' {} a -> s {playerId = a} :: Player)

-- | Name of the team that the player is assigned to in a match. Team names
-- are defined in a matchmaking rule set.
player_team :: Lens.Lens' Player (Core.Maybe Core.Text)
player_team = Lens.lens (\Player' {team} -> team) (\s@Player' {} a -> s {team = a} :: Player)

instance Core.FromJSON Player where
  parseJSON =
    Core.withObject
      "Player"
      ( \x ->
          Player'
            Core.<$> (x Core..:? "PlayerAttributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LatencyInMs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PlayerId")
            Core.<*> (x Core..:? "Team")
      )

instance Core.Hashable Player

instance Core.NFData Player

instance Core.ToJSON Player where
  toJSON Player' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerAttributes" Core..=)
              Core.<$> playerAttributes,
            ("LatencyInMs" Core..=) Core.<$> latencyInMs,
            ("PlayerId" Core..=) Core.<$> playerId,
            ("Team" Core..=) Core.<$> team
          ]
      )

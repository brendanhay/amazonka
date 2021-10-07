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
import qualified Network.AWS.Prelude as Prelude

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
    playerAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | A set of values, expressed in milliseconds, that indicates the amount of
    -- latency that a player experiences when connected to AWS Regions. If this
    -- property is present, FlexMatch considers placing the match only in
    -- Regions for which latency is reported.
    --
    -- If a matchmaker has a rule that evaluates player latency, players must
    -- report latency in order to be matched. If no latency is reported in this
    -- scenario, FlexMatch assumes that no Regions are available to the player
    -- and the ticket is not matchable.
    latencyInMs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural),
    -- | A unique identifier for a player
    playerId :: Prelude.Maybe Prelude.Text,
    -- | Name of the team that the player is assigned to in a match. Team names
    -- are defined in a matchmaking rule set.
    team :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'latencyInMs', 'player_latencyInMs' - A set of values, expressed in milliseconds, that indicates the amount of
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
    { playerAttributes = Prelude.Nothing,
      latencyInMs = Prelude.Nothing,
      playerId = Prelude.Nothing,
      team = Prelude.Nothing
    }

-- | A collection of key:value pairs containing player information for use in
-- matchmaking. Player attribute keys must match the /playerAttributes/
-- used in a matchmaking rule set. Example:
-- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
player_playerAttributes :: Lens.Lens' Player (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
player_playerAttributes = Lens.lens (\Player' {playerAttributes} -> playerAttributes) (\s@Player' {} a -> s {playerAttributes = a} :: Player) Prelude.. Lens.mapping Lens._Coerce

-- | A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to AWS Regions. If this
-- property is present, FlexMatch considers placing the match only in
-- Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must
-- report latency in order to be matched. If no latency is reported in this
-- scenario, FlexMatch assumes that no Regions are available to the player
-- and the ticket is not matchable.
player_latencyInMs :: Lens.Lens' Player (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
player_latencyInMs = Lens.lens (\Player' {latencyInMs} -> latencyInMs) (\s@Player' {} a -> s {latencyInMs = a} :: Player) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for a player
player_playerId :: Lens.Lens' Player (Prelude.Maybe Prelude.Text)
player_playerId = Lens.lens (\Player' {playerId} -> playerId) (\s@Player' {} a -> s {playerId = a} :: Player)

-- | Name of the team that the player is assigned to in a match. Team names
-- are defined in a matchmaking rule set.
player_team :: Lens.Lens' Player (Prelude.Maybe Prelude.Text)
player_team = Lens.lens (\Player' {team} -> team) (\s@Player' {} a -> s {team = a} :: Player)

instance Core.FromJSON Player where
  parseJSON =
    Core.withObject
      "Player"
      ( \x ->
          Player'
            Prelude.<$> ( x Core..:? "PlayerAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LatencyInMs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PlayerId")
            Prelude.<*> (x Core..:? "Team")
      )

instance Prelude.Hashable Player

instance Prelude.NFData Player

instance Core.ToJSON Player where
  toJSON Player' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlayerAttributes" Core..=)
              Prelude.<$> playerAttributes,
            ("LatencyInMs" Core..=) Prelude.<$> latencyInMs,
            ("PlayerId" Core..=) Prelude.<$> playerId,
            ("Team" Core..=) Prelude.<$> team
          ]
      )

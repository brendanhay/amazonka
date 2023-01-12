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
-- Module      : Amazonka.GameLift.Types.Player
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Player where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.AttributeValue
import qualified Amazonka.Prelude as Prelude

-- | Represents a player in matchmaking. When starting a matchmaking request,
-- a player has a player ID, attributes, and may have latency data. Team
-- information is added after a match has been successfully completed.
--
-- /See:/ 'newPlayer' smart constructor.
data Player = Player'
  { -- | A set of values, expressed in milliseconds, that indicates the amount of
    -- latency that a player experiences when connected to \@aws; Regions. If
    -- this property is present, FlexMatch considers placing the match only in
    -- Regions for which latency is reported.
    --
    -- If a matchmaker has a rule that evaluates player latency, players must
    -- report latency in order to be matched. If no latency is reported in this
    -- scenario, FlexMatch assumes that no Regions are available to the player
    -- and the ticket is not matchable.
    latencyInMs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural),
    -- | A collection of key:value pairs containing player information for use in
    -- matchmaking. Player attribute keys must match the /playerAttributes/
    -- used in a matchmaking rule set. Example:
    -- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
    --
    -- You can provide up to 10 @PlayerAttributes@.
    playerAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
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
-- 'latencyInMs', 'player_latencyInMs' - A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to \@aws; Regions. If
-- this property is present, FlexMatch considers placing the match only in
-- Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must
-- report latency in order to be matched. If no latency is reported in this
-- scenario, FlexMatch assumes that no Regions are available to the player
-- and the ticket is not matchable.
--
-- 'playerAttributes', 'player_playerAttributes' - A collection of key:value pairs containing player information for use in
-- matchmaking. Player attribute keys must match the /playerAttributes/
-- used in a matchmaking rule set. Example:
-- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
--
-- You can provide up to 10 @PlayerAttributes@.
--
-- 'playerId', 'player_playerId' - A unique identifier for a player
--
-- 'team', 'player_team' - Name of the team that the player is assigned to in a match. Team names
-- are defined in a matchmaking rule set.
newPlayer ::
  Player
newPlayer =
  Player'
    { latencyInMs = Prelude.Nothing,
      playerAttributes = Prelude.Nothing,
      playerId = Prelude.Nothing,
      team = Prelude.Nothing
    }

-- | A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to \@aws; Regions. If
-- this property is present, FlexMatch considers placing the match only in
-- Regions for which latency is reported.
--
-- If a matchmaker has a rule that evaluates player latency, players must
-- report latency in order to be matched. If no latency is reported in this
-- scenario, FlexMatch assumes that no Regions are available to the player
-- and the ticket is not matchable.
player_latencyInMs :: Lens.Lens' Player (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
player_latencyInMs = Lens.lens (\Player' {latencyInMs} -> latencyInMs) (\s@Player' {} a -> s {latencyInMs = a} :: Player) Prelude.. Lens.mapping Lens.coerced

-- | A collection of key:value pairs containing player information for use in
-- matchmaking. Player attribute keys must match the /playerAttributes/
-- used in a matchmaking rule set. Example:
-- @\"PlayerAttributes\": {\"skill\": {\"N\": \"23\"}, \"gameMode\": {\"S\": \"deathmatch\"}}@.
--
-- You can provide up to 10 @PlayerAttributes@.
player_playerAttributes :: Lens.Lens' Player (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
player_playerAttributes = Lens.lens (\Player' {playerAttributes} -> playerAttributes) (\s@Player' {} a -> s {playerAttributes = a} :: Player) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a player
player_playerId :: Lens.Lens' Player (Prelude.Maybe Prelude.Text)
player_playerId = Lens.lens (\Player' {playerId} -> playerId) (\s@Player' {} a -> s {playerId = a} :: Player)

-- | Name of the team that the player is assigned to in a match. Team names
-- are defined in a matchmaking rule set.
player_team :: Lens.Lens' Player (Prelude.Maybe Prelude.Text)
player_team = Lens.lens (\Player' {team} -> team) (\s@Player' {} a -> s {team = a} :: Player)

instance Data.FromJSON Player where
  parseJSON =
    Data.withObject
      "Player"
      ( \x ->
          Player'
            Prelude.<$> (x Data..:? "LatencyInMs" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "PlayerAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PlayerId")
            Prelude.<*> (x Data..:? "Team")
      )

instance Prelude.Hashable Player where
  hashWithSalt _salt Player' {..} =
    _salt `Prelude.hashWithSalt` latencyInMs
      `Prelude.hashWithSalt` playerAttributes
      `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` team

instance Prelude.NFData Player where
  rnf Player' {..} =
    Prelude.rnf latencyInMs
      `Prelude.seq` Prelude.rnf playerAttributes
      `Prelude.seq` Prelude.rnf playerId
      `Prelude.seq` Prelude.rnf team

instance Data.ToJSON Player where
  toJSON Player' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LatencyInMs" Data..=) Prelude.<$> latencyInMs,
            ("PlayerAttributes" Data..=)
              Prelude.<$> playerAttributes,
            ("PlayerId" Data..=) Prelude.<$> playerId,
            ("Team" Data..=) Prelude.<$> team
          ]
      )

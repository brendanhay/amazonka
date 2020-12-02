{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Player
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Player where

import Network.AWS.GameLift.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.
--
--
--
-- /See:/ 'player' smart constructor.
data Player = Player'
  { _pPlayerAttributes ::
      !(Maybe (Map Text (AttributeValue))),
    _pTeam :: !(Maybe Text),
    _pPlayerId :: !(Maybe Text),
    _pLatencyInMs :: !(Maybe (Map Text (Nat)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Player' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPlayerAttributes' - A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
--
-- * 'pTeam' - Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
--
-- * 'pPlayerId' - A unique identifier for a player
--
-- * 'pLatencyInMs' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.  If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
player ::
  Player
player =
  Player'
    { _pPlayerAttributes = Nothing,
      _pTeam = Nothing,
      _pPlayerId = Nothing,
      _pLatencyInMs = Nothing
    }

-- | A collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
pPlayerAttributes :: Lens' Player (HashMap Text (AttributeValue))
pPlayerAttributes = lens _pPlayerAttributes (\s a -> s {_pPlayerAttributes = a}) . _Default . _Map

-- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
pTeam :: Lens' Player (Maybe Text)
pTeam = lens _pTeam (\s a -> s {_pTeam = a})

-- | A unique identifier for a player
pPlayerId :: Lens' Player (Maybe Text)
pPlayerId = lens _pPlayerId (\s a -> s {_pPlayerId = a})

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. If this property is present, FlexMatch considers placing the match only in Regions for which latency is reported.  If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no Regions are available to the player and the ticket is not matchable.
pLatencyInMs :: Lens' Player (HashMap Text (Natural))
pLatencyInMs = lens _pLatencyInMs (\s a -> s {_pLatencyInMs = a}) . _Default . _Map

instance FromJSON Player where
  parseJSON =
    withObject
      "Player"
      ( \x ->
          Player'
            <$> (x .:? "PlayerAttributes" .!= mempty)
            <*> (x .:? "Team")
            <*> (x .:? "PlayerId")
            <*> (x .:? "LatencyInMs" .!= mempty)
      )

instance Hashable Player

instance NFData Player

instance ToJSON Player where
  toJSON Player' {..} =
    object
      ( catMaybes
          [ ("PlayerAttributes" .=) <$> _pPlayerAttributes,
            ("Team" .=) <$> _pTeam,
            ("PlayerId" .=) <$> _pPlayerId,
            ("LatencyInMs" .=) <$> _pLatencyInMs
          ]
      )

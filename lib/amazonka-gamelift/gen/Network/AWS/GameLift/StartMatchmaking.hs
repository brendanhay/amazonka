{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules. If you're also using GameLift hosting, a new game session is started for the matched players. Each matchmaking request identifies one or more players to find a match for, and specifies the type of match to build, including the team configuration and the rules for an acceptable match. When a matchmaking request identifies a group of players who want to play together, FlexMatch finds additional players to fill the match. Match type, rules, and other features are defined in a @MatchmakingConfiguration@ .
--
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. For each player, you must also include the player attribute values that are required by the matchmaking configuration (in the rule set). If successful, a matchmaking ticket is returned with status set to @QUEUED@ .
--
-- Track the status of the ticket to respond as needed. If you're also using GameLift hosting, a successfully completed ticket contains game session connection information. Ticket status updates are tracked using event notification through Amazon Simple Notification Service (SNS), which is defined in the matchmaking configuration.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-tasks.html FlexMatch Integration Roadmap>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch Works>
--
-- __Related operations__
--
--     * 'StartMatchmaking'
--
--     * 'DescribeMatchmaking'
--
--     * 'StopMatchmaking'
--
--     * 'AcceptMatch'
--
--     * 'StartMatchBackfill'
module Network.AWS.GameLift.StartMatchmaking
  ( -- * Creating a Request
    startMatchmaking,
    StartMatchmaking,

    -- * Request Lenses
    sTicketId,
    sConfigurationName,
    sPlayers,

    -- * Destructuring the Response
    startMatchmakingResponse,
    StartMatchmakingResponse,

    -- * Response Lenses
    srsMatchmakingTicket,
    srsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'startMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { _sTicketId ::
      !(Maybe Text),
    _sConfigurationName :: !Text,
    _sPlayers :: ![Player]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMatchmaking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTicketId' - A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
--
-- * 'sConfigurationName' - Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
--
-- * 'sPlayers' - Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
startMatchmaking ::
  -- | 'sConfigurationName'
  Text ->
  StartMatchmaking
startMatchmaking pConfigurationName_ =
  StartMatchmaking'
    { _sTicketId = Nothing,
      _sConfigurationName = pConfigurationName_,
      _sPlayers = mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
sTicketId :: Lens' StartMatchmaking (Maybe Text)
sTicketId = lens _sTicketId (\s a -> s {_sTicketId = a})

-- | Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
sConfigurationName :: Lens' StartMatchmaking Text
sConfigurationName = lens _sConfigurationName (\s a -> s {_sConfigurationName = a})

-- | Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
sPlayers :: Lens' StartMatchmaking [Player]
sPlayers = lens _sPlayers (\s a -> s {_sPlayers = a}) . _Coerce

instance AWSRequest StartMatchmaking where
  type Rs StartMatchmaking = StartMatchmakingResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          StartMatchmakingResponse'
            <$> (x .?> "MatchmakingTicket") <*> (pure (fromEnum s))
      )

instance Hashable StartMatchmaking

instance NFData StartMatchmaking

instance ToHeaders StartMatchmaking where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.StartMatchmaking" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMatchmaking where
  toJSON StartMatchmaking' {..} =
    object
      ( catMaybes
          [ ("TicketId" .=) <$> _sTicketId,
            Just ("ConfigurationName" .= _sConfigurationName),
            Just ("Players" .= _sPlayers)
          ]
      )

instance ToPath StartMatchmaking where
  toPath = const "/"

instance ToQuery StartMatchmaking where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'startMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { _srsMatchmakingTicket ::
      !(Maybe MatchmakingTicket),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMatchmakingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsMatchmakingTicket' - Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
--
-- * 'srsResponseStatus' - -- | The response status code.
startMatchmakingResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartMatchmakingResponse
startMatchmakingResponse pResponseStatus_ =
  StartMatchmakingResponse'
    { _srsMatchmakingTicket = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
srsMatchmakingTicket :: Lens' StartMatchmakingResponse (Maybe MatchmakingTicket)
srsMatchmakingTicket = lens _srsMatchmakingTicket (\s a -> s {_srsMatchmakingTicket = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartMatchmakingResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartMatchmakingResponse

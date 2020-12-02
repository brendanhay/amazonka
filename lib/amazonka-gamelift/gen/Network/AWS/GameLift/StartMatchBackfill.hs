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
-- Module      : Network.AWS.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created.
--
--
-- To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed.
--
-- The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the 'GameSession' object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How Amazon GameLift FlexMatch Works> .
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>
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
module Network.AWS.GameLift.StartMatchBackfill
  ( -- * Creating a Request
    startMatchBackfill,
    StartMatchBackfill,

    -- * Request Lenses
    smbTicketId,
    smbGameSessionARN,
    smbConfigurationName,
    smbPlayers,

    -- * Destructuring the Response
    startMatchBackfillResponse,
    StartMatchBackfillResponse,

    -- * Response Lenses
    smbrsMatchmakingTicket,
    smbrsResponseStatus,
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
-- /See:/ 'startMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { _smbTicketId ::
      !(Maybe Text),
    _smbGameSessionARN :: !(Maybe Text),
    _smbConfigurationName :: !Text,
    _smbPlayers :: ![Player]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMatchBackfill' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbTicketId' - A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
--
-- * 'smbGameSessionARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
--
-- * 'smbConfigurationName' - Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
--
-- * 'smbPlayers' - Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .      * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
startMatchBackfill ::
  -- | 'smbConfigurationName'
  Text ->
  StartMatchBackfill
startMatchBackfill pConfigurationName_ =
  StartMatchBackfill'
    { _smbTicketId = Nothing,
      _smbGameSessionARN = Nothing,
      _smbConfigurationName = pConfigurationName_,
      _smbPlayers = mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
smbTicketId :: Lens' StartMatchBackfill (Maybe Text)
smbTicketId = lens _smbTicketId (\s a -> s {_smbTicketId = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
smbGameSessionARN :: Lens' StartMatchBackfill (Maybe Text)
smbGameSessionARN = lens _smbGameSessionARN (\s a -> s {_smbGameSessionARN = a})

-- | Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
smbConfigurationName :: Lens' StartMatchBackfill Text
smbConfigurationName = lens _smbConfigurationName (\s a -> s {_smbConfigurationName = a})

-- | Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .      * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
smbPlayers :: Lens' StartMatchBackfill [Player]
smbPlayers = lens _smbPlayers (\s a -> s {_smbPlayers = a}) . _Coerce

instance AWSRequest StartMatchBackfill where
  type Rs StartMatchBackfill = StartMatchBackfillResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          StartMatchBackfillResponse'
            <$> (x .?> "MatchmakingTicket") <*> (pure (fromEnum s))
      )

instance Hashable StartMatchBackfill

instance NFData StartMatchBackfill

instance ToHeaders StartMatchBackfill where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.StartMatchBackfill" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMatchBackfill where
  toJSON StartMatchBackfill' {..} =
    object
      ( catMaybes
          [ ("TicketId" .=) <$> _smbTicketId,
            ("GameSessionArn" .=) <$> _smbGameSessionARN,
            Just ("ConfigurationName" .= _smbConfigurationName),
            Just ("Players" .= _smbPlayers)
          ]
      )

instance ToPath StartMatchBackfill where
  toPath = const "/"

instance ToQuery StartMatchBackfill where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'startMatchBackfillResponse' smart constructor.
data StartMatchBackfillResponse = StartMatchBackfillResponse'
  { _smbrsMatchmakingTicket ::
      !(Maybe MatchmakingTicket),
    _smbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMatchBackfillResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbrsMatchmakingTicket' - Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
--
-- * 'smbrsResponseStatus' - -- | The response status code.
startMatchBackfillResponse ::
  -- | 'smbrsResponseStatus'
  Int ->
  StartMatchBackfillResponse
startMatchBackfillResponse pResponseStatus_ =
  StartMatchBackfillResponse'
    { _smbrsMatchmakingTicket = Nothing,
      _smbrsResponseStatus = pResponseStatus_
    }

-- | Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
smbrsMatchmakingTicket :: Lens' StartMatchBackfillResponse (Maybe MatchmakingTicket)
smbrsMatchmakingTicket = lens _smbrsMatchmakingTicket (\s a -> s {_smbrsMatchmakingTicket = a})

-- | -- | The response status code.
smbrsResponseStatus :: Lens' StartMatchBackfillResponse Int
smbrsResponseStatus = lens _smbrsResponseStatus (\s a -> s {_smbrsResponseStatus = a})

instance NFData StartMatchBackfillResponse

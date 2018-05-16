{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created.
--
--
-- To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed. For more detail how to set up backfilling, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-backfill.html Backfill Existing Games with FlexMatch> .
--
-- The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the 'GameSession' object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html How Amazon GameLift FlexMatch Works> .
--
-- Matchmaking-related operations include:
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
--
--
--
module Network.AWS.GameLift.StartMatchBackfill
    (
    -- * Creating a Request
      startMatchBackfill
    , StartMatchBackfill
    -- * Request Lenses
    , smbTicketId
    , smbConfigurationName
    , smbGameSessionARN
    , smbPlayers

    -- * Destructuring the Response
    , startMatchBackfillResponse
    , StartMatchBackfillResponse
    -- * Response Lenses
    , smbrsMatchmakingTicket
    , smbrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'startMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { _smbTicketId          :: !(Maybe Text)
  , _smbConfigurationName :: !Text
  , _smbGameSessionARN    :: !Text
  , _smbPlayers           :: ![Player]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMatchBackfill' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbTicketId' - Unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
--
-- * 'smbConfigurationName' - Name of the matchmaker to use for this request. The name of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property. This property contains a matchmaking configuration ARN value, which includes the matchmaker name. (In the ARN value "arn:aws:gamelift:us-west-2:111122223333:matchmakingconfiguration/MM-4v4", the matchmaking configuration name is "MM-4v4".) Use only the name for this parameter.
--
-- * 'smbGameSessionARN' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session and uniquely identifies it.
--
-- * 'smbPlayers' - Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> .      * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the region that the game session is currently in. Do not include latency values for any other region.
startMatchBackfill
    :: Text -- ^ 'smbConfigurationName'
    -> Text -- ^ 'smbGameSessionARN'
    -> StartMatchBackfill
startMatchBackfill pConfigurationName_ pGameSessionARN_ =
  StartMatchBackfill'
    { _smbTicketId = Nothing
    , _smbConfigurationName = pConfigurationName_
    , _smbGameSessionARN = pGameSessionARN_
    , _smbPlayers = mempty
    }


-- | Unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
smbTicketId :: Lens' StartMatchBackfill (Maybe Text)
smbTicketId = lens _smbTicketId (\ s a -> s{_smbTicketId = a})

-- | Name of the matchmaker to use for this request. The name of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property. This property contains a matchmaking configuration ARN value, which includes the matchmaker name. (In the ARN value "arn:aws:gamelift:us-west-2:111122223333:matchmakingconfiguration/MM-4v4", the matchmaking configuration name is "MM-4v4".) Use only the name for this parameter.
smbConfigurationName :: Lens' StartMatchBackfill Text
smbConfigurationName = lens _smbConfigurationName (\ s a -> s{_smbConfigurationName = a})

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session and uniquely identifies it.
smbGameSessionARN :: Lens' StartMatchBackfill Text
smbGameSessionARN = lens _smbGameSessionARN (\ s a -> s{_smbGameSessionARN = a})

-- | Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> .      * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the region that the game session is currently in. Do not include latency values for any other region.
smbPlayers :: Lens' StartMatchBackfill [Player]
smbPlayers = lens _smbPlayers (\ s a -> s{_smbPlayers = a}) . _Coerce

instance AWSRequest StartMatchBackfill where
        type Rs StartMatchBackfill =
             StartMatchBackfillResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 StartMatchBackfillResponse' <$>
                   (x .?> "MatchmakingTicket") <*> (pure (fromEnum s)))

instance Hashable StartMatchBackfill where

instance NFData StartMatchBackfill where

instance ToHeaders StartMatchBackfill where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StartMatchBackfill" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartMatchBackfill where
        toJSON StartMatchBackfill'{..}
          = object
              (catMaybes
                 [("TicketId" .=) <$> _smbTicketId,
                  Just ("ConfigurationName" .= _smbConfigurationName),
                  Just ("GameSessionArn" .= _smbGameSessionARN),
                  Just ("Players" .= _smbPlayers)])

instance ToPath StartMatchBackfill where
        toPath = const "/"

instance ToQuery StartMatchBackfill where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'startMatchBackfillResponse' smart constructor.
data StartMatchBackfillResponse = StartMatchBackfillResponse'
  { _smbrsMatchmakingTicket :: !(Maybe MatchmakingTicket)
  , _smbrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMatchBackfillResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbrsMatchmakingTicket' - Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
--
-- * 'smbrsResponseStatus' - -- | The response status code.
startMatchBackfillResponse
    :: Int -- ^ 'smbrsResponseStatus'
    -> StartMatchBackfillResponse
startMatchBackfillResponse pResponseStatus_ =
  StartMatchBackfillResponse'
    {_smbrsMatchmakingTicket = Nothing, _smbrsResponseStatus = pResponseStatus_}


-- | Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
smbrsMatchmakingTicket :: Lens' StartMatchBackfillResponse (Maybe MatchmakingTicket)
smbrsMatchmakingTicket = lens _smbrsMatchmakingTicket (\ s a -> s{_smbrsMatchmakingTicket = a})

-- | -- | The response status code.
smbrsResponseStatus :: Lens' StartMatchBackfillResponse Int
smbrsResponseStatus = lens _smbrsResponseStatus (\ s a -> s{_smbrsResponseStatus = a})

instance NFData StartMatchBackfillResponse where

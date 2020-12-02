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
-- Module      : Network.AWS.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules, and starts a new game for the matched players. Each matchmaking request specifies the type of match to build (team configuration, rules for an acceptable match, etc.). The request also specifies the players to find a match for and where to host the new game session for optimal performance. A matchmaking request might start with a single player or a group of players who want to play together. FlexMatch finds additional players as needed to fill the match. Match type, rules, and the queue used to place a new game session are defined in a @MatchmakingConfiguration@ . For complete information on setting up and using FlexMatch, see the topic <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html Adding FlexMatch to Your Game> .
--
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. You must also include a set of player attributes relevant for the matchmaking configuration. If successful, a matchmaking ticket is returned with status set to @QUEUED@ . Track the status of the ticket to respond as needed and acquire game session connection information for successfully completed matches.
--
-- __Tracking ticket status__ -- A couple of options are available for tracking the status of matchmaking requests:
--
--     * Polling -- Call @DescribeMatchmaking@ . This operation returns the full ticket object, including current status and (for completed tickets) game session connection info. We recommend polling no more than once every 10 seconds.
--
--     * Notifications -- Get event notifications for changes in ticket status using Amazon Simple Notification Service (SNS). Notifications are easy to set up (see 'CreateMatchmakingConfiguration' ) and typically deliver match status changes faster and more efficiently than polling. We recommend that you use polling to back up to notifications (since delivery is not guaranteed) and call @DescribeMatchmaking@ only when notifications are not received within 30 seconds.
--
--
--
-- __Processing a matchmaking request__ -- FlexMatch handles a matchmaking request as follows:
--
--     * Your client code submits a @StartMatchmaking@ request for one or more players and tracks the status of the request ticket.
--
--     * FlexMatch uses this ticket and others in process to build an acceptable match. When a potential match is identified, all tickets in the proposed match are advanced to the next status.
--
--     * If the match requires player acceptance (set in the matchmaking configuration), the tickets move into status @REQUIRES_ACCEPTANCE@ . This status triggers your client code to solicit acceptance from all players in every ticket involved in the match, and then call 'AcceptMatch' for each player. If any player rejects or fails to accept the match before a specified timeout, the proposed match is dropped (see @AcceptMatch@ for more details).
--
--     * Once a match is proposed and accepted, the matchmaking tickets move into status @PLACING@ . FlexMatch locates resources for a new game session using the game session queue (set in the matchmaking configuration) and creates the game session based on the match data.
--
--     * When the match is successfully placed, the matchmaking tickets move into @COMPLETED@ status. Connection information (including game session endpoint and player session) is added to the matchmaking tickets. Matched players can use the connection information to join the game.
--
--
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
module Network.AWS.GameLift.StartMatchmaking
    (
    -- * Creating a Request
      startMatchmaking
    , StartMatchmaking
    -- * Request Lenses
    , sTicketId
    , sConfigurationName
    , sPlayers

    -- * Destructuring the Response
    , startMatchmakingResponse
    , StartMatchmakingResponse
    -- * Response Lenses
    , srsMatchmakingTicket
    , srsResponseStatus
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
-- /See:/ 'startMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { _sTicketId          :: !(Maybe Text)
  , _sConfigurationName :: !Text
  , _sPlayers           :: ![Player]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMatchmaking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTicketId' - Unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
--
-- * 'sConfigurationName' - Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same region as this request.
--
-- * 'sPlayers' - Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
startMatchmaking
    :: Text -- ^ 'sConfigurationName'
    -> StartMatchmaking
startMatchmaking pConfigurationName_ =
  StartMatchmaking'
    { _sTicketId = Nothing
    , _sConfigurationName = pConfigurationName_
    , _sPlayers = mempty
    }


-- | Unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
sTicketId :: Lens' StartMatchmaking (Maybe Text)
sTicketId = lens _sTicketId (\ s a -> s{_sTicketId = a})

-- | Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same region as this request.
sConfigurationName :: Lens' StartMatchmaking Text
sConfigurationName = lens _sConfigurationName (\ s a -> s{_sConfigurationName = a})

-- | Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
sPlayers :: Lens' StartMatchmaking [Player]
sPlayers = lens _sPlayers (\ s a -> s{_sPlayers = a}) . _Coerce

instance AWSRequest StartMatchmaking where
        type Rs StartMatchmaking = StartMatchmakingResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 StartMatchmakingResponse' <$>
                   (x .?> "MatchmakingTicket") <*> (pure (fromEnum s)))

instance Hashable StartMatchmaking where

instance NFData StartMatchmaking where

instance ToHeaders StartMatchmaking where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StartMatchmaking" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartMatchmaking where
        toJSON StartMatchmaking'{..}
          = object
              (catMaybes
                 [("TicketId" .=) <$> _sTicketId,
                  Just ("ConfigurationName" .= _sConfigurationName),
                  Just ("Players" .= _sPlayers)])

instance ToPath StartMatchmaking where
        toPath = const "/"

instance ToQuery StartMatchmaking where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'startMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { _srsMatchmakingTicket :: !(Maybe MatchmakingTicket)
  , _srsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMatchmakingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsMatchmakingTicket' - Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
--
-- * 'srsResponseStatus' - -- | The response status code.
startMatchmakingResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartMatchmakingResponse
startMatchmakingResponse pResponseStatus_ =
  StartMatchmakingResponse'
    {_srsMatchmakingTicket = Nothing, _srsResponseStatus = pResponseStatus_}


-- | Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
srsMatchmakingTicket :: Lens' StartMatchmakingResponse (Maybe MatchmakingTicket)
srsMatchmakingTicket = lens _srsMatchmakingTicket (\ s a -> s{_srsMatchmakingTicket = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartMatchmakingResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartMatchmakingResponse where

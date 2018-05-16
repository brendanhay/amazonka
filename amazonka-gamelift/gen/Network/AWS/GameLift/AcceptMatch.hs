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
-- Module      : Network.AWS.GameLift.AcceptMatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a player's acceptance or rejection of a proposed FlexMatch match. A matchmaking configuration may require player acceptance; if so, then matches built with that configuration cannot be completed unless all players accept the proposed match within a specified time limit.
--
--
-- When FlexMatch builds a match, all the matchmaking tickets involved in the proposed match are placed into status @REQUIRES_ACCEPTANCE@ . This is a trigger for your game to get acceptance from all players in the ticket. Acceptances are only valid for tickets when they are in this status; all other acceptances result in an error.
--
-- To register acceptance, specify the ticket ID, a response, and one or more players. Once all players have registered acceptance, the matchmaking tickets advance to status @PLACING@ , where a new game session is created for the match.
--
-- If any player rejects the match, or if acceptances are not received before a specified timeout, the proposed match is dropped. The matchmaking tickets are then handled in one of two ways: For tickets where all players accepted the match, the ticket status is returned to @SEARCHING@ to find a new match. For tickets where one or more players failed to accept the match, the ticket status is set to @FAILED@ , and processing is terminated. A new matchmaking request for these players can be submitted as needed.
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
module Network.AWS.GameLift.AcceptMatch
    (
    -- * Creating a Request
      acceptMatch
    , AcceptMatch
    -- * Request Lenses
    , amTicketId
    , amPlayerIds
    , amAcceptanceType

    -- * Destructuring the Response
    , acceptMatchResponse
    , AcceptMatchResponse
    -- * Response Lenses
    , amrsResponseStatus
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
-- /See:/ 'acceptMatch' smart constructor.
data AcceptMatch = AcceptMatch'
  { _amTicketId       :: !Text
  , _amPlayerIds      :: ![Text]
  , _amAcceptanceType :: !AcceptanceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amTicketId' - Unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
--
-- * 'amPlayerIds' - Unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
--
-- * 'amAcceptanceType' - Player response to the proposed match.
acceptMatch
    :: Text -- ^ 'amTicketId'
    -> AcceptanceType -- ^ 'amAcceptanceType'
    -> AcceptMatch
acceptMatch pTicketId_ pAcceptanceType_ =
  AcceptMatch'
    { _amTicketId = pTicketId_
    , _amPlayerIds = mempty
    , _amAcceptanceType = pAcceptanceType_
    }


-- | Unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
amTicketId :: Lens' AcceptMatch Text
amTicketId = lens _amTicketId (\ s a -> s{_amTicketId = a})

-- | Unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
amPlayerIds :: Lens' AcceptMatch [Text]
amPlayerIds = lens _amPlayerIds (\ s a -> s{_amPlayerIds = a}) . _Coerce

-- | Player response to the proposed match.
amAcceptanceType :: Lens' AcceptMatch AcceptanceType
amAcceptanceType = lens _amAcceptanceType (\ s a -> s{_amAcceptanceType = a})

instance AWSRequest AcceptMatch where
        type Rs AcceptMatch = AcceptMatchResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 AcceptMatchResponse' <$> (pure (fromEnum s)))

instance Hashable AcceptMatch where

instance NFData AcceptMatch where

instance ToHeaders AcceptMatch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.AcceptMatch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcceptMatch where
        toJSON AcceptMatch'{..}
          = object
              (catMaybes
                 [Just ("TicketId" .= _amTicketId),
                  Just ("PlayerIds" .= _amPlayerIds),
                  Just ("AcceptanceType" .= _amAcceptanceType)])

instance ToPath AcceptMatch where
        toPath = const "/"

instance ToQuery AcceptMatch where
        toQuery = const mempty

-- | /See:/ 'acceptMatchResponse' smart constructor.
newtype AcceptMatchResponse = AcceptMatchResponse'
  { _amrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptMatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amrsResponseStatus' - -- | The response status code.
acceptMatchResponse
    :: Int -- ^ 'amrsResponseStatus'
    -> AcceptMatchResponse
acceptMatchResponse pResponseStatus_ =
  AcceptMatchResponse' {_amrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
amrsResponseStatus :: Lens' AcceptMatchResponse Int
amrsResponseStatus = lens _amrsResponseStatus (\ s a -> s{_amrsResponseStatus = a})

instance NFData AcceptMatchResponse where

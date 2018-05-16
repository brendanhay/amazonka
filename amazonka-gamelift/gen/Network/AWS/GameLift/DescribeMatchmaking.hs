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
-- Module      : Network.AWS.GameLift.DescribeMatchmaking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more matchmaking tickets. Use this operation to retrieve ticket information, including status and--once a successful match is made--acquire connection information for the resulting new game session.
--
--
-- You can use this operation to track the progress of matchmaking requests (through polling) as an alternative to using event notifications. See more details on tracking matchmaking requests through polling or notifications in 'StartMatchmaking' .
--
-- To request matchmaking tickets, provide a list of up to 10 ticket IDs. If the request is successful, a ticket object is returned for each requested ID that currently exists.
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
module Network.AWS.GameLift.DescribeMatchmaking
    (
    -- * Creating a Request
      describeMatchmaking
    , DescribeMatchmaking
    -- * Request Lenses
    , dmTicketIds

    -- * Destructuring the Response
    , describeMatchmakingResponse
    , DescribeMatchmakingResponse
    -- * Response Lenses
    , dmrsTicketList
    , dmrsResponseStatus
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
-- /See:/ 'describeMatchmaking' smart constructor.
newtype DescribeMatchmaking = DescribeMatchmaking'
  { _dmTicketIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmaking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmTicketIds' - Unique identifier for a matchmaking ticket. You can include up to 10 ID values.
describeMatchmaking
    :: DescribeMatchmaking
describeMatchmaking = DescribeMatchmaking' {_dmTicketIds = mempty}


-- | Unique identifier for a matchmaking ticket. You can include up to 10 ID values.
dmTicketIds :: Lens' DescribeMatchmaking [Text]
dmTicketIds = lens _dmTicketIds (\ s a -> s{_dmTicketIds = a}) . _Coerce

instance AWSRequest DescribeMatchmaking where
        type Rs DescribeMatchmaking =
             DescribeMatchmakingResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMatchmakingResponse' <$>
                   (x .?> "TicketList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeMatchmaking where

instance NFData DescribeMatchmaking where

instance ToHeaders DescribeMatchmaking where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeMatchmaking" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMatchmaking where
        toJSON DescribeMatchmaking'{..}
          = object
              (catMaybes [Just ("TicketIds" .= _dmTicketIds)])

instance ToPath DescribeMatchmaking where
        toPath = const "/"

instance ToQuery DescribeMatchmaking where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeMatchmakingResponse' smart constructor.
data DescribeMatchmakingResponse = DescribeMatchmakingResponse'
  { _dmrsTicketList     :: !(Maybe [MatchmakingTicket])
  , _dmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmakingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsTicketList' - Collection of existing matchmaking ticket objects matching the request.
--
-- * 'dmrsResponseStatus' - -- | The response status code.
describeMatchmakingResponse
    :: Int -- ^ 'dmrsResponseStatus'
    -> DescribeMatchmakingResponse
describeMatchmakingResponse pResponseStatus_ =
  DescribeMatchmakingResponse'
    {_dmrsTicketList = Nothing, _dmrsResponseStatus = pResponseStatus_}


-- | Collection of existing matchmaking ticket objects matching the request.
dmrsTicketList :: Lens' DescribeMatchmakingResponse [MatchmakingTicket]
dmrsTicketList = lens _dmrsTicketList (\ s a -> s{_dmrsTicketList = a}) . _Default . _Coerce

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DescribeMatchmakingResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\ s a -> s{_dmrsResponseStatus = a})

instance NFData DescribeMatchmakingResponse where

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
-- Module      : Network.AWS.GameLift.StopMatchmaking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a matchmaking ticket that is currently being processed. To stop the matchmaking operation, specify the ticket ID. If successful, work on the ticket is stopped, and the ticket status is changed to @CANCELLED@ .
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
module Network.AWS.GameLift.StopMatchmaking
    (
    -- * Creating a Request
      stopMatchmaking
    , StopMatchmaking
    -- * Request Lenses
    , smTicketId

    -- * Destructuring the Response
    , stopMatchmakingResponse
    , StopMatchmakingResponse
    -- * Response Lenses
    , smrsResponseStatus
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
-- /See:/ 'stopMatchmaking' smart constructor.
newtype StopMatchmaking = StopMatchmaking'
  { _smTicketId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopMatchmaking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smTicketId' - Unique identifier for a matchmaking ticket.
stopMatchmaking
    :: Text -- ^ 'smTicketId'
    -> StopMatchmaking
stopMatchmaking pTicketId_ = StopMatchmaking' {_smTicketId = pTicketId_}


-- | Unique identifier for a matchmaking ticket.
smTicketId :: Lens' StopMatchmaking Text
smTicketId = lens _smTicketId (\ s a -> s{_smTicketId = a})

instance AWSRequest StopMatchmaking where
        type Rs StopMatchmaking = StopMatchmakingResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 StopMatchmakingResponse' <$> (pure (fromEnum s)))

instance Hashable StopMatchmaking where

instance NFData StopMatchmaking where

instance ToHeaders StopMatchmaking where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StopMatchmaking" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopMatchmaking where
        toJSON StopMatchmaking'{..}
          = object
              (catMaybes [Just ("TicketId" .= _smTicketId)])

instance ToPath StopMatchmaking where
        toPath = const "/"

instance ToQuery StopMatchmaking where
        toQuery = const mempty

-- | /See:/ 'stopMatchmakingResponse' smart constructor.
newtype StopMatchmakingResponse = StopMatchmakingResponse'
  { _smrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopMatchmakingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsResponseStatus' - -- | The response status code.
stopMatchmakingResponse
    :: Int -- ^ 'smrsResponseStatus'
    -> StopMatchmakingResponse
stopMatchmakingResponse pResponseStatus_ =
  StopMatchmakingResponse' {_smrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
smrsResponseStatus :: Lens' StopMatchmakingResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\ s a -> s{_smrsResponseStatus = a})

instance NFData StopMatchmakingResponse where

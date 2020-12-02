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
-- Module      : Network.AWS.GameLift.ListGameServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Retrieves information on all game servers that are currently active in a specified game server group. You can opt to sort the list by game server age. Use the pagination parameters to retrieve results in a set of sequential segments.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
--     * 'RegisterGameServer'
--
--     * 'ListGameServers'
--
--     * 'ClaimGameServer'
--
--     * 'DescribeGameServer'
--
--     * 'UpdateGameServer'
--
--     * 'DeregisterGameServer'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListGameServers
  ( -- * Creating a Request
    listGameServers,
    ListGameServers,

    -- * Request Lenses
    lgsNextToken,
    lgsSortOrder,
    lgsLimit,
    lgsGameServerGroupName,

    -- * Destructuring the Response
    listGameServersResponse,
    ListGameServersResponse,

    -- * Response Lenses
    lgsrsGameServers,
    lgsrsNextToken,
    lgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGameServers' smart constructor.
data ListGameServers = ListGameServers'
  { _lgsNextToken ::
      !(Maybe Text),
    _lgsSortOrder :: !(Maybe SortOrder),
    _lgsLimit :: !(Maybe Nat),
    _lgsGameServerGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGameServers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgsNextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- * 'lgsSortOrder' - Indicates how to sort the returned data based on game server registration timestamp. Use ASCENDING to retrieve oldest game servers first, or use DESCENDING to retrieve newest game servers first. If this parameter is left empty, game servers are returned in no particular order.
--
-- * 'lgsLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- * 'lgsGameServerGroupName' - An identifier for the game server group to retrieve a list of game servers from. Use either the 'GameServerGroup' name or ARN value.
listGameServers ::
  -- | 'lgsGameServerGroupName'
  Text ->
  ListGameServers
listGameServers pGameServerGroupName_ =
  ListGameServers'
    { _lgsNextToken = Nothing,
      _lgsSortOrder = Nothing,
      _lgsLimit = Nothing,
      _lgsGameServerGroupName = pGameServerGroupName_
    }

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
lgsNextToken :: Lens' ListGameServers (Maybe Text)
lgsNextToken = lens _lgsNextToken (\s a -> s {_lgsNextToken = a})

-- | Indicates how to sort the returned data based on game server registration timestamp. Use ASCENDING to retrieve oldest game servers first, or use DESCENDING to retrieve newest game servers first. If this parameter is left empty, game servers are returned in no particular order.
lgsSortOrder :: Lens' ListGameServers (Maybe SortOrder)
lgsSortOrder = lens _lgsSortOrder (\s a -> s {_lgsSortOrder = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
lgsLimit :: Lens' ListGameServers (Maybe Natural)
lgsLimit = lens _lgsLimit (\s a -> s {_lgsLimit = a}) . mapping _Nat

-- | An identifier for the game server group to retrieve a list of game servers from. Use either the 'GameServerGroup' name or ARN value.
lgsGameServerGroupName :: Lens' ListGameServers Text
lgsGameServerGroupName = lens _lgsGameServerGroupName (\s a -> s {_lgsGameServerGroupName = a})

instance AWSPager ListGameServers where
  page rq rs
    | stop (rs ^. lgsrsNextToken) = Nothing
    | stop (rs ^. lgsrsGameServers) = Nothing
    | otherwise = Just $ rq & lgsNextToken .~ rs ^. lgsrsNextToken

instance AWSRequest ListGameServers where
  type Rs ListGameServers = ListGameServersResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ListGameServersResponse'
            <$> (x .?> "GameServers" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListGameServers

instance NFData ListGameServers

instance ToHeaders ListGameServers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ListGameServers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListGameServers where
  toJSON ListGameServers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lgsNextToken,
            ("SortOrder" .=) <$> _lgsSortOrder,
            ("Limit" .=) <$> _lgsLimit,
            Just ("GameServerGroupName" .= _lgsGameServerGroupName)
          ]
      )

instance ToPath ListGameServers where
  toPath = const "/"

instance ToQuery ListGameServers where
  toQuery = const mempty

-- | /See:/ 'listGameServersResponse' smart constructor.
data ListGameServersResponse = ListGameServersResponse'
  { _lgsrsGameServers ::
      !(Maybe [GameServer]),
    _lgsrsNextToken :: !(Maybe Text),
    _lgsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGameServersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgsrsGameServers' - A collection of game server objects that match the request.
--
-- * 'lgsrsNextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'lgsrsResponseStatus' - -- | The response status code.
listGameServersResponse ::
  -- | 'lgsrsResponseStatus'
  Int ->
  ListGameServersResponse
listGameServersResponse pResponseStatus_ =
  ListGameServersResponse'
    { _lgsrsGameServers = Nothing,
      _lgsrsNextToken = Nothing,
      _lgsrsResponseStatus = pResponseStatus_
    }

-- | A collection of game server objects that match the request.
lgsrsGameServers :: Lens' ListGameServersResponse [GameServer]
lgsrsGameServers = lens _lgsrsGameServers (\s a -> s {_lgsrsGameServers = a}) . _Default . _Coerce

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
lgsrsNextToken :: Lens' ListGameServersResponse (Maybe Text)
lgsrsNextToken = lens _lgsrsNextToken (\s a -> s {_lgsrsNextToken = a})

-- | -- | The response status code.
lgsrsResponseStatus :: Lens' ListGameServersResponse Int
lgsrsResponseStatus = lens _lgsrsResponseStatus (\s a -> s {_lgsrsResponseStatus = a})

instance NFData ListGameServersResponse

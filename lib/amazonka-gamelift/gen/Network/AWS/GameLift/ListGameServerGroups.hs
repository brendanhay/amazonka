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
-- Module      : Network.AWS.GameLift.ListGameServerGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Retrieves information on all game servers groups that exist in the current AWS account for the selected Region. Use the pagination parameters to retrieve results in a set of sequential segments.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
--     * 'CreateGameServerGroup'
--
--     * 'ListGameServerGroups'
--
--     * 'DescribeGameServerGroup'
--
--     * 'UpdateGameServerGroup'
--
--     * 'DeleteGameServerGroup'
--
--     * 'ResumeGameServerGroup'
--
--     * 'SuspendGameServerGroup'
--
--     * 'DescribeGameServerInstances'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListGameServerGroups
  ( -- * Creating a Request
    listGameServerGroups,
    ListGameServerGroups,

    -- * Request Lenses
    lgsgNextToken,
    lgsgLimit,

    -- * Destructuring the Response
    listGameServerGroupsResponse,
    ListGameServerGroupsResponse,

    -- * Response Lenses
    lgsgrsGameServerGroups,
    lgsgrsNextToken,
    lgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGameServerGroups' smart constructor.
data ListGameServerGroups = ListGameServerGroups'
  { _lgsgNextToken ::
      !(Maybe Text),
    _lgsgLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGameServerGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgsgNextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- * 'lgsgLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
listGameServerGroups ::
  ListGameServerGroups
listGameServerGroups =
  ListGameServerGroups'
    { _lgsgNextToken = Nothing,
      _lgsgLimit = Nothing
    }

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
lgsgNextToken :: Lens' ListGameServerGroups (Maybe Text)
lgsgNextToken = lens _lgsgNextToken (\s a -> s {_lgsgNextToken = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
lgsgLimit :: Lens' ListGameServerGroups (Maybe Natural)
lgsgLimit = lens _lgsgLimit (\s a -> s {_lgsgLimit = a}) . mapping _Nat

instance AWSPager ListGameServerGroups where
  page rq rs
    | stop (rs ^. lgsgrsNextToken) = Nothing
    | stop (rs ^. lgsgrsGameServerGroups) = Nothing
    | otherwise = Just $ rq & lgsgNextToken .~ rs ^. lgsgrsNextToken

instance AWSRequest ListGameServerGroups where
  type Rs ListGameServerGroups = ListGameServerGroupsResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ListGameServerGroupsResponse'
            <$> (x .?> "GameServerGroups" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListGameServerGroups

instance NFData ListGameServerGroups

instance ToHeaders ListGameServerGroups where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ListGameServerGroups" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListGameServerGroups where
  toJSON ListGameServerGroups' {..} =
    object
      ( catMaybes
          [("NextToken" .=) <$> _lgsgNextToken, ("Limit" .=) <$> _lgsgLimit]
      )

instance ToPath ListGameServerGroups where
  toPath = const "/"

instance ToQuery ListGameServerGroups where
  toQuery = const mempty

-- | /See:/ 'listGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { _lgsgrsGameServerGroups ::
      !(Maybe [GameServerGroup]),
    _lgsgrsNextToken :: !(Maybe Text),
    _lgsgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGameServerGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgsgrsGameServerGroups' - A collection of game server group objects that match the request.
--
-- * 'lgsgrsNextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'lgsgrsResponseStatus' - -- | The response status code.
listGameServerGroupsResponse ::
  -- | 'lgsgrsResponseStatus'
  Int ->
  ListGameServerGroupsResponse
listGameServerGroupsResponse pResponseStatus_ =
  ListGameServerGroupsResponse'
    { _lgsgrsGameServerGroups = Nothing,
      _lgsgrsNextToken = Nothing,
      _lgsgrsResponseStatus = pResponseStatus_
    }

-- | A collection of game server group objects that match the request.
lgsgrsGameServerGroups :: Lens' ListGameServerGroupsResponse [GameServerGroup]
lgsgrsGameServerGroups = lens _lgsgrsGameServerGroups (\s a -> s {_lgsgrsGameServerGroups = a}) . _Default . _Coerce

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
lgsgrsNextToken :: Lens' ListGameServerGroupsResponse (Maybe Text)
lgsgrsNextToken = lens _lgsgrsNextToken (\s a -> s {_lgsgrsNextToken = a})

-- | -- | The response status code.
lgsgrsResponseStatus :: Lens' ListGameServerGroupsResponse Int
lgsgrsResponseStatus = lens _lgsgrsResponseStatus (\s a -> s {_lgsgrsResponseStatus = a})

instance NFData ListGameServerGroupsResponse

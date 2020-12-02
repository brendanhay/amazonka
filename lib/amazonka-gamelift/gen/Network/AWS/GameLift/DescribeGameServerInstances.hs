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
-- Module      : Network.AWS.GameLift.DescribeGameServerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Retrieves status information about the Amazon EC2 instances associated with a GameLift FleetIQ game server group. Use this operation to detect when instances are active or not available to host new game servers. If you are looking for instance configuration information, call 'DescribeGameServerGroup' or access the corresponding Auto Scaling group properties.
--
-- To request status for all instances in the game server group, provide a game server group ID only. To request status for specific instances, provide the game server group ID and one or more instance IDs. Use the pagination parameters to retrieve results in sequential segments. If successful, a collection of @GameServerInstance@ objects is returned.
--
-- This operation is not designed to be called with every game server claim request; this practice can cause you to exceed your API limit, which results in errors. Instead, as a best practice, cache the results and refresh your cache no more than once every 10 seconds.
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
module Network.AWS.GameLift.DescribeGameServerInstances
  ( -- * Creating a Request
    describeGameServerInstances,
    DescribeGameServerInstances,

    -- * Request Lenses
    dgsiNextToken,
    dgsiInstanceIds,
    dgsiLimit,
    dgsiGameServerGroupName,

    -- * Destructuring the Response
    describeGameServerInstancesResponse,
    DescribeGameServerInstancesResponse,

    -- * Response Lenses
    dgsirsGameServerInstances,
    dgsirsNextToken,
    dgsirsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGameServerInstances' smart constructor.
data DescribeGameServerInstances = DescribeGameServerInstances'
  { _dgsiNextToken ::
      !(Maybe Text),
    _dgsiInstanceIds ::
      !(Maybe (List1 Text)),
    _dgsiLimit :: !(Maybe Nat),
    _dgsiGameServerGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServerInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsiNextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- * 'dgsiInstanceIds' - The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty.
--
-- * 'dgsiLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- * 'dgsiGameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
describeGameServerInstances ::
  -- | 'dgsiGameServerGroupName'
  Text ->
  DescribeGameServerInstances
describeGameServerInstances pGameServerGroupName_ =
  DescribeGameServerInstances'
    { _dgsiNextToken = Nothing,
      _dgsiInstanceIds = Nothing,
      _dgsiLimit = Nothing,
      _dgsiGameServerGroupName = pGameServerGroupName_
    }

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
dgsiNextToken :: Lens' DescribeGameServerInstances (Maybe Text)
dgsiNextToken = lens _dgsiNextToken (\s a -> s {_dgsiNextToken = a})

-- | The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty.
dgsiInstanceIds :: Lens' DescribeGameServerInstances (Maybe (NonEmpty Text))
dgsiInstanceIds = lens _dgsiInstanceIds (\s a -> s {_dgsiInstanceIds = a}) . mapping _List1

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
dgsiLimit :: Lens' DescribeGameServerInstances (Maybe Natural)
dgsiLimit = lens _dgsiLimit (\s a -> s {_dgsiLimit = a}) . mapping _Nat

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
dgsiGameServerGroupName :: Lens' DescribeGameServerInstances Text
dgsiGameServerGroupName = lens _dgsiGameServerGroupName (\s a -> s {_dgsiGameServerGroupName = a})

instance AWSPager DescribeGameServerInstances where
  page rq rs
    | stop (rs ^. dgsirsNextToken) = Nothing
    | stop (rs ^. dgsirsGameServerInstances) = Nothing
    | otherwise = Just $ rq & dgsiNextToken .~ rs ^. dgsirsNextToken

instance AWSRequest DescribeGameServerInstances where
  type
    Rs DescribeGameServerInstances =
      DescribeGameServerInstancesResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeGameServerInstancesResponse'
            <$> (x .?> "GameServerInstances" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeGameServerInstances

instance NFData DescribeGameServerInstances

instance ToHeaders DescribeGameServerInstances where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.DescribeGameServerInstances" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeGameServerInstances where
  toJSON DescribeGameServerInstances' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dgsiNextToken,
            ("InstanceIds" .=) <$> _dgsiInstanceIds,
            ("Limit" .=) <$> _dgsiLimit,
            Just ("GameServerGroupName" .= _dgsiGameServerGroupName)
          ]
      )

instance ToPath DescribeGameServerInstances where
  toPath = const "/"

instance ToQuery DescribeGameServerInstances where
  toQuery = const mempty

-- | /See:/ 'describeGameServerInstancesResponse' smart constructor.
data DescribeGameServerInstancesResponse = DescribeGameServerInstancesResponse'
  { _dgsirsGameServerInstances ::
      !( Maybe
           [GameServerInstance]
       ),
    _dgsirsNextToken ::
      !(Maybe Text),
    _dgsirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServerInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsirsGameServerInstances' - The collection of requested game server instances.
--
-- * 'dgsirsNextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'dgsirsResponseStatus' - -- | The response status code.
describeGameServerInstancesResponse ::
  -- | 'dgsirsResponseStatus'
  Int ->
  DescribeGameServerInstancesResponse
describeGameServerInstancesResponse pResponseStatus_ =
  DescribeGameServerInstancesResponse'
    { _dgsirsGameServerInstances =
        Nothing,
      _dgsirsNextToken = Nothing,
      _dgsirsResponseStatus = pResponseStatus_
    }

-- | The collection of requested game server instances.
dgsirsGameServerInstances :: Lens' DescribeGameServerInstancesResponse [GameServerInstance]
dgsirsGameServerInstances = lens _dgsirsGameServerInstances (\s a -> s {_dgsirsGameServerInstances = a}) . _Default . _Coerce

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
dgsirsNextToken :: Lens' DescribeGameServerInstancesResponse (Maybe Text)
dgsirsNextToken = lens _dgsirsNextToken (\s a -> s {_dgsirsNextToken = a})

-- | -- | The response status code.
dgsirsResponseStatus :: Lens' DescribeGameServerInstancesResponse Int
dgsirsResponseStatus = lens _dgsirsResponseStatus (\s a -> s {_dgsirsResponseStatus = a})

instance NFData DescribeGameServerInstancesResponse

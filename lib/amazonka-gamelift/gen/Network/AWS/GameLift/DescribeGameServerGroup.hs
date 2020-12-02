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
-- Module      : Network.AWS.GameLift.DescribeGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Retrieves information on a game server group. This operation returns only properties related to GameLift FleetIQ. To view or update properties for the corresponding Auto Scaling group, such as launch template, auto scaling policies, and maximum/minimum group size, access the Auto Scaling group directly.
--
-- To get attributes for a game server group, provide a group name or ARN value. If successful, a 'GameServerGroup' object is returned.
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
module Network.AWS.GameLift.DescribeGameServerGroup
  ( -- * Creating a Request
    describeGameServerGroup,
    DescribeGameServerGroup,

    -- * Request Lenses
    desGameServerGroupName,

    -- * Destructuring the Response
    describeGameServerGroupResponse,
    DescribeGameServerGroupResponse,

    -- * Response Lenses
    desrsGameServerGroup,
    desrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGameServerGroup' smart constructor.
newtype DescribeGameServerGroup = DescribeGameServerGroup'
  { _desGameServerGroupName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desGameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
describeGameServerGroup ::
  -- | 'desGameServerGroupName'
  Text ->
  DescribeGameServerGroup
describeGameServerGroup pGameServerGroupName_ =
  DescribeGameServerGroup'
    { _desGameServerGroupName =
        pGameServerGroupName_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
desGameServerGroupName :: Lens' DescribeGameServerGroup Text
desGameServerGroupName = lens _desGameServerGroupName (\s a -> s {_desGameServerGroupName = a})

instance AWSRequest DescribeGameServerGroup where
  type Rs DescribeGameServerGroup = DescribeGameServerGroupResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeGameServerGroupResponse'
            <$> (x .?> "GameServerGroup") <*> (pure (fromEnum s))
      )

instance Hashable DescribeGameServerGroup

instance NFData DescribeGameServerGroup

instance ToHeaders DescribeGameServerGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.DescribeGameServerGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeGameServerGroup where
  toJSON DescribeGameServerGroup' {..} =
    object
      ( catMaybes
          [Just ("GameServerGroupName" .= _desGameServerGroupName)]
      )

instance ToPath DescribeGameServerGroup where
  toPath = const "/"

instance ToQuery DescribeGameServerGroup where
  toQuery = const mempty

-- | /See:/ 'describeGameServerGroupResponse' smart constructor.
data DescribeGameServerGroupResponse = DescribeGameServerGroupResponse'
  { _desrsGameServerGroup ::
      !(Maybe GameServerGroup),
    _desrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsGameServerGroup' - An object with the property settings for the requested game server group resource.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeGameServerGroupResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeGameServerGroupResponse
describeGameServerGroupResponse pResponseStatus_ =
  DescribeGameServerGroupResponse'
    { _desrsGameServerGroup = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | An object with the property settings for the requested game server group resource.
desrsGameServerGroup :: Lens' DescribeGameServerGroupResponse (Maybe GameServerGroup)
desrsGameServerGroup = lens _desrsGameServerGroup (\s a -> s {_desrsGameServerGroup = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeGameServerGroupResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeGameServerGroupResponse

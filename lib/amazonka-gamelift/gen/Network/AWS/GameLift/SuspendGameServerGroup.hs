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
-- Module      : Network.AWS.GameLift.SuspendGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Temporarily stops activity on a game server group without terminating instances or the game server group. You can restart activity by calling 'ResumeGameServerGroup' . You can suspend the following activity:
--
--     * __Instance type replacement__ - This activity evaluates the current game hosting viability of all Spot instance types that are defined for the game server group. It updates the Auto Scaling group to remove nonviable Spot Instance types, which have a higher chance of game server interruptions. It then balances capacity across the remaining viable Spot Instance types. When this activity is suspended, the Auto Scaling group continues with its current balance, regardless of viability. Instance protection, utilization metrics, and capacity scaling activities continue to be active.
--
--
--
-- To suspend activity, specify a game server group ARN and the type of activity to be suspended. If successful, a 'GameServerGroup' object is returned showing that the activity is listed in @SuspendedActions@ .
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
module Network.AWS.GameLift.SuspendGameServerGroup
  ( -- * Creating a Request
    suspendGameServerGroup,
    SuspendGameServerGroup,

    -- * Request Lenses
    sgsgGameServerGroupName,
    sgsgSuspendActions,

    -- * Destructuring the Response
    suspendGameServerGroupResponse,
    SuspendGameServerGroupResponse,

    -- * Response Lenses
    sgsgrsGameServerGroup,
    sgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'suspendGameServerGroup' smart constructor.
data SuspendGameServerGroup = SuspendGameServerGroup'
  { _sgsgGameServerGroupName ::
      !Text,
    _sgsgSuspendActions ::
      !(List1 GameServerGroupAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgsgGameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'sgsgSuspendActions' - The activity to suspend for this game server group.
suspendGameServerGroup ::
  -- | 'sgsgGameServerGroupName'
  Text ->
  -- | 'sgsgSuspendActions'
  NonEmpty GameServerGroupAction ->
  SuspendGameServerGroup
suspendGameServerGroup pGameServerGroupName_ pSuspendActions_ =
  SuspendGameServerGroup'
    { _sgsgGameServerGroupName =
        pGameServerGroupName_,
      _sgsgSuspendActions = _List1 # pSuspendActions_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
sgsgGameServerGroupName :: Lens' SuspendGameServerGroup Text
sgsgGameServerGroupName = lens _sgsgGameServerGroupName (\s a -> s {_sgsgGameServerGroupName = a})

-- | The activity to suspend for this game server group.
sgsgSuspendActions :: Lens' SuspendGameServerGroup (NonEmpty GameServerGroupAction)
sgsgSuspendActions = lens _sgsgSuspendActions (\s a -> s {_sgsgSuspendActions = a}) . _List1

instance AWSRequest SuspendGameServerGroup where
  type Rs SuspendGameServerGroup = SuspendGameServerGroupResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          SuspendGameServerGroupResponse'
            <$> (x .?> "GameServerGroup") <*> (pure (fromEnum s))
      )

instance Hashable SuspendGameServerGroup

instance NFData SuspendGameServerGroup

instance ToHeaders SuspendGameServerGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.SuspendGameServerGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SuspendGameServerGroup where
  toJSON SuspendGameServerGroup' {..} =
    object
      ( catMaybes
          [ Just ("GameServerGroupName" .= _sgsgGameServerGroupName),
            Just ("SuspendActions" .= _sgsgSuspendActions)
          ]
      )

instance ToPath SuspendGameServerGroup where
  toPath = const "/"

instance ToQuery SuspendGameServerGroup where
  toQuery = const mempty

-- | /See:/ 'suspendGameServerGroupResponse' smart constructor.
data SuspendGameServerGroupResponse = SuspendGameServerGroupResponse'
  { _sgsgrsGameServerGroup ::
      !(Maybe GameServerGroup),
    _sgsgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgsgrsGameServerGroup' - An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
--
-- * 'sgsgrsResponseStatus' - -- | The response status code.
suspendGameServerGroupResponse ::
  -- | 'sgsgrsResponseStatus'
  Int ->
  SuspendGameServerGroupResponse
suspendGameServerGroupResponse pResponseStatus_ =
  SuspendGameServerGroupResponse'
    { _sgsgrsGameServerGroup = Nothing,
      _sgsgrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
sgsgrsGameServerGroup :: Lens' SuspendGameServerGroupResponse (Maybe GameServerGroup)
sgsgrsGameServerGroup = lens _sgsgrsGameServerGroup (\s a -> s {_sgsgrsGameServerGroup = a})

-- | -- | The response status code.
sgsgrsResponseStatus :: Lens' SuspendGameServerGroupResponse Int
sgsgrsResponseStatus = lens _sgsgrsResponseStatus (\s a -> s {_sgsgrsResponseStatus = a})

instance NFData SuspendGameServerGroupResponse

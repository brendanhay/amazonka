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
-- Module      : Network.AWS.GameLift.ResumeGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Reinstates activity on a game server group after it has been suspended. A game server group might be suspended by the'SuspendGameServerGroup' operation, or it might be suspended involuntarily due to a configuration problem. In the second case, you can manually resume activity on the group once the configuration problem has been resolved. Refer to the game server group status and status reason for more information on why group activity is suspended.
--
-- To resume activity, specify a game server group ARN and the type of activity to be resumed. If successful, a 'GameServerGroup' object is returned showing that the resumed activity is no longer listed in @SuspendedActions@ .
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
module Network.AWS.GameLift.ResumeGameServerGroup
  ( -- * Creating a Request
    resumeGameServerGroup,
    ResumeGameServerGroup,

    -- * Request Lenses
    rgsgGameServerGroupName,
    rgsgResumeActions,

    -- * Destructuring the Response
    resumeGameServerGroupResponse,
    ResumeGameServerGroupResponse,

    -- * Response Lenses
    rgsgrsGameServerGroup,
    rgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resumeGameServerGroup' smart constructor.
data ResumeGameServerGroup = ResumeGameServerGroup'
  { _rgsgGameServerGroupName ::
      !Text,
    _rgsgResumeActions ::
      !(List1 GameServerGroupAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsgGameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'rgsgResumeActions' - The activity to resume for this game server group.
resumeGameServerGroup ::
  -- | 'rgsgGameServerGroupName'
  Text ->
  -- | 'rgsgResumeActions'
  NonEmpty GameServerGroupAction ->
  ResumeGameServerGroup
resumeGameServerGroup pGameServerGroupName_ pResumeActions_ =
  ResumeGameServerGroup'
    { _rgsgGameServerGroupName =
        pGameServerGroupName_,
      _rgsgResumeActions = _List1 # pResumeActions_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
rgsgGameServerGroupName :: Lens' ResumeGameServerGroup Text
rgsgGameServerGroupName = lens _rgsgGameServerGroupName (\s a -> s {_rgsgGameServerGroupName = a})

-- | The activity to resume for this game server group.
rgsgResumeActions :: Lens' ResumeGameServerGroup (NonEmpty GameServerGroupAction)
rgsgResumeActions = lens _rgsgResumeActions (\s a -> s {_rgsgResumeActions = a}) . _List1

instance AWSRequest ResumeGameServerGroup where
  type Rs ResumeGameServerGroup = ResumeGameServerGroupResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ResumeGameServerGroupResponse'
            <$> (x .?> "GameServerGroup") <*> (pure (fromEnum s))
      )

instance Hashable ResumeGameServerGroup

instance NFData ResumeGameServerGroup

instance ToHeaders ResumeGameServerGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.ResumeGameServerGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ResumeGameServerGroup where
  toJSON ResumeGameServerGroup' {..} =
    object
      ( catMaybes
          [ Just ("GameServerGroupName" .= _rgsgGameServerGroupName),
            Just ("ResumeActions" .= _rgsgResumeActions)
          ]
      )

instance ToPath ResumeGameServerGroup where
  toPath = const "/"

instance ToQuery ResumeGameServerGroup where
  toQuery = const mempty

-- | /See:/ 'resumeGameServerGroupResponse' smart constructor.
data ResumeGameServerGroupResponse = ResumeGameServerGroupResponse'
  { _rgsgrsGameServerGroup ::
      !(Maybe GameServerGroup),
    _rgsgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsgrsGameServerGroup' - An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
--
-- * 'rgsgrsResponseStatus' - -- | The response status code.
resumeGameServerGroupResponse ::
  -- | 'rgsgrsResponseStatus'
  Int ->
  ResumeGameServerGroupResponse
resumeGameServerGroupResponse pResponseStatus_ =
  ResumeGameServerGroupResponse'
    { _rgsgrsGameServerGroup = Nothing,
      _rgsgrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
rgsgrsGameServerGroup :: Lens' ResumeGameServerGroupResponse (Maybe GameServerGroup)
rgsgrsGameServerGroup = lens _rgsgrsGameServerGroup (\s a -> s {_rgsgrsGameServerGroup = a})

-- | -- | The response status code.
rgsgrsResponseStatus :: Lens' ResumeGameServerGroupResponse Int
rgsgrsResponseStatus = lens _rgsgrsResponseStatus (\s a -> s {_rgsgrsResponseStatus = a})

instance NFData ResumeGameServerGroupResponse

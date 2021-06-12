{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ResumeGameServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Reinstates activity on a game server group after it has been suspended.
-- A game server group might be suspended by theSuspendGameServerGroup
-- operation, or it might be suspended involuntarily due to a configuration
-- problem. In the second case, you can manually resume activity on the
-- group once the configuration problem has been resolved. Refer to the
-- game server group status and status reason for more information on why
-- group activity is suspended.
--
-- To resume activity, specify a game server group ARN and the type of
-- activity to be resumed. If successful, a GameServerGroup object is
-- returned showing that the resumed activity is no longer listed in
-- @SuspendedActions@.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
-- -   CreateGameServerGroup
--
-- -   ListGameServerGroups
--
-- -   DescribeGameServerGroup
--
-- -   UpdateGameServerGroup
--
-- -   DeleteGameServerGroup
--
-- -   ResumeGameServerGroup
--
-- -   SuspendGameServerGroup
--
-- -   DescribeGameServerInstances
module Network.AWS.GameLift.ResumeGameServerGroup
  ( -- * Creating a Request
    ResumeGameServerGroup (..),
    newResumeGameServerGroup,

    -- * Request Lenses
    resumeGameServerGroup_gameServerGroupName,
    resumeGameServerGroup_resumeActions,

    -- * Destructuring the Response
    ResumeGameServerGroupResponse (..),
    newResumeGameServerGroupResponse,

    -- * Response Lenses
    resumeGameServerGroupResponse_gameServerGroup,
    resumeGameServerGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResumeGameServerGroup' smart constructor.
data ResumeGameServerGroup = ResumeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the
    -- GameServerGroup name or ARN value.
    gameServerGroupName :: Core.Text,
    -- | The activity to resume for this game server group.
    resumeActions :: Core.NonEmpty GameServerGroupAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResumeGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'resumeGameServerGroup_gameServerGroupName' - A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
--
-- 'resumeActions', 'resumeGameServerGroup_resumeActions' - The activity to resume for this game server group.
newResumeGameServerGroup ::
  -- | 'gameServerGroupName'
  Core.Text ->
  -- | 'resumeActions'
  Core.NonEmpty GameServerGroupAction ->
  ResumeGameServerGroup
newResumeGameServerGroup
  pGameServerGroupName_
  pResumeActions_ =
    ResumeGameServerGroup'
      { gameServerGroupName =
          pGameServerGroupName_,
        resumeActions = Lens._Coerce Lens.# pResumeActions_
      }

-- | A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
resumeGameServerGroup_gameServerGroupName :: Lens.Lens' ResumeGameServerGroup Core.Text
resumeGameServerGroup_gameServerGroupName = Lens.lens (\ResumeGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@ResumeGameServerGroup' {} a -> s {gameServerGroupName = a} :: ResumeGameServerGroup)

-- | The activity to resume for this game server group.
resumeGameServerGroup_resumeActions :: Lens.Lens' ResumeGameServerGroup (Core.NonEmpty GameServerGroupAction)
resumeGameServerGroup_resumeActions = Lens.lens (\ResumeGameServerGroup' {resumeActions} -> resumeActions) (\s@ResumeGameServerGroup' {} a -> s {resumeActions = a} :: ResumeGameServerGroup) Core.. Lens._Coerce

instance Core.AWSRequest ResumeGameServerGroup where
  type
    AWSResponse ResumeGameServerGroup =
      ResumeGameServerGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeGameServerGroupResponse'
            Core.<$> (x Core..?> "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResumeGameServerGroup

instance Core.NFData ResumeGameServerGroup

instance Core.ToHeaders ResumeGameServerGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.ResumeGameServerGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResumeGameServerGroup where
  toJSON ResumeGameServerGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("ResumeActions" Core..= resumeActions)
          ]
      )

instance Core.ToPath ResumeGameServerGroup where
  toPath = Core.const "/"

instance Core.ToQuery ResumeGameServerGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newResumeGameServerGroupResponse' smart constructor.
data ResumeGameServerGroupResponse = ResumeGameServerGroupResponse'
  { -- | An object that describes the game server group resource, with the
    -- @SuspendedActions@ property updated to reflect the resumed activity.
    gameServerGroup :: Core.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResumeGameServerGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroup', 'resumeGameServerGroupResponse_gameServerGroup' - An object that describes the game server group resource, with the
-- @SuspendedActions@ property updated to reflect the resumed activity.
--
-- 'httpStatus', 'resumeGameServerGroupResponse_httpStatus' - The response's http status code.
newResumeGameServerGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResumeGameServerGroupResponse
newResumeGameServerGroupResponse pHttpStatus_ =
  ResumeGameServerGroupResponse'
    { gameServerGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the game server group resource, with the
-- @SuspendedActions@ property updated to reflect the resumed activity.
resumeGameServerGroupResponse_gameServerGroup :: Lens.Lens' ResumeGameServerGroupResponse (Core.Maybe GameServerGroup)
resumeGameServerGroupResponse_gameServerGroup = Lens.lens (\ResumeGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@ResumeGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: ResumeGameServerGroupResponse)

-- | The response's http status code.
resumeGameServerGroupResponse_httpStatus :: Lens.Lens' ResumeGameServerGroupResponse Core.Int
resumeGameServerGroupResponse_httpStatus = Lens.lens (\ResumeGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@ResumeGameServerGroupResponse' {} a -> s {httpStatus = a} :: ResumeGameServerGroupResponse)

instance Core.NFData ResumeGameServerGroupResponse

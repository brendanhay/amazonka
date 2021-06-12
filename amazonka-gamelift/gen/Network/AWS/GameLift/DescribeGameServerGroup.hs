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
-- Module      : Network.AWS.GameLift.DescribeGameServerGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Retrieves information on a game server group. This operation returns
-- only properties related to GameLift FleetIQ. To view or update
-- properties for the corresponding Auto Scaling group, such as launch
-- template, auto scaling policies, and maximum\/minimum group size, access
-- the Auto Scaling group directly.
--
-- To get attributes for a game server group, provide a group name or ARN
-- value. If successful, a GameServerGroup object is returned.
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
module Network.AWS.GameLift.DescribeGameServerGroup
  ( -- * Creating a Request
    DescribeGameServerGroup (..),
    newDescribeGameServerGroup,

    -- * Request Lenses
    describeGameServerGroup_gameServerGroupName,

    -- * Destructuring the Response
    DescribeGameServerGroupResponse (..),
    newDescribeGameServerGroupResponse,

    -- * Response Lenses
    describeGameServerGroupResponse_gameServerGroup,
    describeGameServerGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGameServerGroup' smart constructor.
data DescribeGameServerGroup = DescribeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the
    -- GameServerGroup name or ARN value.
    gameServerGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'describeGameServerGroup_gameServerGroupName' - A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
newDescribeGameServerGroup ::
  -- | 'gameServerGroupName'
  Core.Text ->
  DescribeGameServerGroup
newDescribeGameServerGroup pGameServerGroupName_ =
  DescribeGameServerGroup'
    { gameServerGroupName =
        pGameServerGroupName_
    }

-- | A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
describeGameServerGroup_gameServerGroupName :: Lens.Lens' DescribeGameServerGroup Core.Text
describeGameServerGroup_gameServerGroupName = Lens.lens (\DescribeGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@DescribeGameServerGroup' {} a -> s {gameServerGroupName = a} :: DescribeGameServerGroup)

instance Core.AWSRequest DescribeGameServerGroup where
  type
    AWSResponse DescribeGameServerGroup =
      DescribeGameServerGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerGroupResponse'
            Core.<$> (x Core..?> "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGameServerGroup

instance Core.NFData DescribeGameServerGroup

instance Core.ToHeaders DescribeGameServerGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameServerGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeGameServerGroup where
  toJSON DescribeGameServerGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GameServerGroupName" Core..= gameServerGroupName)
          ]
      )

instance Core.ToPath DescribeGameServerGroup where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGameServerGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeGameServerGroupResponse' smart constructor.
data DescribeGameServerGroupResponse = DescribeGameServerGroupResponse'
  { -- | An object with the property settings for the requested game server group
    -- resource.
    gameServerGroup :: Core.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameServerGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroup', 'describeGameServerGroupResponse_gameServerGroup' - An object with the property settings for the requested game server group
-- resource.
--
-- 'httpStatus', 'describeGameServerGroupResponse_httpStatus' - The response's http status code.
newDescribeGameServerGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGameServerGroupResponse
newDescribeGameServerGroupResponse pHttpStatus_ =
  DescribeGameServerGroupResponse'
    { gameServerGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object with the property settings for the requested game server group
-- resource.
describeGameServerGroupResponse_gameServerGroup :: Lens.Lens' DescribeGameServerGroupResponse (Core.Maybe GameServerGroup)
describeGameServerGroupResponse_gameServerGroup = Lens.lens (\DescribeGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@DescribeGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: DescribeGameServerGroupResponse)

-- | The response's http status code.
describeGameServerGroupResponse_httpStatus :: Lens.Lens' DescribeGameServerGroupResponse Core.Int
describeGameServerGroupResponse_httpStatus = Lens.lens (\DescribeGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeGameServerGroupResponse' {} a -> s {httpStatus = a} :: DescribeGameServerGroupResponse)

instance Core.NFData DescribeGameServerGroupResponse

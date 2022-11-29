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
-- Module      : Amazonka.GameLift.SuspendGameServerGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Temporarily stops activity on a game server group without terminating
-- instances or the game server group. You can restart activity by calling
-- ResumeGameServerGroup. You can suspend the following activity:
--
-- -   __Instance type replacement__ - This activity evaluates the current
--     game hosting viability of all Spot instance types that are defined
--     for the game server group. It updates the Auto Scaling group to
--     remove nonviable Spot Instance types, which have a higher chance of
--     game server interruptions. It then balances capacity across the
--     remaining viable Spot Instance types. When this activity is
--     suspended, the Auto Scaling group continues with its current
--     balance, regardless of viability. Instance protection, utilization
--     metrics, and capacity scaling activities continue to be active.
--
-- To suspend activity, specify a game server group ARN and the type of
-- activity to be suspended. If successful, a GameServerGroup object is
-- returned showing that the activity is listed in @SuspendedActions@.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related actions__
--
-- CreateGameServerGroup | ListGameServerGroups | DescribeGameServerGroup |
-- UpdateGameServerGroup | DeleteGameServerGroup | ResumeGameServerGroup |
-- SuspendGameServerGroup | DescribeGameServerInstances |
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/reference-awssdk-fleetiq.html All APIs by task>
module Amazonka.GameLift.SuspendGameServerGroup
  ( -- * Creating a Request
    SuspendGameServerGroup (..),
    newSuspendGameServerGroup,

    -- * Request Lenses
    suspendGameServerGroup_gameServerGroupName,
    suspendGameServerGroup_suspendActions,

    -- * Destructuring the Response
    SuspendGameServerGroupResponse (..),
    newSuspendGameServerGroupResponse,

    -- * Response Lenses
    suspendGameServerGroupResponse_gameServerGroup,
    suspendGameServerGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSuspendGameServerGroup' smart constructor.
data SuspendGameServerGroup = SuspendGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the
    -- GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text,
    -- | The activity to suspend for this game server group.
    suspendActions :: Prelude.NonEmpty GameServerGroupAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuspendGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'suspendGameServerGroup_gameServerGroupName' - A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
--
-- 'suspendActions', 'suspendGameServerGroup_suspendActions' - The activity to suspend for this game server group.
newSuspendGameServerGroup ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  -- | 'suspendActions'
  Prelude.NonEmpty GameServerGroupAction ->
  SuspendGameServerGroup
newSuspendGameServerGroup
  pGameServerGroupName_
  pSuspendActions_ =
    SuspendGameServerGroup'
      { gameServerGroupName =
          pGameServerGroupName_,
        suspendActions =
          Lens.coerced Lens.# pSuspendActions_
      }

-- | A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
suspendGameServerGroup_gameServerGroupName :: Lens.Lens' SuspendGameServerGroup Prelude.Text
suspendGameServerGroup_gameServerGroupName = Lens.lens (\SuspendGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@SuspendGameServerGroup' {} a -> s {gameServerGroupName = a} :: SuspendGameServerGroup)

-- | The activity to suspend for this game server group.
suspendGameServerGroup_suspendActions :: Lens.Lens' SuspendGameServerGroup (Prelude.NonEmpty GameServerGroupAction)
suspendGameServerGroup_suspendActions = Lens.lens (\SuspendGameServerGroup' {suspendActions} -> suspendActions) (\s@SuspendGameServerGroup' {} a -> s {suspendActions = a} :: SuspendGameServerGroup) Prelude.. Lens.coerced

instance Core.AWSRequest SuspendGameServerGroup where
  type
    AWSResponse SuspendGameServerGroup =
      SuspendGameServerGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SuspendGameServerGroupResponse'
            Prelude.<$> (x Core..?> "GameServerGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SuspendGameServerGroup where
  hashWithSalt _salt SuspendGameServerGroup' {..} =
    _salt `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` suspendActions

instance Prelude.NFData SuspendGameServerGroup where
  rnf SuspendGameServerGroup' {..} =
    Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf suspendActions

instance Core.ToHeaders SuspendGameServerGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.SuspendGameServerGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SuspendGameServerGroup where
  toJSON SuspendGameServerGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameServerGroupName" Core..= gameServerGroupName),
            Prelude.Just
              ("SuspendActions" Core..= suspendActions)
          ]
      )

instance Core.ToPath SuspendGameServerGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery SuspendGameServerGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSuspendGameServerGroupResponse' smart constructor.
data SuspendGameServerGroupResponse = SuspendGameServerGroupResponse'
  { -- | An object that describes the game server group resource, with the
    -- @SuspendedActions@ property updated to reflect the suspended activity.
    gameServerGroup :: Prelude.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuspendGameServerGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroup', 'suspendGameServerGroupResponse_gameServerGroup' - An object that describes the game server group resource, with the
-- @SuspendedActions@ property updated to reflect the suspended activity.
--
-- 'httpStatus', 'suspendGameServerGroupResponse_httpStatus' - The response's http status code.
newSuspendGameServerGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SuspendGameServerGroupResponse
newSuspendGameServerGroupResponse pHttpStatus_ =
  SuspendGameServerGroupResponse'
    { gameServerGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the game server group resource, with the
-- @SuspendedActions@ property updated to reflect the suspended activity.
suspendGameServerGroupResponse_gameServerGroup :: Lens.Lens' SuspendGameServerGroupResponse (Prelude.Maybe GameServerGroup)
suspendGameServerGroupResponse_gameServerGroup = Lens.lens (\SuspendGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@SuspendGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: SuspendGameServerGroupResponse)

-- | The response's http status code.
suspendGameServerGroupResponse_httpStatus :: Lens.Lens' SuspendGameServerGroupResponse Prelude.Int
suspendGameServerGroupResponse_httpStatus = Lens.lens (\SuspendGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@SuspendGameServerGroupResponse' {} a -> s {httpStatus = a} :: SuspendGameServerGroupResponse)

instance
  Prelude.NFData
    SuspendGameServerGroupResponse
  where
  rnf SuspendGameServerGroupResponse' {..} =
    Prelude.rnf gameServerGroup
      `Prelude.seq` Prelude.rnf httpStatus

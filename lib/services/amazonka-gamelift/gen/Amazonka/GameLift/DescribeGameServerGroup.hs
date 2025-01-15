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
-- Module      : Amazonka.GameLift.DescribeGameServerGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Retrieves information on a game server group. This operation returns
-- only properties related to GameLift FleetIQ. To view or update
-- properties for the corresponding Auto Scaling group, such as launch
-- template, auto scaling policies, and maximum\/minimum group size, access
-- the Auto Scaling group directly.
--
-- To get attributes for a game server group, provide a group name or ARN
-- value. If successful, a @GameServerGroup@ object is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
module Amazonka.GameLift.DescribeGameServerGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGameServerGroup' smart constructor.
data DescribeGameServerGroup = DescribeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the name or
    -- ARN value.
    gameServerGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameServerGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'describeGameServerGroup_gameServerGroupName' - A unique identifier for the game server group. Use either the name or
-- ARN value.
newDescribeGameServerGroup ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  DescribeGameServerGroup
newDescribeGameServerGroup pGameServerGroupName_ =
  DescribeGameServerGroup'
    { gameServerGroupName =
        pGameServerGroupName_
    }

-- | A unique identifier for the game server group. Use either the name or
-- ARN value.
describeGameServerGroup_gameServerGroupName :: Lens.Lens' DescribeGameServerGroup Prelude.Text
describeGameServerGroup_gameServerGroupName = Lens.lens (\DescribeGameServerGroup' {gameServerGroupName} -> gameServerGroupName) (\s@DescribeGameServerGroup' {} a -> s {gameServerGroupName = a} :: DescribeGameServerGroup)

instance Core.AWSRequest DescribeGameServerGroup where
  type
    AWSResponse DescribeGameServerGroup =
      DescribeGameServerGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerGroupResponse'
            Prelude.<$> (x Data..?> "GameServerGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameServerGroup where
  hashWithSalt _salt DescribeGameServerGroup' {..} =
    _salt `Prelude.hashWithSalt` gameServerGroupName

instance Prelude.NFData DescribeGameServerGroup where
  rnf DescribeGameServerGroup' {..} =
    Prelude.rnf gameServerGroupName

instance Data.ToHeaders DescribeGameServerGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeGameServerGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGameServerGroup where
  toJSON DescribeGameServerGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName)
          ]
      )

instance Data.ToPath DescribeGameServerGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGameServerGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGameServerGroupResponse' smart constructor.
data DescribeGameServerGroupResponse = DescribeGameServerGroupResponse'
  { -- | An object with the property settings for the requested game server group
    -- resource.
    gameServerGroup :: Prelude.Maybe GameServerGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeGameServerGroupResponse
newDescribeGameServerGroupResponse pHttpStatus_ =
  DescribeGameServerGroupResponse'
    { gameServerGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object with the property settings for the requested game server group
-- resource.
describeGameServerGroupResponse_gameServerGroup :: Lens.Lens' DescribeGameServerGroupResponse (Prelude.Maybe GameServerGroup)
describeGameServerGroupResponse_gameServerGroup = Lens.lens (\DescribeGameServerGroupResponse' {gameServerGroup} -> gameServerGroup) (\s@DescribeGameServerGroupResponse' {} a -> s {gameServerGroup = a} :: DescribeGameServerGroupResponse)

-- | The response's http status code.
describeGameServerGroupResponse_httpStatus :: Lens.Lens' DescribeGameServerGroupResponse Prelude.Int
describeGameServerGroupResponse_httpStatus = Lens.lens (\DescribeGameServerGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeGameServerGroupResponse' {} a -> s {httpStatus = a} :: DescribeGameServerGroupResponse)

instance
  Prelude.NFData
    DescribeGameServerGroupResponse
  where
  rnf DescribeGameServerGroupResponse' {..} =
    Prelude.rnf gameServerGroup `Prelude.seq`
      Prelude.rnf httpStatus

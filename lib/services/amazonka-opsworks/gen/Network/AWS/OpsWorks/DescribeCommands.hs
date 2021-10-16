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
-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the results of specified commands.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeCommands
  ( -- * Creating a Request
    DescribeCommands (..),
    newDescribeCommands,

    -- * Request Lenses
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommands_commandIds,

    -- * Destructuring the Response
    DescribeCommandsResponse (..),
    newDescribeCommandsResponse,

    -- * Response Lenses
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCommands' smart constructor.
data DescribeCommands = DescribeCommands'
  { -- | The deployment ID. If you include this parameter, @DescribeCommands@
    -- returns a description of the commands associated with the specified
    -- deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The instance ID. If you include this parameter, @DescribeCommands@
    -- returns a description of the commands associated with the specified
    -- instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | An array of command IDs. If you include this parameter,
    -- @DescribeCommands@ returns a description of the specified commands.
    -- Otherwise, it returns a description of every command.
    commandIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCommands' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'describeCommands_deploymentId' - The deployment ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- deployment.
--
-- 'instanceId', 'describeCommands_instanceId' - The instance ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- instance.
--
-- 'commandIds', 'describeCommands_commandIds' - An array of command IDs. If you include this parameter,
-- @DescribeCommands@ returns a description of the specified commands.
-- Otherwise, it returns a description of every command.
newDescribeCommands ::
  DescribeCommands
newDescribeCommands =
  DescribeCommands'
    { deploymentId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      commandIds = Prelude.Nothing
    }

-- | The deployment ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- deployment.
describeCommands_deploymentId :: Lens.Lens' DescribeCommands (Prelude.Maybe Prelude.Text)
describeCommands_deploymentId = Lens.lens (\DescribeCommands' {deploymentId} -> deploymentId) (\s@DescribeCommands' {} a -> s {deploymentId = a} :: DescribeCommands)

-- | The instance ID. If you include this parameter, @DescribeCommands@
-- returns a description of the commands associated with the specified
-- instance.
describeCommands_instanceId :: Lens.Lens' DescribeCommands (Prelude.Maybe Prelude.Text)
describeCommands_instanceId = Lens.lens (\DescribeCommands' {instanceId} -> instanceId) (\s@DescribeCommands' {} a -> s {instanceId = a} :: DescribeCommands)

-- | An array of command IDs. If you include this parameter,
-- @DescribeCommands@ returns a description of the specified commands.
-- Otherwise, it returns a description of every command.
describeCommands_commandIds :: Lens.Lens' DescribeCommands (Prelude.Maybe [Prelude.Text])
describeCommands_commandIds = Lens.lens (\DescribeCommands' {commandIds} -> commandIds) (\s@DescribeCommands' {} a -> s {commandIds = a} :: DescribeCommands) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeCommands where
  type
    AWSResponse DescribeCommands =
      DescribeCommandsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCommandsResponse'
            Prelude.<$> (x Core..?> "Commands" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCommands

instance Prelude.NFData DescribeCommands

instance Core.ToHeaders DescribeCommands where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeCommands" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCommands where
  toJSON DescribeCommands' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeploymentId" Core..=) Prelude.<$> deploymentId,
            ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("CommandIds" Core..=) Prelude.<$> commandIds
          ]
      )

instance Core.ToPath DescribeCommands where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCommands where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeCommands@ request.
--
-- /See:/ 'newDescribeCommandsResponse' smart constructor.
data DescribeCommandsResponse = DescribeCommandsResponse'
  { -- | An array of @Command@ objects that describe each of the specified
    -- commands.
    commands :: Prelude.Maybe [Command],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCommandsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commands', 'describeCommandsResponse_commands' - An array of @Command@ objects that describe each of the specified
-- commands.
--
-- 'httpStatus', 'describeCommandsResponse_httpStatus' - The response's http status code.
newDescribeCommandsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCommandsResponse
newDescribeCommandsResponse pHttpStatus_ =
  DescribeCommandsResponse'
    { commands =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Command@ objects that describe each of the specified
-- commands.
describeCommandsResponse_commands :: Lens.Lens' DescribeCommandsResponse (Prelude.Maybe [Command])
describeCommandsResponse_commands = Lens.lens (\DescribeCommandsResponse' {commands} -> commands) (\s@DescribeCommandsResponse' {} a -> s {commands = a} :: DescribeCommandsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCommandsResponse_httpStatus :: Lens.Lens' DescribeCommandsResponse Prelude.Int
describeCommandsResponse_httpStatus = Lens.lens (\DescribeCommandsResponse' {httpStatus} -> httpStatus) (\s@DescribeCommandsResponse' {} a -> s {httpStatus = a} :: DescribeCommandsResponse)

instance Prelude.NFData DescribeCommandsResponse

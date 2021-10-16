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
-- Module      : Network.AWS.ECS.ExecuteCommand
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a command remotely on a container within a task.
module Network.AWS.ECS.ExecuteCommand
  ( -- * Creating a Request
    ExecuteCommand (..),
    newExecuteCommand,

    -- * Request Lenses
    executeCommand_container,
    executeCommand_cluster,
    executeCommand_command,
    executeCommand_interactive,
    executeCommand_task,

    -- * Destructuring the Response
    ExecuteCommandResponse (..),
    newExecuteCommandResponse,

    -- * Response Lenses
    executeCommandResponse_clusterArn,
    executeCommandResponse_interactive,
    executeCommandResponse_containerArn,
    executeCommandResponse_session,
    executeCommandResponse_containerName,
    executeCommandResponse_taskArn,
    executeCommandResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExecuteCommand' smart constructor.
data ExecuteCommand = ExecuteCommand'
  { -- | The name of the container to execute the command on. A container name
    -- only needs to be specified for tasks containing multiple containers.
    container :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) or short name of the cluster the task is
    -- running in. If you do not specify a cluster, the default cluster is
    -- assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The command to run on the container.
    command :: Prelude.Text,
    -- | Use this flag to run your command in interactive mode.
    interactive :: Prelude.Bool,
    -- | The Amazon Resource Name (ARN) or ID of the task the container is part
    -- of.
    task :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'executeCommand_container' - The name of the container to execute the command on. A container name
-- only needs to be specified for tasks containing multiple containers.
--
-- 'cluster', 'executeCommand_cluster' - The Amazon Resource Name (ARN) or short name of the cluster the task is
-- running in. If you do not specify a cluster, the default cluster is
-- assumed.
--
-- 'command', 'executeCommand_command' - The command to run on the container.
--
-- 'interactive', 'executeCommand_interactive' - Use this flag to run your command in interactive mode.
--
-- 'task', 'executeCommand_task' - The Amazon Resource Name (ARN) or ID of the task the container is part
-- of.
newExecuteCommand ::
  -- | 'command'
  Prelude.Text ->
  -- | 'interactive'
  Prelude.Bool ->
  -- | 'task'
  Prelude.Text ->
  ExecuteCommand
newExecuteCommand pCommand_ pInteractive_ pTask_ =
  ExecuteCommand'
    { container = Prelude.Nothing,
      cluster = Prelude.Nothing,
      command = pCommand_,
      interactive = pInteractive_,
      task = pTask_
    }

-- | The name of the container to execute the command on. A container name
-- only needs to be specified for tasks containing multiple containers.
executeCommand_container :: Lens.Lens' ExecuteCommand (Prelude.Maybe Prelude.Text)
executeCommand_container = Lens.lens (\ExecuteCommand' {container} -> container) (\s@ExecuteCommand' {} a -> s {container = a} :: ExecuteCommand)

-- | The Amazon Resource Name (ARN) or short name of the cluster the task is
-- running in. If you do not specify a cluster, the default cluster is
-- assumed.
executeCommand_cluster :: Lens.Lens' ExecuteCommand (Prelude.Maybe Prelude.Text)
executeCommand_cluster = Lens.lens (\ExecuteCommand' {cluster} -> cluster) (\s@ExecuteCommand' {} a -> s {cluster = a} :: ExecuteCommand)

-- | The command to run on the container.
executeCommand_command :: Lens.Lens' ExecuteCommand Prelude.Text
executeCommand_command = Lens.lens (\ExecuteCommand' {command} -> command) (\s@ExecuteCommand' {} a -> s {command = a} :: ExecuteCommand)

-- | Use this flag to run your command in interactive mode.
executeCommand_interactive :: Lens.Lens' ExecuteCommand Prelude.Bool
executeCommand_interactive = Lens.lens (\ExecuteCommand' {interactive} -> interactive) (\s@ExecuteCommand' {} a -> s {interactive = a} :: ExecuteCommand)

-- | The Amazon Resource Name (ARN) or ID of the task the container is part
-- of.
executeCommand_task :: Lens.Lens' ExecuteCommand Prelude.Text
executeCommand_task = Lens.lens (\ExecuteCommand' {task} -> task) (\s@ExecuteCommand' {} a -> s {task = a} :: ExecuteCommand)

instance Core.AWSRequest ExecuteCommand where
  type
    AWSResponse ExecuteCommand =
      ExecuteCommandResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteCommandResponse'
            Prelude.<$> (x Core..?> "clusterArn")
            Prelude.<*> (x Core..?> "interactive")
            Prelude.<*> (x Core..?> "containerArn")
            Prelude.<*> (x Core..?> "session")
            Prelude.<*> (x Core..?> "containerName")
            Prelude.<*> (x Core..?> "taskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteCommand

instance Prelude.NFData ExecuteCommand

instance Core.ToHeaders ExecuteCommand where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ExecuteCommand" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExecuteCommand where
  toJSON ExecuteCommand' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("container" Core..=) Prelude.<$> container,
            ("cluster" Core..=) Prelude.<$> cluster,
            Prelude.Just ("command" Core..= command),
            Prelude.Just ("interactive" Core..= interactive),
            Prelude.Just ("task" Core..= task)
          ]
      )

instance Core.ToPath ExecuteCommand where
  toPath = Prelude.const "/"

instance Core.ToQuery ExecuteCommand where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteCommandResponse' smart constructor.
data ExecuteCommandResponse = ExecuteCommandResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the execute command session is running in interactive
    -- mode. Amazon ECS only supports initiating interactive sessions, so you
    -- must specify @true@ for this value.
    interactive :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the container.
    containerArn :: Prelude.Maybe Prelude.Text,
    -- | The details of the SSM session that was created for this instance of
    -- execute-command.
    session :: Prelude.Maybe Session,
    -- | The name of the container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCommandResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'executeCommandResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'interactive', 'executeCommandResponse_interactive' - Whether or not the execute command session is running in interactive
-- mode. Amazon ECS only supports initiating interactive sessions, so you
-- must specify @true@ for this value.
--
-- 'containerArn', 'executeCommandResponse_containerArn' - The Amazon Resource Name (ARN) of the container.
--
-- 'session', 'executeCommandResponse_session' - The details of the SSM session that was created for this instance of
-- execute-command.
--
-- 'containerName', 'executeCommandResponse_containerName' - The name of the container.
--
-- 'taskArn', 'executeCommandResponse_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'httpStatus', 'executeCommandResponse_httpStatus' - The response's http status code.
newExecuteCommandResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteCommandResponse
newExecuteCommandResponse pHttpStatus_ =
  ExecuteCommandResponse'
    { clusterArn =
        Prelude.Nothing,
      interactive = Prelude.Nothing,
      containerArn = Prelude.Nothing,
      session = Prelude.Nothing,
      containerName = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
executeCommandResponse_clusterArn :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Prelude.Text)
executeCommandResponse_clusterArn = Lens.lens (\ExecuteCommandResponse' {clusterArn} -> clusterArn) (\s@ExecuteCommandResponse' {} a -> s {clusterArn = a} :: ExecuteCommandResponse)

-- | Whether or not the execute command session is running in interactive
-- mode. Amazon ECS only supports initiating interactive sessions, so you
-- must specify @true@ for this value.
executeCommandResponse_interactive :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Prelude.Bool)
executeCommandResponse_interactive = Lens.lens (\ExecuteCommandResponse' {interactive} -> interactive) (\s@ExecuteCommandResponse' {} a -> s {interactive = a} :: ExecuteCommandResponse)

-- | The Amazon Resource Name (ARN) of the container.
executeCommandResponse_containerArn :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Prelude.Text)
executeCommandResponse_containerArn = Lens.lens (\ExecuteCommandResponse' {containerArn} -> containerArn) (\s@ExecuteCommandResponse' {} a -> s {containerArn = a} :: ExecuteCommandResponse)

-- | The details of the SSM session that was created for this instance of
-- execute-command.
executeCommandResponse_session :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Session)
executeCommandResponse_session = Lens.lens (\ExecuteCommandResponse' {session} -> session) (\s@ExecuteCommandResponse' {} a -> s {session = a} :: ExecuteCommandResponse)

-- | The name of the container.
executeCommandResponse_containerName :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Prelude.Text)
executeCommandResponse_containerName = Lens.lens (\ExecuteCommandResponse' {containerName} -> containerName) (\s@ExecuteCommandResponse' {} a -> s {containerName = a} :: ExecuteCommandResponse)

-- | The Amazon Resource Name (ARN) of the task.
executeCommandResponse_taskArn :: Lens.Lens' ExecuteCommandResponse (Prelude.Maybe Prelude.Text)
executeCommandResponse_taskArn = Lens.lens (\ExecuteCommandResponse' {taskArn} -> taskArn) (\s@ExecuteCommandResponse' {} a -> s {taskArn = a} :: ExecuteCommandResponse)

-- | The response's http status code.
executeCommandResponse_httpStatus :: Lens.Lens' ExecuteCommandResponse Prelude.Int
executeCommandResponse_httpStatus = Lens.lens (\ExecuteCommandResponse' {httpStatus} -> httpStatus) (\s@ExecuteCommandResponse' {} a -> s {httpStatus = a} :: ExecuteCommandResponse)

instance Prelude.NFData ExecuteCommandResponse

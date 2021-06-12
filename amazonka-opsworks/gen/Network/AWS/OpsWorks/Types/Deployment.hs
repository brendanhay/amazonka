{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Deployment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.DeploymentCommand

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The IDs of the target instances.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The deployment status:
    --
    -- -   running
    --
    -- -   successful
    --
    -- -   failed
    status :: Core.Maybe Core.Text,
    -- | The deployment ID.
    deploymentId :: Core.Maybe Core.Text,
    -- | The app ID.
    appId :: Core.Maybe Core.Text,
    -- | The user\'s IAM ARN.
    iamUserArn :: Core.Maybe Core.Text,
    -- | The deployment duration.
    duration :: Core.Maybe Core.Int,
    -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | A user-defined comment.
    comment :: Core.Maybe Core.Text,
    -- | A string that contains user-defined custom JSON. It can be used to
    -- override the corresponding default stack configuration attribute values
    -- for stack or to pass data to recipes. The string should be in the
    -- following format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information on custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
    customJson :: Core.Maybe Core.Text,
    -- | Date when the deployment completed.
    completedAt :: Core.Maybe Core.Text,
    -- | Date when the deployment was created.
    createdAt :: Core.Maybe Core.Text,
    -- | Used to specify a stack or deployment command.
    command :: Core.Maybe DeploymentCommand
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'deployment_instanceIds' - The IDs of the target instances.
--
-- 'status', 'deployment_status' - The deployment status:
--
-- -   running
--
-- -   successful
--
-- -   failed
--
-- 'deploymentId', 'deployment_deploymentId' - The deployment ID.
--
-- 'appId', 'deployment_appId' - The app ID.
--
-- 'iamUserArn', 'deployment_iamUserArn' - The user\'s IAM ARN.
--
-- 'duration', 'deployment_duration' - The deployment duration.
--
-- 'stackId', 'deployment_stackId' - The stack ID.
--
-- 'comment', 'deployment_comment' - A user-defined comment.
--
-- 'customJson', 'deployment_customJson' - A string that contains user-defined custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- for stack or to pass data to recipes. The string should be in the
-- following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
--
-- 'completedAt', 'deployment_completedAt' - Date when the deployment completed.
--
-- 'createdAt', 'deployment_createdAt' - Date when the deployment was created.
--
-- 'command', 'deployment_command' - Used to specify a stack or deployment command.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { instanceIds = Core.Nothing,
      status = Core.Nothing,
      deploymentId = Core.Nothing,
      appId = Core.Nothing,
      iamUserArn = Core.Nothing,
      duration = Core.Nothing,
      stackId = Core.Nothing,
      comment = Core.Nothing,
      customJson = Core.Nothing,
      completedAt = Core.Nothing,
      createdAt = Core.Nothing,
      command = Core.Nothing
    }

-- | The IDs of the target instances.
deployment_instanceIds :: Lens.Lens' Deployment (Core.Maybe [Core.Text])
deployment_instanceIds = Lens.lens (\Deployment' {instanceIds} -> instanceIds) (\s@Deployment' {} a -> s {instanceIds = a} :: Deployment) Core.. Lens.mapping Lens._Coerce

-- | The deployment status:
--
-- -   running
--
-- -   successful
--
-- -   failed
deployment_status :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The deployment ID.
deployment_deploymentId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The app ID.
deployment_appId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_appId = Lens.lens (\Deployment' {appId} -> appId) (\s@Deployment' {} a -> s {appId = a} :: Deployment)

-- | The user\'s IAM ARN.
deployment_iamUserArn :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_iamUserArn = Lens.lens (\Deployment' {iamUserArn} -> iamUserArn) (\s@Deployment' {} a -> s {iamUserArn = a} :: Deployment)

-- | The deployment duration.
deployment_duration :: Lens.Lens' Deployment (Core.Maybe Core.Int)
deployment_duration = Lens.lens (\Deployment' {duration} -> duration) (\s@Deployment' {} a -> s {duration = a} :: Deployment)

-- | The stack ID.
deployment_stackId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_stackId = Lens.lens (\Deployment' {stackId} -> stackId) (\s@Deployment' {} a -> s {stackId = a} :: Deployment)

-- | A user-defined comment.
deployment_comment :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_comment = Lens.lens (\Deployment' {comment} -> comment) (\s@Deployment' {} a -> s {comment = a} :: Deployment)

-- | A string that contains user-defined custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- for stack or to pass data to recipes. The string should be in the
-- following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
deployment_customJson :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_customJson = Lens.lens (\Deployment' {customJson} -> customJson) (\s@Deployment' {} a -> s {customJson = a} :: Deployment)

-- | Date when the deployment completed.
deployment_completedAt :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_completedAt = Lens.lens (\Deployment' {completedAt} -> completedAt) (\s@Deployment' {} a -> s {completedAt = a} :: Deployment)

-- | Date when the deployment was created.
deployment_createdAt :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment)

-- | Used to specify a stack or deployment command.
deployment_command :: Lens.Lens' Deployment (Core.Maybe DeploymentCommand)
deployment_command = Lens.lens (\Deployment' {command} -> command) (\s@Deployment' {} a -> s {command = a} :: Deployment)

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Core.<$> (x Core..:? "InstanceIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "DeploymentId")
            Core.<*> (x Core..:? "AppId")
            Core.<*> (x Core..:? "IamUserArn")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "Comment")
            Core.<*> (x Core..:? "CustomJson")
            Core.<*> (x Core..:? "CompletedAt")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "Command")
      )

instance Core.Hashable Deployment

instance Core.NFData Deployment

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
-- Module      : Amazonka.OpsWorks.Types.Deployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Deployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.DeploymentCommand
import qualified Amazonka.Prelude as Prelude

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The user\'s IAM ARN.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | A string that contains user-defined custom JSON. It can be used to
    -- override the corresponding default stack configuration attribute values
    -- for stack or to pass data to recipes. The string should be in the
    -- following format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information on custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | The deployment ID.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Used to specify a stack or deployment command.
    command :: Prelude.Maybe DeploymentCommand,
    -- | The deployment status:
    --
    -- -   running
    --
    -- -   successful
    --
    -- -   failed
    status :: Prelude.Maybe Prelude.Text,
    -- | The deployment duration.
    duration :: Prelude.Maybe Prelude.Int,
    -- | A user-defined comment.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the target instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | Date when the deployment completed.
    completedAt :: Prelude.Maybe Prelude.Text,
    -- | Date when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'deployment_stackId' - The stack ID.
--
-- 'iamUserArn', 'deployment_iamUserArn' - The user\'s IAM ARN.
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
-- 'deploymentId', 'deployment_deploymentId' - The deployment ID.
--
-- 'command', 'deployment_command' - Used to specify a stack or deployment command.
--
-- 'status', 'deployment_status' - The deployment status:
--
-- -   running
--
-- -   successful
--
-- -   failed
--
-- 'duration', 'deployment_duration' - The deployment duration.
--
-- 'comment', 'deployment_comment' - A user-defined comment.
--
-- 'instanceIds', 'deployment_instanceIds' - The IDs of the target instances.
--
-- 'completedAt', 'deployment_completedAt' - Date when the deployment completed.
--
-- 'createdAt', 'deployment_createdAt' - Date when the deployment was created.
--
-- 'appId', 'deployment_appId' - The app ID.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { stackId = Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      customJson = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      command = Prelude.Nothing,
      status = Prelude.Nothing,
      duration = Prelude.Nothing,
      comment = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The stack ID.
deployment_stackId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_stackId = Lens.lens (\Deployment' {stackId} -> stackId) (\s@Deployment' {} a -> s {stackId = a} :: Deployment)

-- | The user\'s IAM ARN.
deployment_iamUserArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_iamUserArn = Lens.lens (\Deployment' {iamUserArn} -> iamUserArn) (\s@Deployment' {} a -> s {iamUserArn = a} :: Deployment)

-- | A string that contains user-defined custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- for stack or to pass data to recipes. The string should be in the
-- following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
deployment_customJson :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_customJson = Lens.lens (\Deployment' {customJson} -> customJson) (\s@Deployment' {} a -> s {customJson = a} :: Deployment)

-- | The deployment ID.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | Used to specify a stack or deployment command.
deployment_command :: Lens.Lens' Deployment (Prelude.Maybe DeploymentCommand)
deployment_command = Lens.lens (\Deployment' {command} -> command) (\s@Deployment' {} a -> s {command = a} :: Deployment)

-- | The deployment status:
--
-- -   running
--
-- -   successful
--
-- -   failed
deployment_status :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The deployment duration.
deployment_duration :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_duration = Lens.lens (\Deployment' {duration} -> duration) (\s@Deployment' {} a -> s {duration = a} :: Deployment)

-- | A user-defined comment.
deployment_comment :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_comment = Lens.lens (\Deployment' {comment} -> comment) (\s@Deployment' {} a -> s {comment = a} :: Deployment)

-- | The IDs of the target instances.
deployment_instanceIds :: Lens.Lens' Deployment (Prelude.Maybe [Prelude.Text])
deployment_instanceIds = Lens.lens (\Deployment' {instanceIds} -> instanceIds) (\s@Deployment' {} a -> s {instanceIds = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | Date when the deployment completed.
deployment_completedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_completedAt = Lens.lens (\Deployment' {completedAt} -> completedAt) (\s@Deployment' {} a -> s {completedAt = a} :: Deployment)

-- | Date when the deployment was created.
deployment_createdAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment)

-- | The app ID.
deployment_appId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_appId = Lens.lens (\Deployment' {appId} -> appId) (\s@Deployment' {} a -> s {appId = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "IamUserArn")
            Prelude.<*> (x Data..:? "CustomJson")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "Command")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "Comment")
            Prelude.<*> (x Data..:? "InstanceIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CompletedAt")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "AppId")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` customJson
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` appId

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf customJson
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf appId

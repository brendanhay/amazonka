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
-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs deployment or stack commands. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html Deploying Apps>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html Run Stack Commands>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Deploy or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_instanceIds,
    createDeployment_appId,
    createDeployment_comment,
    createDeployment_customJson,
    createDeployment_layerIds,
    createDeployment_stackId,
    createDeployment_command,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The instance IDs for the deployment targets.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The app ID. This parameter is required for app deployments, but not for
    -- other deployment commands.
    appId :: Core.Maybe Core.Text,
    -- | A user-defined comment.
    comment :: Core.Maybe Core.Text,
    -- | A string that contains user-defined, custom JSON. You can use this
    -- parameter to override some corresponding default stack configuration
    -- JSON values. The string should be in the following format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information about custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
    -- and
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON>.
    customJson :: Core.Maybe Core.Text,
    -- | The layer IDs for the deployment targets.
    layerIds :: Core.Maybe [Core.Text],
    -- | The stack ID.
    stackId :: Core.Text,
    -- | A @DeploymentCommand@ object that specifies the deployment command and
    -- any associated arguments.
    command :: DeploymentCommand
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'createDeployment_instanceIds' - The instance IDs for the deployment targets.
--
-- 'appId', 'createDeployment_appId' - The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
--
-- 'comment', 'createDeployment_comment' - A user-defined comment.
--
-- 'customJson', 'createDeployment_customJson' - A string that contains user-defined, custom JSON. You can use this
-- parameter to override some corresponding default stack configuration
-- JSON values. The string should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON>.
--
-- 'layerIds', 'createDeployment_layerIds' - The layer IDs for the deployment targets.
--
-- 'stackId', 'createDeployment_stackId' - The stack ID.
--
-- 'command', 'createDeployment_command' - A @DeploymentCommand@ object that specifies the deployment command and
-- any associated arguments.
newCreateDeployment ::
  -- | 'stackId'
  Core.Text ->
  -- | 'command'
  DeploymentCommand ->
  CreateDeployment
newCreateDeployment pStackId_ pCommand_ =
  CreateDeployment'
    { instanceIds = Core.Nothing,
      appId = Core.Nothing,
      comment = Core.Nothing,
      customJson = Core.Nothing,
      layerIds = Core.Nothing,
      stackId = pStackId_,
      command = pCommand_
    }

-- | The instance IDs for the deployment targets.
createDeployment_instanceIds :: Lens.Lens' CreateDeployment (Core.Maybe [Core.Text])
createDeployment_instanceIds = Lens.lens (\CreateDeployment' {instanceIds} -> instanceIds) (\s@CreateDeployment' {} a -> s {instanceIds = a} :: CreateDeployment) Core.. Lens.mapping Lens._Coerce

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
createDeployment_appId :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
createDeployment_appId = Lens.lens (\CreateDeployment' {appId} -> appId) (\s@CreateDeployment' {} a -> s {appId = a} :: CreateDeployment)

-- | A user-defined comment.
createDeployment_comment :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
createDeployment_comment = Lens.lens (\CreateDeployment' {comment} -> comment) (\s@CreateDeployment' {} a -> s {comment = a} :: CreateDeployment)

-- | A string that contains user-defined, custom JSON. You can use this
-- parameter to override some corresponding default stack configuration
-- JSON values. The string should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON>.
createDeployment_customJson :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
createDeployment_customJson = Lens.lens (\CreateDeployment' {customJson} -> customJson) (\s@CreateDeployment' {} a -> s {customJson = a} :: CreateDeployment)

-- | The layer IDs for the deployment targets.
createDeployment_layerIds :: Lens.Lens' CreateDeployment (Core.Maybe [Core.Text])
createDeployment_layerIds = Lens.lens (\CreateDeployment' {layerIds} -> layerIds) (\s@CreateDeployment' {} a -> s {layerIds = a} :: CreateDeployment) Core.. Lens.mapping Lens._Coerce

-- | The stack ID.
createDeployment_stackId :: Lens.Lens' CreateDeployment Core.Text
createDeployment_stackId = Lens.lens (\CreateDeployment' {stackId} -> stackId) (\s@CreateDeployment' {} a -> s {stackId = a} :: CreateDeployment)

-- | A @DeploymentCommand@ object that specifies the deployment command and
-- any associated arguments.
createDeployment_command :: Lens.Lens' CreateDeployment DeploymentCommand
createDeployment_command = Lens.lens (\CreateDeployment' {command} -> command) (\s@CreateDeployment' {} a -> s {command = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Core.<$> (x Core..?> "DeploymentId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDeployment

instance Core.NFData CreateDeployment

instance Core.ToHeaders CreateDeployment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.CreateDeployment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceIds" Core..=) Core.<$> instanceIds,
            ("AppId" Core..=) Core.<$> appId,
            ("Comment" Core..=) Core.<$> comment,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("LayerIds" Core..=) Core.<$> layerIds,
            Core.Just ("StackId" Core..= stackId),
            Core.Just ("Command" Core..= command)
          ]
      )

instance Core.ToPath CreateDeployment where
  toPath = Core.const "/"

instance Core.ToQuery CreateDeployment where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @CreateDeployment@ request.
--
-- /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The deployment ID, which can be used with other requests to identify the
    -- deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The deployment ID, which can be used with other requests to identify the
-- deployment.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDeploymentResponse
newCreateDeploymentResponse pHttpStatus_ =
  CreateDeploymentResponse'
    { deploymentId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Core.Maybe Core.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Core.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Core.NFData CreateDeploymentResponse

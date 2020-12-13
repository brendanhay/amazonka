{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs deployment or stack commands. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html Deploying Apps> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html Run Stack Commands> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateDeployment
  ( -- * Creating a request
    CreateDeployment (..),
    mkCreateDeployment,

    -- ** Request lenses
    cdCommand,
    cdCustomJSON,
    cdAppId,
    cdInstanceIds,
    cdLayerIds,
    cdStackId,
    cdComment,

    -- * Destructuring the response
    CreateDeploymentResponse (..),
    mkCreateDeploymentResponse,

    -- ** Response lenses
    cdrsDeploymentId,
    cdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
    command :: DeploymentCommand,
    -- | A string that contains user-defined, custom JSON. You can use this parameter to override some corresponding default stack configuration JSON values. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON> .
    customJSON :: Lude.Maybe Lude.Text,
    -- | The app ID. This parameter is required for app deployments, but not for other deployment commands.
    appId :: Lude.Maybe Lude.Text,
    -- | The instance IDs for the deployment targets.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | The layer IDs for the deployment targets.
    layerIds :: Lude.Maybe [Lude.Text],
    -- | The stack ID.
    stackId :: Lude.Text,
    -- | A user-defined comment.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- * 'command' - A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
-- * 'customJSON' - A string that contains user-defined, custom JSON. You can use this parameter to override some corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON> .
-- * 'appId' - The app ID. This parameter is required for app deployments, but not for other deployment commands.
-- * 'instanceIds' - The instance IDs for the deployment targets.
-- * 'layerIds' - The layer IDs for the deployment targets.
-- * 'stackId' - The stack ID.
-- * 'comment' - A user-defined comment.
mkCreateDeployment ::
  -- | 'command'
  DeploymentCommand ->
  -- | 'stackId'
  Lude.Text ->
  CreateDeployment
mkCreateDeployment pCommand_ pStackId_ =
  CreateDeployment'
    { command = pCommand_,
      customJSON = Lude.Nothing,
      appId = Lude.Nothing,
      instanceIds = Lude.Nothing,
      layerIds = Lude.Nothing,
      stackId = pStackId_,
      comment = Lude.Nothing
    }

-- | A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCommand :: Lens.Lens' CreateDeployment DeploymentCommand
cdCommand = Lens.lens (command :: CreateDeployment -> DeploymentCommand) (\s a -> s {command = a} :: CreateDeployment)
{-# DEPRECATED cdCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | A string that contains user-defined, custom JSON. You can use this parameter to override some corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCustomJSON :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdCustomJSON = Lens.lens (customJSON :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: CreateDeployment)
{-# DEPRECATED cdCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | The app ID. This parameter is required for app deployments, but not for other deployment commands.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAppId :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdAppId = Lens.lens (appId :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: CreateDeployment)
{-# DEPRECATED cdAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The instance IDs for the deployment targets.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInstanceIds :: Lens.Lens' CreateDeployment (Lude.Maybe [Lude.Text])
cdInstanceIds = Lens.lens (instanceIds :: CreateDeployment -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: CreateDeployment)
{-# DEPRECATED cdInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The layer IDs for the deployment targets.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLayerIds :: Lens.Lens' CreateDeployment (Lude.Maybe [Lude.Text])
cdLayerIds = Lens.lens (layerIds :: CreateDeployment -> Lude.Maybe [Lude.Text]) (\s a -> s {layerIds = a} :: CreateDeployment)
{-# DEPRECATED cdLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStackId :: Lens.Lens' CreateDeployment Lude.Text
cdStackId = Lens.lens (stackId :: CreateDeployment -> Lude.Text) (\s a -> s {stackId = a} :: CreateDeployment)
{-# DEPRECATED cdStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A user-defined comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComment :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdComment = Lens.lens (comment :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CreateDeployment)
{-# DEPRECATED cdComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.AWSRequest CreateDeployment where
  type Rs CreateDeployment = CreateDeploymentResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Lude.<$> (x Lude..?> "DeploymentId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateDeployment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Command" Lude..= command),
            ("CustomJson" Lude..=) Lude.<$> customJSON,
            ("AppId" Lude..=) Lude.<$> appId,
            ("InstanceIds" Lude..=) Lude.<$> instanceIds,
            ("LayerIds" Lude..=) Lude.<$> layerIds,
            Lude.Just ("StackId" Lude..= stackId),
            ("Comment" Lude..=) Lude.<$> comment
          ]
      )

instance Lude.ToPath CreateDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDeployment where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateDeployment@ request.
--
-- /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The deployment ID, which can be used with other requests to identify the deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The deployment ID, which can be used with other requests to identify the deployment.
-- * 'responseStatus' - The response status code.
mkCreateDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeploymentResponse
mkCreateDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
    { deploymentId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The deployment ID, which can be used with other requests to identify the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Lude.Maybe Lude.Text)
cdrsDeploymentId = Lens.lens (deploymentId :: CreateDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDeploymentResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Deployment
  ( Deployment (..),

    -- * Smart constructor
    mkDeployment,

    -- * Lenses
    dDeploymentId,
    dStatus,
    dCommand,
    dCreatedAt,
    dCustomJSON,
    dIAMUserARN,
    dAppId,
    dInstanceIds,
    dCompletedAt,
    dStackId,
    dComment,
    dDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.DeploymentCommand
import qualified Network.AWS.Prelude as Lude

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { deploymentId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    command :: Lude.Maybe DeploymentCommand,
    createdAt :: Lude.Maybe Lude.Text,
    customJSON :: Lude.Maybe Lude.Text,
    iamUserARN :: Lude.Maybe Lude.Text,
    appId :: Lude.Maybe Lude.Text,
    instanceIds :: Lude.Maybe [Lude.Text],
    completedAt :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- * 'appId' - The app ID.
-- * 'command' - Used to specify a stack or deployment command.
-- * 'comment' - A user-defined comment.
-- * 'completedAt' - Date when the deployment completed.
-- * 'createdAt' - Date when the deployment was created.
-- * 'customJSON' - A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
-- * 'deploymentId' - The deployment ID.
-- * 'duration' - The deployment duration.
-- * 'iamUserARN' - The user's IAM ARN.
-- * 'instanceIds' - The IDs of the target instances.
-- * 'stackId' - The stack ID.
-- * 'status' - The deployment status:
--
--
--     * running
--
--
--     * successful
--
--
--     * failed
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { deploymentId = Lude.Nothing,
      status = Lude.Nothing,
      command = Lude.Nothing,
      createdAt = Lude.Nothing,
      customJSON = Lude.Nothing,
      iamUserARN = Lude.Nothing,
      appId = Lude.Nothing,
      instanceIds = Lude.Nothing,
      completedAt = Lude.Nothing,
      stackId = Lude.Nothing,
      comment = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The deployment ID.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dDeploymentId = Lens.lens (deploymentId :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: Deployment)
{-# DEPRECATED dDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The deployment status:
--
--
--     * running
--
--
--     * successful
--
--
--     * failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dStatus = Lens.lens (status :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Deployment)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Used to specify a stack or deployment command.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCommand :: Lens.Lens' Deployment (Lude.Maybe DeploymentCommand)
dCommand = Lens.lens (command :: Deployment -> Lude.Maybe DeploymentCommand) (\s a -> s {command = a} :: Deployment)
{-# DEPRECATED dCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | Date when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dCreatedAt = Lens.lens (createdAt :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Deployment)
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomJSON :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dCustomJSON = Lens.lens (customJSON :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: Deployment)
{-# DEPRECATED dCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIAMUserARN :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dIAMUserARN = Lens.lens (iamUserARN :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: Deployment)
{-# DEPRECATED dIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dAppId = Lens.lens (appId :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: Deployment)
{-# DEPRECATED dAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The IDs of the target instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstanceIds :: Lens.Lens' Deployment (Lude.Maybe [Lude.Text])
dInstanceIds = Lens.lens (instanceIds :: Deployment -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: Deployment)
{-# DEPRECATED dInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Date when the deployment completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCompletedAt :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dCompletedAt = Lens.lens (completedAt :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {completedAt = a} :: Deployment)
{-# DEPRECATED dCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dStackId = Lens.lens (stackId :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Deployment)
{-# DEPRECATED dStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A user-defined comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dComment :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dComment = Lens.lens (comment :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: Deployment)
{-# DEPRECATED dComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The deployment duration.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDuration :: Lens.Lens' Deployment (Lude.Maybe Lude.Int)
dDuration = Lens.lens (duration :: Deployment -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Deployment)
{-# DEPRECATED dDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromJSON Deployment where
  parseJSON =
    Lude.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Lude.<$> (x Lude..:? "DeploymentId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Command")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "CustomJson")
            Lude.<*> (x Lude..:? "IamUserArn")
            Lude.<*> (x Lude..:? "AppId")
            Lude.<*> (x Lude..:? "InstanceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CompletedAt")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "Comment")
            Lude.<*> (x Lude..:? "Duration")
      )

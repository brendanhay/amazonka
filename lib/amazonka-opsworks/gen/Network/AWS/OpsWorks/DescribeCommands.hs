{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the results of specified commands.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeCommands
  ( -- * Creating a request
    DescribeCommands (..),
    mkDescribeCommands,

    -- ** Request lenses
    dcDeploymentId,
    dcInstanceId,
    dcCommandIds,

    -- * Destructuring the response
    DescribeCommandsResponse (..),
    mkDescribeCommandsResponse,

    -- ** Response lenses
    dcrsCommands,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCommands' smart constructor.
data DescribeCommands = DescribeCommands'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    commandIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCommands' with the minimum fields required to make a request.
--
-- * 'commandIds' - An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
-- * 'deploymentId' - The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
-- * 'instanceId' - The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
mkDescribeCommands ::
  DescribeCommands
mkDescribeCommands =
  DescribeCommands'
    { deploymentId = Lude.Nothing,
      instanceId = Lude.Nothing,
      commandIds = Lude.Nothing
    }

-- | The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDeploymentId :: Lens.Lens' DescribeCommands (Lude.Maybe Lude.Text)
dcDeploymentId = Lens.lens (deploymentId :: DescribeCommands -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: DescribeCommands)
{-# DEPRECATED dcDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcInstanceId :: Lens.Lens' DescribeCommands (Lude.Maybe Lude.Text)
dcInstanceId = Lens.lens (instanceId :: DescribeCommands -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeCommands)
{-# DEPRECATED dcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
--
-- /Note:/ Consider using 'commandIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCommandIds :: Lens.Lens' DescribeCommands (Lude.Maybe [Lude.Text])
dcCommandIds = Lens.lens (commandIds :: DescribeCommands -> Lude.Maybe [Lude.Text]) (\s a -> s {commandIds = a} :: DescribeCommands)
{-# DEPRECATED dcCommandIds "Use generic-lens or generic-optics with 'commandIds' instead." #-}

instance Lude.AWSRequest DescribeCommands where
  type Rs DescribeCommands = DescribeCommandsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCommandsResponse'
            Lude.<$> (x Lude..?> "Commands" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCommands where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeCommands" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCommands where
  toJSON DescribeCommands' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeploymentId" Lude..=) Lude.<$> deploymentId,
            ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("CommandIds" Lude..=) Lude.<$> commandIds
          ]
      )

instance Lude.ToPath DescribeCommands where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCommands where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeCommands@ request.
--
-- /See:/ 'mkDescribeCommandsResponse' smart constructor.
data DescribeCommandsResponse = DescribeCommandsResponse'
  { commands ::
      Lude.Maybe [Command],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCommandsResponse' with the minimum fields required to make a request.
--
-- * 'commands' - An array of @Command@ objects that describe each of the specified commands.
-- * 'responseStatus' - The response status code.
mkDescribeCommandsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCommandsResponse
mkDescribeCommandsResponse pResponseStatus_ =
  DescribeCommandsResponse'
    { commands = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Command@ objects that describe each of the specified commands.
--
-- /Note:/ Consider using 'commands' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCommands :: Lens.Lens' DescribeCommandsResponse (Lude.Maybe [Command])
dcrsCommands = Lens.lens (commands :: DescribeCommandsResponse -> Lude.Maybe [Command]) (\s a -> s {commands = a} :: DescribeCommandsResponse)
{-# DEPRECATED dcrsCommands "Use generic-lens or generic-optics with 'commands' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCommandsResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCommandsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCommandsResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterEcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a specified Amazon ECS cluster with a stack. You can register only one cluster with a stack. A cluster can be registered with only one stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterEcsCluster
  ( -- * Creating a request
    RegisterEcsCluster (..),
    mkRegisterEcsCluster,

    -- ** Request lenses
    recEcsClusterARN,
    recStackId,

    -- * Destructuring the response
    RegisterEcsClusterResponse (..),
    mkRegisterEcsClusterResponse,

    -- ** Response lenses
    recrsEcsClusterARN,
    recrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterEcsCluster' smart constructor.
data RegisterEcsCluster = RegisterEcsCluster'
  { -- | The cluster's ARN.
    ecsClusterARN :: Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterEcsCluster' with the minimum fields required to make a request.
--
-- * 'ecsClusterARN' - The cluster's ARN.
-- * 'stackId' - The stack ID.
mkRegisterEcsCluster ::
  -- | 'ecsClusterARN'
  Lude.Text ->
  -- | 'stackId'
  Lude.Text ->
  RegisterEcsCluster
mkRegisterEcsCluster pEcsClusterARN_ pStackId_ =
  RegisterEcsCluster'
    { ecsClusterARN = pEcsClusterARN_,
      stackId = pStackId_
    }

-- | The cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recEcsClusterARN :: Lens.Lens' RegisterEcsCluster Lude.Text
recEcsClusterARN = Lens.lens (ecsClusterARN :: RegisterEcsCluster -> Lude.Text) (\s a -> s {ecsClusterARN = a} :: RegisterEcsCluster)
{-# DEPRECATED recEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recStackId :: Lens.Lens' RegisterEcsCluster Lude.Text
recStackId = Lens.lens (stackId :: RegisterEcsCluster -> Lude.Text) (\s a -> s {stackId = a} :: RegisterEcsCluster)
{-# DEPRECATED recStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest RegisterEcsCluster where
  type Rs RegisterEcsCluster = RegisterEcsClusterResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterEcsClusterResponse'
            Lude.<$> (x Lude..?> "EcsClusterArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterEcsCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RegisterEcsCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterEcsCluster where
  toJSON RegisterEcsCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EcsClusterArn" Lude..= ecsClusterARN),
            Lude.Just ("StackId" Lude..= stackId)
          ]
      )

instance Lude.ToPath RegisterEcsCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterEcsCluster where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @RegisterEcsCluster@ request.
--
-- /See:/ 'mkRegisterEcsClusterResponse' smart constructor.
data RegisterEcsClusterResponse = RegisterEcsClusterResponse'
  { -- | The cluster's ARN.
    ecsClusterARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterEcsClusterResponse' with the minimum fields required to make a request.
--
-- * 'ecsClusterARN' - The cluster's ARN.
-- * 'responseStatus' - The response status code.
mkRegisterEcsClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterEcsClusterResponse
mkRegisterEcsClusterResponse pResponseStatus_ =
  RegisterEcsClusterResponse'
    { ecsClusterARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recrsEcsClusterARN :: Lens.Lens' RegisterEcsClusterResponse (Lude.Maybe Lude.Text)
recrsEcsClusterARN = Lens.lens (ecsClusterARN :: RegisterEcsClusterResponse -> Lude.Maybe Lude.Text) (\s a -> s {ecsClusterARN = a} :: RegisterEcsClusterResponse)
{-# DEPRECATED recrsEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recrsResponseStatus :: Lens.Lens' RegisterEcsClusterResponse Lude.Int
recrsResponseStatus = Lens.lens (responseStatus :: RegisterEcsClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterEcsClusterResponse)
{-# DEPRECATED recrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

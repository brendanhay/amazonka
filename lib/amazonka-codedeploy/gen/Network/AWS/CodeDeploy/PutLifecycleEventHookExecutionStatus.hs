{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the result of a Lambda validation function. The function validates lifecycle hooks during a deployment that uses the AWS Lambda or Amazon ECS compute platform. For AWS Lambda deployments, the available lifecycle hooks are @BeforeAllowTraffic@ and @AfterAllowTraffic@ . For Amazon ECS deployments, the available lifecycle hooks are @BeforeInstall@ , @AfterInstall@ , @AfterAllowTestTraffic@ , @BeforeAllowTraffic@ , and @AfterAllowTraffic@ . Lambda validation functions return @Succeeded@ or @Failed@ . For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-lambda AppSpec 'hooks' Section for an AWS Lambda Deployment > and <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-ecs AppSpec 'hooks' Section for an Amazon ECS Deployment> .
module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
  ( -- * Creating a request
    PutLifecycleEventHookExecutionStatus (..),
    mkPutLifecycleEventHookExecutionStatus,

    -- ** Request lenses
    plehesStatus,
    plehesDeploymentId,
    plehesLifecycleEventHookExecutionId,

    -- * Destructuring the response
    PutLifecycleEventHookExecutionStatusResponse (..),
    mkPutLifecycleEventHookExecutionStatusResponse,

    -- ** Response lenses
    plehesrsLifecycleEventHookExecutionId,
    plehesrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLifecycleEventHookExecutionStatus' smart constructor.
data PutLifecycleEventHookExecutionStatus = PutLifecycleEventHookExecutionStatus'
  { -- | The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
    status :: Lude.Maybe LifecycleEventStatus,
    -- | The unique ID of a deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
    lifecycleEventHookExecutionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecycleEventHookExecutionStatus' with the minimum fields required to make a request.
--
-- * 'status' - The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
-- * 'deploymentId' - The unique ID of a deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
-- * 'lifecycleEventHookExecutionId' - The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
mkPutLifecycleEventHookExecutionStatus ::
  PutLifecycleEventHookExecutionStatus
mkPutLifecycleEventHookExecutionStatus =
  PutLifecycleEventHookExecutionStatus'
    { status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      lifecycleEventHookExecutionId = Lude.Nothing
    }

-- | The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Lude.Maybe LifecycleEventStatus)
plehesStatus = Lens.lens (status :: PutLifecycleEventHookExecutionStatus -> Lude.Maybe LifecycleEventStatus) (\s a -> s {status = a} :: PutLifecycleEventHookExecutionStatus)
{-# DEPRECATED plehesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesDeploymentId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Lude.Maybe Lude.Text)
plehesDeploymentId = Lens.lens (deploymentId :: PutLifecycleEventHookExecutionStatus -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: PutLifecycleEventHookExecutionStatus)
{-# DEPRECATED plehesDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
--
-- /Note:/ Consider using 'lifecycleEventHookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesLifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Lude.Maybe Lude.Text)
plehesLifecycleEventHookExecutionId = Lens.lens (lifecycleEventHookExecutionId :: PutLifecycleEventHookExecutionStatus -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatus)
{-# DEPRECATED plehesLifecycleEventHookExecutionId "Use generic-lens or generic-optics with 'lifecycleEventHookExecutionId' instead." #-}

instance Lude.AWSRequest PutLifecycleEventHookExecutionStatus where
  type
    Rs PutLifecycleEventHookExecutionStatus =
      PutLifecycleEventHookExecutionStatusResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutLifecycleEventHookExecutionStatusResponse'
            Lude.<$> (x Lude..?> "lifecycleEventHookExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLifecycleEventHookExecutionStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.PutLifecycleEventHookExecutionStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutLifecycleEventHookExecutionStatus where
  toJSON PutLifecycleEventHookExecutionStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("deploymentId" Lude..=) Lude.<$> deploymentId,
            ("lifecycleEventHookExecutionId" Lude..=)
              Lude.<$> lifecycleEventHookExecutionId
          ]
      )

instance Lude.ToPath PutLifecycleEventHookExecutionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLifecycleEventHookExecutionStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLifecycleEventHookExecutionStatusResponse' smart constructor.
data PutLifecycleEventHookExecutionStatusResponse = PutLifecycleEventHookExecutionStatusResponse'
  { -- | The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
    lifecycleEventHookExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecycleEventHookExecutionStatusResponse' with the minimum fields required to make a request.
--
-- * 'lifecycleEventHookExecutionId' - The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
-- * 'responseStatus' - The response status code.
mkPutLifecycleEventHookExecutionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLifecycleEventHookExecutionStatusResponse
mkPutLifecycleEventHookExecutionStatusResponse pResponseStatus_ =
  PutLifecycleEventHookExecutionStatusResponse'
    { lifecycleEventHookExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
--
-- /Note:/ Consider using 'lifecycleEventHookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesrsLifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse (Lude.Maybe Lude.Text)
plehesrsLifecycleEventHookExecutionId = Lens.lens (lifecycleEventHookExecutionId :: PutLifecycleEventHookExecutionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatusResponse)
{-# DEPRECATED plehesrsLifecycleEventHookExecutionId "Use generic-lens or generic-optics with 'lifecycleEventHookExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesrsResponseStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse Lude.Int
plehesrsResponseStatus = Lens.lens (responseStatus :: PutLifecycleEventHookExecutionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLifecycleEventHookExecutionStatusResponse)
{-# DEPRECATED plehesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

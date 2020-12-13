{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaTarget
  ( LambdaTarget (..),

    -- * Smart constructor
    mkLambdaTarget,

    -- * Lenses
    ltTargetARN,
    ltTargetId,
    ltStatus,
    ltDeploymentId,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltLambdaFunctionInfo,
  )
where

import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the target AWS Lambda function during an AWS Lambda deployment.
--
-- /See:/ 'mkLambdaTarget' smart constructor.
data LambdaTarget = LambdaTarget'
  { -- | The Amazon Resource Name (ARN) of the target.
    targetARN :: Lude.Maybe Lude.Text,
    -- | The unique ID of a deployment target that has a type of @lambdaTarget@ .
    targetId :: Lude.Maybe Lude.Text,
    -- | The status an AWS Lambda deployment's target Lambda function.
    status :: Lude.Maybe TargetStatus,
    -- | The unique ID of a deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | The date and time when the target Lambda function was updated by a deployment.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The lifecycle events of the deployment to this target Lambda function.
    lifecycleEvents :: Lude.Maybe [LifecycleEvent],
    -- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
    lambdaFunctionInfo :: Lude.Maybe LambdaFunctionInfo
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaTarget' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target.
-- * 'targetId' - The unique ID of a deployment target that has a type of @lambdaTarget@ .
-- * 'status' - The status an AWS Lambda deployment's target Lambda function.
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'lastUpdatedAt' - The date and time when the target Lambda function was updated by a deployment.
-- * 'lifecycleEvents' - The lifecycle events of the deployment to this target Lambda function.
-- * 'lambdaFunctionInfo' - A @LambdaFunctionInfo@ object that describes a target Lambda function.
mkLambdaTarget ::
  LambdaTarget
mkLambdaTarget =
  LambdaTarget'
    { targetARN = Lude.Nothing,
      targetId = Lude.Nothing,
      status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      lifecycleEvents = Lude.Nothing,
      lambdaFunctionInfo = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetARN :: Lens.Lens' LambdaTarget (Lude.Maybe Lude.Text)
ltTargetARN = Lens.lens (targetARN :: LambdaTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: LambdaTarget)
{-# DEPRECATED ltTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The unique ID of a deployment target that has a type of @lambdaTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetId :: Lens.Lens' LambdaTarget (Lude.Maybe Lude.Text)
ltTargetId = Lens.lens (targetId :: LambdaTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: LambdaTarget)
{-# DEPRECATED ltTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The status an AWS Lambda deployment's target Lambda function.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltStatus :: Lens.Lens' LambdaTarget (Lude.Maybe TargetStatus)
ltStatus = Lens.lens (status :: LambdaTarget -> Lude.Maybe TargetStatus) (\s a -> s {status = a} :: LambdaTarget)
{-# DEPRECATED ltStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDeploymentId :: Lens.Lens' LambdaTarget (Lude.Maybe Lude.Text)
ltDeploymentId = Lens.lens (deploymentId :: LambdaTarget -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: LambdaTarget)
{-# DEPRECATED ltDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The date and time when the target Lambda function was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLastUpdatedAt :: Lens.Lens' LambdaTarget (Lude.Maybe Lude.Timestamp)
ltLastUpdatedAt = Lens.lens (lastUpdatedAt :: LambdaTarget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: LambdaTarget)
{-# DEPRECATED ltLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the deployment to this target Lambda function.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLifecycleEvents :: Lens.Lens' LambdaTarget (Lude.Maybe [LifecycleEvent])
ltLifecycleEvents = Lens.lens (lifecycleEvents :: LambdaTarget -> Lude.Maybe [LifecycleEvent]) (\s a -> s {lifecycleEvents = a} :: LambdaTarget)
{-# DEPRECATED ltLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

-- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
--
-- /Note:/ Consider using 'lambdaFunctionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLambdaFunctionInfo :: Lens.Lens' LambdaTarget (Lude.Maybe LambdaFunctionInfo)
ltLambdaFunctionInfo = Lens.lens (lambdaFunctionInfo :: LambdaTarget -> Lude.Maybe LambdaFunctionInfo) (\s a -> s {lambdaFunctionInfo = a} :: LambdaTarget)
{-# DEPRECATED ltLambdaFunctionInfo "Use generic-lens or generic-optics with 'lambdaFunctionInfo' instead." #-}

instance Lude.FromJSON LambdaTarget where
  parseJSON =
    Lude.withObject
      "LambdaTarget"
      ( \x ->
          LambdaTarget'
            Lude.<$> (x Lude..:? "targetArn")
            Lude.<*> (x Lude..:? "targetId")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "lifecycleEvents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lambdaFunctionInfo")
      )

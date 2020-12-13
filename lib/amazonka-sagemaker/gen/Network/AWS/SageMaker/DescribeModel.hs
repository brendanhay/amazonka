{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a model that you created using the @CreateModel@ API.
module Network.AWS.SageMaker.DescribeModel
  ( -- * Creating a request
    DescribeModel (..),
    mkDescribeModel,

    -- ** Request lenses
    dmModelName,

    -- * Destructuring the response
    DescribeModelResponse (..),
    mkDescribeModelResponse,

    -- ** Response lenses
    dmrsCreationTime,
    dmrsModelARN,
    dmrsModelName,
    dmrsPrimaryContainer,
    dmrsExecutionRoleARN,
    dmrsEnableNetworkIsolation,
    dmrsContainers,
    dmrsVPCConfig,
    dmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeModel' smart constructor.
newtype DescribeModel = DescribeModel'
  { -- | The name of the model.
    modelName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeModel' with the minimum fields required to make a request.
--
-- * 'modelName' - The name of the model.
mkDescribeModel ::
  -- | 'modelName'
  Lude.Text ->
  DescribeModel
mkDescribeModel pModelName_ =
  DescribeModel' {modelName = pModelName_}

-- | The name of the model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmModelName :: Lens.Lens' DescribeModel Lude.Text
dmModelName = Lens.lens (modelName :: DescribeModel -> Lude.Text) (\s a -> s {modelName = a} :: DescribeModel)
{-# DEPRECATED dmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Lude.AWSRequest DescribeModel where
  type Rs DescribeModel = DescribeModelResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeModelResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "ModelArn")
            Lude.<*> (x Lude..:> "ModelName")
            Lude.<*> (x Lude..?> "PrimaryContainer")
            Lude.<*> (x Lude..:> "ExecutionRoleArn")
            Lude.<*> (x Lude..?> "EnableNetworkIsolation")
            Lude.<*> (x Lude..?> "Containers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "VpcConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeModel where
  toJSON DescribeModel' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ModelName" Lude..= modelName)])

instance Lude.ToPath DescribeModel where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { -- | A timestamp that shows when the model was created.
    creationTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the model.
    modelARN :: Lude.Text,
    -- | Name of the Amazon SageMaker model.
    modelName :: Lude.Text,
    -- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
    primaryContainer :: Lude.Maybe ContainerDefinition,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
    executionRoleARN :: Lude.Text,
    -- | If @True@ , no inbound or outbound network calls can be made to or from the model container.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    -- | The containers in the inference pipeline.
    containers :: Lude.Maybe [ContainerDefinition],
    -- | A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeModelResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the model was created.
-- * 'modelARN' - The Amazon Resource Name (ARN) of the model.
-- * 'modelName' - Name of the Amazon SageMaker model.
-- * 'primaryContainer' - The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
-- * 'enableNetworkIsolation' - If @True@ , no inbound or outbound network calls can be made to or from the model container.
-- * 'containers' - The containers in the inference pipeline.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- * 'responseStatus' - The response status code.
mkDescribeModelResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'modelARN'
  Lude.Text ->
  -- | 'modelName'
  Lude.Text ->
  -- | 'executionRoleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeModelResponse
mkDescribeModelResponse
  pCreationTime_
  pModelARN_
  pModelName_
  pExecutionRoleARN_
  pResponseStatus_ =
    DescribeModelResponse'
      { creationTime = pCreationTime_,
        modelARN = pModelARN_,
        modelName = pModelName_,
        primaryContainer = Lude.Nothing,
        executionRoleARN = pExecutionRoleARN_,
        enableNetworkIsolation = Lude.Nothing,
        containers = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A timestamp that shows when the model was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsCreationTime :: Lens.Lens' DescribeModelResponse Lude.Timestamp
dmrsCreationTime = Lens.lens (creationTime :: DescribeModelResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the model.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsModelARN :: Lens.Lens' DescribeModelResponse Lude.Text
dmrsModelARN = Lens.lens (modelARN :: DescribeModelResponse -> Lude.Text) (\s a -> s {modelARN = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | Name of the Amazon SageMaker model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsModelName :: Lens.Lens' DescribeModelResponse Lude.Text
dmrsModelName = Lens.lens (modelName :: DescribeModelResponse -> Lude.Text) (\s a -> s {modelName = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
--
-- /Note:/ Consider using 'primaryContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsPrimaryContainer :: Lens.Lens' DescribeModelResponse (Lude.Maybe ContainerDefinition)
dmrsPrimaryContainer = Lens.lens (primaryContainer :: DescribeModelResponse -> Lude.Maybe ContainerDefinition) (\s a -> s {primaryContainer = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsPrimaryContainer "Use generic-lens or generic-optics with 'primaryContainer' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsExecutionRoleARN :: Lens.Lens' DescribeModelResponse Lude.Text
dmrsExecutionRoleARN = Lens.lens (executionRoleARN :: DescribeModelResponse -> Lude.Text) (\s a -> s {executionRoleARN = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | If @True@ , no inbound or outbound network calls can be made to or from the model container.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsEnableNetworkIsolation :: Lens.Lens' DescribeModelResponse (Lude.Maybe Lude.Bool)
dmrsEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: DescribeModelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | The containers in the inference pipeline.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsContainers :: Lens.Lens' DescribeModelResponse (Lude.Maybe [ContainerDefinition])
dmrsContainers = Lens.lens (containers :: DescribeModelResponse -> Lude.Maybe [ContainerDefinition]) (\s a -> s {containers = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsVPCConfig :: Lens.Lens' DescribeModelResponse (Lude.Maybe VPCConfig)
dmrsVPCConfig = Lens.lens (vpcConfig :: DescribeModelResponse -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsResponseStatus :: Lens.Lens' DescribeModelResponse Lude.Int
dmrsResponseStatus = Lens.lens (responseStatus :: DescribeModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeModelResponse)
{-# DEPRECATED dmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

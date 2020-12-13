{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
module Network.AWS.SageMaker.DescribeFlowDefinition
  ( -- * Creating a request
    DescribeFlowDefinition (..),
    mkDescribeFlowDefinition,

    -- ** Request lenses
    dFlowDefinitionName,

    -- * Destructuring the response
    DescribeFlowDefinitionResponse (..),
    mkDescribeFlowDefinitionResponse,

    -- ** Response lenses
    dfdfrsCreationTime,
    dfdfrsFailureReason,
    dfdfrsFlowDefinitionStatus,
    dfdfrsHumanLoopConfig,
    dfdfrsHumanLoopRequestSource,
    dfdfrsFlowDefinitionARN,
    dfdfrsOutputConfig,
    dfdfrsHumanLoopActivationConfig,
    dfdfrsFlowDefinitionName,
    dfdfrsRoleARN,
    dfdfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeFlowDefinition' smart constructor.
newtype DescribeFlowDefinition = DescribeFlowDefinition'
  { -- | The name of the flow definition.
    flowDefinitionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFlowDefinition' with the minimum fields required to make a request.
--
-- * 'flowDefinitionName' - The name of the flow definition.
mkDescribeFlowDefinition ::
  -- | 'flowDefinitionName'
  Lude.Text ->
  DescribeFlowDefinition
mkDescribeFlowDefinition pFlowDefinitionName_ =
  DescribeFlowDefinition'
    { flowDefinitionName =
        pFlowDefinitionName_
    }

-- | The name of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFlowDefinitionName :: Lens.Lens' DescribeFlowDefinition Lude.Text
dFlowDefinitionName = Lens.lens (flowDefinitionName :: DescribeFlowDefinition -> Lude.Text) (\s a -> s {flowDefinitionName = a} :: DescribeFlowDefinition)
{-# DEPRECATED dFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

instance Lude.AWSRequest DescribeFlowDefinition where
  type Rs DescribeFlowDefinition = DescribeFlowDefinitionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFlowDefinitionResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..:> "FlowDefinitionStatus")
            Lude.<*> (x Lude..:> "HumanLoopConfig")
            Lude.<*> (x Lude..?> "HumanLoopRequestSource")
            Lude.<*> (x Lude..:> "FlowDefinitionArn")
            Lude.<*> (x Lude..:> "OutputConfig")
            Lude.<*> (x Lude..?> "HumanLoopActivationConfig")
            Lude.<*> (x Lude..:> "FlowDefinitionName")
            Lude.<*> (x Lude..:> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFlowDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeFlowDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFlowDefinition where
  toJSON DescribeFlowDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("FlowDefinitionName" Lude..= flowDefinitionName)]
      )

instance Lude.ToPath DescribeFlowDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFlowDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { -- | The timestamp when the flow definition was created.
    creationTime :: Lude.Timestamp,
    -- | The reason your flow definition failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The status of the flow definition. Valid values are listed below.
    flowDefinitionStatus :: FlowDefinitionStatus,
    -- | An object containing information about who works on the task, the workforce task price, and other task details.
    humanLoopConfig :: HumanLoopConfig,
    -- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
    humanLoopRequestSource :: Lude.Maybe HumanLoopRequestSource,
    -- | The Amazon Resource Name (ARN) of the flow defintion.
    flowDefinitionARN :: Lude.Text,
    -- | An object containing information about the output file.
    outputConfig :: FlowDefinitionOutputConfig,
    -- | An object containing information about what triggers a human review workflow.
    humanLoopActivationConfig :: Lude.Maybe HumanLoopActivationConfig,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
    roleARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The timestamp when the flow definition was created.
-- * 'failureReason' - The reason your flow definition failed.
-- * 'flowDefinitionStatus' - The status of the flow definition. Valid values are listed below.
-- * 'humanLoopConfig' - An object containing information about who works on the task, the workforce task price, and other task details.
-- * 'humanLoopRequestSource' - Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
-- * 'flowDefinitionARN' - The Amazon Resource Name (ARN) of the flow defintion.
-- * 'outputConfig' - An object containing information about the output file.
-- * 'humanLoopActivationConfig' - An object containing information about what triggers a human review workflow.
-- * 'flowDefinitionName' - The Amazon Resource Name (ARN) of the flow definition.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
-- * 'responseStatus' - The response status code.
mkDescribeFlowDefinitionResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'flowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'humanLoopConfig'
  HumanLoopConfig ->
  -- | 'flowDefinitionARN'
  Lude.Text ->
  -- | 'outputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'flowDefinitionName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFlowDefinitionResponse
mkDescribeFlowDefinitionResponse
  pCreationTime_
  pFlowDefinitionStatus_
  pHumanLoopConfig_
  pFlowDefinitionARN_
  pOutputConfig_
  pFlowDefinitionName_
  pRoleARN_
  pResponseStatus_ =
    DescribeFlowDefinitionResponse'
      { creationTime = pCreationTime_,
        failureReason = Lude.Nothing,
        flowDefinitionStatus = pFlowDefinitionStatus_,
        humanLoopConfig = pHumanLoopConfig_,
        humanLoopRequestSource = Lude.Nothing,
        flowDefinitionARN = pFlowDefinitionARN_,
        outputConfig = pOutputConfig_,
        humanLoopActivationConfig = Lude.Nothing,
        flowDefinitionName = pFlowDefinitionName_,
        roleARN = pRoleARN_,
        responseStatus = pResponseStatus_
      }

-- | The timestamp when the flow definition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsCreationTime :: Lens.Lens' DescribeFlowDefinitionResponse Lude.Timestamp
dfdfrsCreationTime = Lens.lens (creationTime :: DescribeFlowDefinitionResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The reason your flow definition failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsFailureReason :: Lens.Lens' DescribeFlowDefinitionResponse (Lude.Maybe Lude.Text)
dfdfrsFailureReason = Lens.lens (failureReason :: DescribeFlowDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the flow definition. Valid values are listed below.
--
-- /Note:/ Consider using 'flowDefinitionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsFlowDefinitionStatus :: Lens.Lens' DescribeFlowDefinitionResponse FlowDefinitionStatus
dfdfrsFlowDefinitionStatus = Lens.lens (flowDefinitionStatus :: DescribeFlowDefinitionResponse -> FlowDefinitionStatus) (\s a -> s {flowDefinitionStatus = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsFlowDefinitionStatus "Use generic-lens or generic-optics with 'flowDefinitionStatus' instead." #-}

-- | An object containing information about who works on the task, the workforce task price, and other task details.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsHumanLoopConfig :: Lens.Lens' DescribeFlowDefinitionResponse HumanLoopConfig
dfdfrsHumanLoopConfig = Lens.lens (humanLoopConfig :: DescribeFlowDefinitionResponse -> HumanLoopConfig) (\s a -> s {humanLoopConfig = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsHumanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead." #-}

-- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- /Note:/ Consider using 'humanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsHumanLoopRequestSource :: Lens.Lens' DescribeFlowDefinitionResponse (Lude.Maybe HumanLoopRequestSource)
dfdfrsHumanLoopRequestSource = Lens.lens (humanLoopRequestSource :: DescribeFlowDefinitionResponse -> Lude.Maybe HumanLoopRequestSource) (\s a -> s {humanLoopRequestSource = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsHumanLoopRequestSource "Use generic-lens or generic-optics with 'humanLoopRequestSource' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow defintion.
--
-- /Note:/ Consider using 'flowDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsFlowDefinitionARN :: Lens.Lens' DescribeFlowDefinitionResponse Lude.Text
dfdfrsFlowDefinitionARN = Lens.lens (flowDefinitionARN :: DescribeFlowDefinitionResponse -> Lude.Text) (\s a -> s {flowDefinitionARN = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsFlowDefinitionARN "Use generic-lens or generic-optics with 'flowDefinitionARN' instead." #-}

-- | An object containing information about the output file.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsOutputConfig :: Lens.Lens' DescribeFlowDefinitionResponse FlowDefinitionOutputConfig
dfdfrsOutputConfig = Lens.lens (outputConfig :: DescribeFlowDefinitionResponse -> FlowDefinitionOutputConfig) (\s a -> s {outputConfig = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | An object containing information about what triggers a human review workflow.
--
-- /Note:/ Consider using 'humanLoopActivationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsHumanLoopActivationConfig :: Lens.Lens' DescribeFlowDefinitionResponse (Lude.Maybe HumanLoopActivationConfig)
dfdfrsHumanLoopActivationConfig = Lens.lens (humanLoopActivationConfig :: DescribeFlowDefinitionResponse -> Lude.Maybe HumanLoopActivationConfig) (\s a -> s {humanLoopActivationConfig = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsHumanLoopActivationConfig "Use generic-lens or generic-optics with 'humanLoopActivationConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsFlowDefinitionName :: Lens.Lens' DescribeFlowDefinitionResponse Lude.Text
dfdfrsFlowDefinitionName = Lens.lens (flowDefinitionName :: DescribeFlowDefinitionResponse -> Lude.Text) (\s a -> s {flowDefinitionName = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsRoleARN :: Lens.Lens' DescribeFlowDefinitionResponse Lude.Text
dfdfrsRoleARN = Lens.lens (roleARN :: DescribeFlowDefinitionResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdfrsResponseStatus :: Lens.Lens' DescribeFlowDefinitionResponse Lude.Int
dfdfrsResponseStatus = Lens.lens (responseStatus :: DescribeFlowDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFlowDefinitionResponse)
{-# DEPRECATED dfdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

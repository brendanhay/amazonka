{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow definition.
module Network.AWS.SageMaker.CreateFlowDefinition
  ( -- * Creating a request
    CreateFlowDefinition (..),
    mkCreateFlowDefinition,

    -- ** Request lenses
    cfdHumanLoopRequestSource,
    cfdHumanLoopActivationConfig,
    cfdTags,
    cfdFlowDefinitionName,
    cfdHumanLoopConfig,
    cfdOutputConfig,
    cfdRoleARN,

    -- * Destructuring the response
    CreateFlowDefinitionResponse (..),
    mkCreateFlowDefinitionResponse,

    -- ** Response lenses
    cfdrsResponseStatus,
    cfdrsFlowDefinitionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateFlowDefinition' smart constructor.
data CreateFlowDefinition = CreateFlowDefinition'
  { humanLoopRequestSource ::
      Lude.Maybe HumanLoopRequestSource,
    humanLoopActivationConfig ::
      Lude.Maybe HumanLoopActivationConfig,
    tags :: Lude.Maybe [Tag],
    flowDefinitionName :: Lude.Text,
    humanLoopConfig :: HumanLoopConfig,
    outputConfig :: FlowDefinitionOutputConfig,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFlowDefinition' with the minimum fields required to make a request.
--
-- * 'flowDefinitionName' - The name of your flow definition.
-- * 'humanLoopActivationConfig' - An object containing information about the events that trigger a human workflow.
-- * 'humanLoopConfig' - An object containing information about the tasks the human reviewers will perform.
-- * 'humanLoopRequestSource' - Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
-- * 'outputConfig' - An object containing information about where the human review results will be uploaded.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
-- * 'tags' - An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
mkCreateFlowDefinition ::
  -- | 'flowDefinitionName'
  Lude.Text ->
  -- | 'humanLoopConfig'
  HumanLoopConfig ->
  -- | 'outputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'roleARN'
  Lude.Text ->
  CreateFlowDefinition
mkCreateFlowDefinition
  pFlowDefinitionName_
  pHumanLoopConfig_
  pOutputConfig_
  pRoleARN_ =
    CreateFlowDefinition'
      { humanLoopRequestSource = Lude.Nothing,
        humanLoopActivationConfig = Lude.Nothing,
        tags = Lude.Nothing,
        flowDefinitionName = pFlowDefinitionName_,
        humanLoopConfig = pHumanLoopConfig_,
        outputConfig = pOutputConfig_,
        roleARN = pRoleARN_
      }

-- | Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- /Note:/ Consider using 'humanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopRequestSource :: Lens.Lens' CreateFlowDefinition (Lude.Maybe HumanLoopRequestSource)
cfdHumanLoopRequestSource = Lens.lens (humanLoopRequestSource :: CreateFlowDefinition -> Lude.Maybe HumanLoopRequestSource) (\s a -> s {humanLoopRequestSource = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdHumanLoopRequestSource "Use generic-lens or generic-optics with 'humanLoopRequestSource' instead." #-}

-- | An object containing information about the events that trigger a human workflow.
--
-- /Note:/ Consider using 'humanLoopActivationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopActivationConfig :: Lens.Lens' CreateFlowDefinition (Lude.Maybe HumanLoopActivationConfig)
cfdHumanLoopActivationConfig = Lens.lens (humanLoopActivationConfig :: CreateFlowDefinition -> Lude.Maybe HumanLoopActivationConfig) (\s a -> s {humanLoopActivationConfig = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdHumanLoopActivationConfig "Use generic-lens or generic-optics with 'humanLoopActivationConfig' instead." #-}

-- | An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdTags :: Lens.Lens' CreateFlowDefinition (Lude.Maybe [Tag])
cfdTags = Lens.lens (tags :: CreateFlowDefinition -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of your flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdFlowDefinitionName :: Lens.Lens' CreateFlowDefinition Lude.Text
cfdFlowDefinitionName = Lens.lens (flowDefinitionName :: CreateFlowDefinition -> Lude.Text) (\s a -> s {flowDefinitionName = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

-- | An object containing information about the tasks the human reviewers will perform.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopConfig :: Lens.Lens' CreateFlowDefinition HumanLoopConfig
cfdHumanLoopConfig = Lens.lens (humanLoopConfig :: CreateFlowDefinition -> HumanLoopConfig) (\s a -> s {humanLoopConfig = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdHumanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead." #-}

-- | An object containing information about where the human review results will be uploaded.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdOutputConfig :: Lens.Lens' CreateFlowDefinition FlowDefinitionOutputConfig
cfdOutputConfig = Lens.lens (outputConfig :: CreateFlowDefinition -> FlowDefinitionOutputConfig) (\s a -> s {outputConfig = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdRoleARN :: Lens.Lens' CreateFlowDefinition Lude.Text
cfdRoleARN = Lens.lens (roleARN :: CreateFlowDefinition -> Lude.Text) (\s a -> s {roleARN = a} :: CreateFlowDefinition)
{-# DEPRECATED cfdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateFlowDefinition where
  type Rs CreateFlowDefinition = CreateFlowDefinitionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFlowDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "FlowDefinitionArn")
      )

instance Lude.ToHeaders CreateFlowDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateFlowDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateFlowDefinition where
  toJSON CreateFlowDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HumanLoopRequestSource" Lude..=)
              Lude.<$> humanLoopRequestSource,
            ("HumanLoopActivationConfig" Lude..=)
              Lude.<$> humanLoopActivationConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("FlowDefinitionName" Lude..= flowDefinitionName),
            Lude.Just ("HumanLoopConfig" Lude..= humanLoopConfig),
            Lude.Just ("OutputConfig" Lude..= outputConfig),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateFlowDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFlowDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFlowDefinitionResponse' smart constructor.
data CreateFlowDefinitionResponse = CreateFlowDefinitionResponse'
  { responseStatus ::
      Lude.Int,
    flowDefinitionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'flowDefinitionARN' - The Amazon Resource Name (ARN) of the flow definition you create.
-- * 'responseStatus' - The response status code.
mkCreateFlowDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'flowDefinitionARN'
  Lude.Text ->
  CreateFlowDefinitionResponse
mkCreateFlowDefinitionResponse pResponseStatus_ pFlowDefinitionARN_ =
  CreateFlowDefinitionResponse'
    { responseStatus = pResponseStatus_,
      flowDefinitionARN = pFlowDefinitionARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsResponseStatus :: Lens.Lens' CreateFlowDefinitionResponse Lude.Int
cfdrsResponseStatus = Lens.lens (responseStatus :: CreateFlowDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFlowDefinitionResponse)
{-# DEPRECATED cfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition you create.
--
-- /Note:/ Consider using 'flowDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsFlowDefinitionARN :: Lens.Lens' CreateFlowDefinitionResponse Lude.Text
cfdrsFlowDefinitionARN = Lens.lens (flowDefinitionARN :: CreateFlowDefinitionResponse -> Lude.Text) (\s a -> s {flowDefinitionARN = a} :: CreateFlowDefinitionResponse)
{-# DEPRECATED cfdrsFlowDefinitionARN "Use generic-lens or generic-optics with 'flowDefinitionARN' instead." #-}

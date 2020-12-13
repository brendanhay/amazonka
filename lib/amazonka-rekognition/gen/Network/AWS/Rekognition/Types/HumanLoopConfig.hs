{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopConfig
  ( HumanLoopConfig (..),

    -- * Smart constructor
    mkHumanLoopConfig,

    -- * Lenses
    hlcHumanLoopName,
    hlcDataAttributes,
    hlcFlowDefinitionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes

-- | Sets up the flow definition the image will be sent to if one of the conditions is met. You can also set certain attributes of the image before review.
--
-- /See:/ 'mkHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | The name of the human review used for this image. This should be kept unique within a region.
    humanLoopName :: Lude.Text,
    -- | Sets attributes of the input data.
    dataAttributes :: Lude.Maybe HumanLoopDataAttributes,
    -- | The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
    flowDefinitionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopConfig' with the minimum fields required to make a request.
--
-- * 'humanLoopName' - The name of the human review used for this image. This should be kept unique within a region.
-- * 'dataAttributes' - Sets attributes of the input data.
-- * 'flowDefinitionARN' - The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
mkHumanLoopConfig ::
  -- | 'humanLoopName'
  Lude.Text ->
  -- | 'flowDefinitionARN'
  Lude.Text ->
  HumanLoopConfig
mkHumanLoopConfig pHumanLoopName_ pFlowDefinitionARN_ =
  HumanLoopConfig'
    { humanLoopName = pHumanLoopName_,
      dataAttributes = Lude.Nothing,
      flowDefinitionARN = pFlowDefinitionARN_
    }

-- | The name of the human review used for this image. This should be kept unique within a region.
--
-- /Note:/ Consider using 'humanLoopName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcHumanLoopName :: Lens.Lens' HumanLoopConfig Lude.Text
hlcHumanLoopName = Lens.lens (humanLoopName :: HumanLoopConfig -> Lude.Text) (\s a -> s {humanLoopName = a} :: HumanLoopConfig)
{-# DEPRECATED hlcHumanLoopName "Use generic-lens or generic-optics with 'humanLoopName' instead." #-}

-- | Sets attributes of the input data.
--
-- /Note:/ Consider using 'dataAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcDataAttributes :: Lens.Lens' HumanLoopConfig (Lude.Maybe HumanLoopDataAttributes)
hlcDataAttributes = Lens.lens (dataAttributes :: HumanLoopConfig -> Lude.Maybe HumanLoopDataAttributes) (\s a -> s {dataAttributes = a} :: HumanLoopConfig)
{-# DEPRECATED hlcDataAttributes "Use generic-lens or generic-optics with 'dataAttributes' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
--
-- /Note:/ Consider using 'flowDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcFlowDefinitionARN :: Lens.Lens' HumanLoopConfig Lude.Text
hlcFlowDefinitionARN = Lens.lens (flowDefinitionARN :: HumanLoopConfig -> Lude.Text) (\s a -> s {flowDefinitionARN = a} :: HumanLoopConfig)
{-# DEPRECATED hlcFlowDefinitionARN "Use generic-lens or generic-optics with 'flowDefinitionARN' instead." #-}

instance Lude.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("HumanLoopName" Lude..= humanLoopName),
            ("DataAttributes" Lude..=) Lude.<$> dataAttributes,
            Lude.Just ("FlowDefinitionArn" Lude..= flowDefinitionARN)
          ]
      )

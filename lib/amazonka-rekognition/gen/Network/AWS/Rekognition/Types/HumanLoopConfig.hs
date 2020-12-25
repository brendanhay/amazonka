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
    hlcFlowDefinitionArn,
    hlcDataAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.FlowDefinitionArn as Types
import qualified Network.AWS.Rekognition.Types.HumanLoopDataAttributes as Types
import qualified Network.AWS.Rekognition.Types.HumanLoopName as Types

-- | Sets up the flow definition the image will be sent to if one of the conditions is met. You can also set certain attributes of the image before review.
--
-- /See:/ 'mkHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | The name of the human review used for this image. This should be kept unique within a region.
    humanLoopName :: Types.HumanLoopName,
    -- | The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
    flowDefinitionArn :: Types.FlowDefinitionArn,
    -- | Sets attributes of the input data.
    dataAttributes :: Core.Maybe Types.HumanLoopDataAttributes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopConfig' value with any optional fields omitted.
mkHumanLoopConfig ::
  -- | 'humanLoopName'
  Types.HumanLoopName ->
  -- | 'flowDefinitionArn'
  Types.FlowDefinitionArn ->
  HumanLoopConfig
mkHumanLoopConfig humanLoopName flowDefinitionArn =
  HumanLoopConfig'
    { humanLoopName,
      flowDefinitionArn,
      dataAttributes = Core.Nothing
    }

-- | The name of the human review used for this image. This should be kept unique within a region.
--
-- /Note:/ Consider using 'humanLoopName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcHumanLoopName :: Lens.Lens' HumanLoopConfig Types.HumanLoopName
hlcHumanLoopName = Lens.field @"humanLoopName"
{-# DEPRECATED hlcHumanLoopName "Use generic-lens or generic-optics with 'humanLoopName' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
--
-- /Note:/ Consider using 'flowDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcFlowDefinitionArn :: Lens.Lens' HumanLoopConfig Types.FlowDefinitionArn
hlcFlowDefinitionArn = Lens.field @"flowDefinitionArn"
{-# DEPRECATED hlcFlowDefinitionArn "Use generic-lens or generic-optics with 'flowDefinitionArn' instead." #-}

-- | Sets attributes of the input data.
--
-- /Note:/ Consider using 'dataAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcDataAttributes :: Lens.Lens' HumanLoopConfig (Core.Maybe Types.HumanLoopDataAttributes)
hlcDataAttributes = Lens.field @"dataAttributes"
{-# DEPRECATED hlcDataAttributes "Use generic-lens or generic-optics with 'dataAttributes' instead." #-}

instance Core.FromJSON HumanLoopConfig where
  toJSON HumanLoopConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HumanLoopName" Core..= humanLoopName),
            Core.Just ("FlowDefinitionArn" Core..= flowDefinitionArn),
            ("DataAttributes" Core..=) Core.<$> dataAttributes
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PublishFindingToSnsParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.PublishFindingToSnsParams
  ( PublishFindingToSnsParams (..)
  -- * Smart constructor
  , mkPublishFindingToSnsParams
  -- * Lenses
  , pftspTopicArn
  ) where

import qualified Network.AWS.IoT.Types.SnsTopicArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
--
-- /See:/ 'mkPublishFindingToSnsParams' smart constructor.
newtype PublishFindingToSnsParams = PublishFindingToSnsParams'
  { topicArn :: Types.SnsTopicArn
    -- ^ The ARN of the topic to which you want to publish the findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PublishFindingToSnsParams' value with any optional fields omitted.
mkPublishFindingToSnsParams
    :: Types.SnsTopicArn -- ^ 'topicArn'
    -> PublishFindingToSnsParams
mkPublishFindingToSnsParams topicArn
  = PublishFindingToSnsParams'{topicArn}

-- | The ARN of the topic to which you want to publish the findings.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftspTopicArn :: Lens.Lens' PublishFindingToSnsParams Types.SnsTopicArn
pftspTopicArn = Lens.field @"topicArn"
{-# INLINEABLE pftspTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.FromJSON PublishFindingToSnsParams where
        toJSON PublishFindingToSnsParams{..}
          = Core.object
              (Core.catMaybes [Core.Just ("topicArn" Core..= topicArn)])

instance Core.FromJSON PublishFindingToSnsParams where
        parseJSON
          = Core.withObject "PublishFindingToSnsParams" Core.$
              \ x -> PublishFindingToSnsParams' Core.<$> (x Core..: "topicArn")

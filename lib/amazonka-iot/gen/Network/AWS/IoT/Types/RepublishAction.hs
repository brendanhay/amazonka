{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RepublishAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.RepublishAction
  ( RepublishAction (..)
  -- * Smart constructor
  , mkRepublishAction
  -- * Lenses
  , raRoleArn
  , raTopic
  , raQos
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.Topic as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to republish to another topic.
--
-- /See:/ 'mkRepublishAction' smart constructor.
data RepublishAction = RepublishAction'
  { roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , topic :: Types.Topic
    -- ^ The name of the MQTT topic.
  , qos :: Core.Maybe Core.Natural
    -- ^ The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RepublishAction' value with any optional fields omitted.
mkRepublishAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.Topic -- ^ 'topic'
    -> RepublishAction
mkRepublishAction roleArn topic
  = RepublishAction'{roleArn, topic, qos = Core.Nothing}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRoleArn :: Lens.Lens' RepublishAction Types.AwsArn
raRoleArn = Lens.field @"roleArn"
{-# INLINEABLE raRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the MQTT topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTopic :: Lens.Lens' RepublishAction Types.Topic
raTopic = Lens.field @"topic"
{-# INLINEABLE raTopic #-}
{-# DEPRECATED topic "Use generic-lens or generic-optics with 'topic' instead"  #-}

-- | The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
--
-- /Note:/ Consider using 'qos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raQos :: Lens.Lens' RepublishAction (Core.Maybe Core.Natural)
raQos = Lens.field @"qos"
{-# INLINEABLE raQos #-}
{-# DEPRECATED qos "Use generic-lens or generic-optics with 'qos' instead"  #-}

instance Core.FromJSON RepublishAction where
        toJSON RepublishAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("topic" Core..= topic), ("qos" Core..=) Core.<$> qos])

instance Core.FromJSON RepublishAction where
        parseJSON
          = Core.withObject "RepublishAction" Core.$
              \ x ->
                RepublishAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "topic" Core.<*>
                    x Core..:? "qos"

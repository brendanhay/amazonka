{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Topic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types.Topic
  ( Topic (..)
  -- * Smart constructor
  , mkTopic
  -- * Lenses
  , tTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SNS.Types.TopicArn as Types

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use @GetTopicAttributes@ .
--
-- /See:/ 'mkTopic' smart constructor.
newtype Topic = Topic'
  { topicArn :: Core.Maybe Types.TopicArn
    -- ^ The topic's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Topic' value with any optional fields omitted.
mkTopic
    :: Topic
mkTopic = Topic'{topicArn = Core.Nothing}

-- | The topic's ARN.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTopicArn :: Lens.Lens' Topic (Core.Maybe Types.TopicArn)
tTopicArn = Lens.field @"topicArn"
{-# INLINEABLE tTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.FromXML Topic where
        parseXML x = Topic' Core.<$> (x Core..@? "TopicArn")

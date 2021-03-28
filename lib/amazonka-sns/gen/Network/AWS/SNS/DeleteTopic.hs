{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might prevent some messages previously sent to the topic from being delivered to subscribers. This action is idempotent, so deleting a topic that does not exist does not result in an error.
module Network.AWS.SNS.DeleteTopic
    (
    -- * Creating a request
      DeleteTopic (..)
    , mkDeleteTopic
    -- ** Request lenses
    , dtTopicArn

    -- * Destructuring the response
    , DeleteTopicResponse (..)
    , mkDeleteTopicResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | /See:/ 'mkDeleteTopic' smart constructor.
newtype DeleteTopic = DeleteTopic'
  { topicArn :: Types.TopicArn
    -- ^ The ARN of the topic you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopic' value with any optional fields omitted.
mkDeleteTopic
    :: Types.TopicArn -- ^ 'topicArn'
    -> DeleteTopic
mkDeleteTopic topicArn = DeleteTopic'{topicArn}

-- | The ARN of the topic you want to delete.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTopicArn :: Lens.Lens' DeleteTopic Types.TopicArn
dtTopicArn = Lens.field @"topicArn"
{-# INLINEABLE dtTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery DeleteTopic where
        toQuery DeleteTopic{..}
          = Core.toQueryPair "Action" ("DeleteTopic" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn

instance Core.ToHeaders DeleteTopic where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTopic where
        type Rs DeleteTopic = DeleteTopicResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteTopicResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicResponse' value with any optional fields omitted.
mkDeleteTopicResponse
    :: DeleteTopicResponse
mkDeleteTopicResponse = DeleteTopicResponse'

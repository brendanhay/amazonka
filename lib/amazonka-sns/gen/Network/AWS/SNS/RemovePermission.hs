{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from a topic's access control policy.
module Network.AWS.SNS.RemovePermission
    (
    -- * Creating a request
      RemovePermission (..)
    , mkRemovePermission
    -- ** Request lenses
    , rpTopicArn
    , rpLabel

    -- * Destructuring the response
    , RemovePermissionResponse (..)
    , mkRemovePermissionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for RemovePermission action.
--
-- /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { topicArn :: Types.TopicArn
    -- ^ The ARN of the topic whose access control policy you wish to modify.
  , label :: Types.Label
    -- ^ The unique label of the statement you want to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermission' value with any optional fields omitted.
mkRemovePermission
    :: Types.TopicArn -- ^ 'topicArn'
    -> Types.Label -- ^ 'label'
    -> RemovePermission
mkRemovePermission topicArn label
  = RemovePermission'{topicArn, label}

-- | The ARN of the topic whose access control policy you wish to modify.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpTopicArn :: Lens.Lens' RemovePermission Types.TopicArn
rpTopicArn = Lens.field @"topicArn"
{-# INLINEABLE rpTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | The unique label of the statement you want to remove.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLabel :: Lens.Lens' RemovePermission Types.Label
rpLabel = Lens.field @"label"
{-# INLINEABLE rpLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

instance Core.ToQuery RemovePermission where
        toQuery RemovePermission{..}
          = Core.toQueryPair "Action" ("RemovePermission" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn
              Core.<> Core.toQueryPair "Label" label

instance Core.ToHeaders RemovePermission where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemovePermission where
        type Rs RemovePermission = RemovePermissionResponse
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
        parseResponse = Response.receiveNull RemovePermissionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermissionResponse' value with any optional fields omitted.
mkRemovePermissionResponse
    :: RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'

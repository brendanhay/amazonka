{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes any permissions in the queue policy that matches the specified @Label@ parameter.
module Network.AWS.SQS.RemovePermission
    (
    -- * Creating a request
      RemovePermission (..)
    , mkRemovePermission
    -- ** Request lenses
    , rpQueueUrl
    , rpLabel

    -- * Destructuring the response
    , RemovePermissionResponse (..)
    , mkRemovePermissionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
  , label :: Core.Text
    -- ^ The identification of the permission to remove. This is the label added using the @'AddPermission' @ action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermission' value with any optional fields omitted.
mkRemovePermission
    :: Core.Text -- ^ 'queueUrl'
    -> Core.Text -- ^ 'label'
    -> RemovePermission
mkRemovePermission queueUrl label
  = RemovePermission'{queueUrl, label}

-- | The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpQueueUrl :: Lens.Lens' RemovePermission Core.Text
rpQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE rpQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The identification of the permission to remove. This is the label added using the @'AddPermission' @ action.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLabel :: Lens.Lens' RemovePermission Core.Text
rpLabel = Lens.field @"label"
{-# INLINEABLE rpLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

instance Core.ToQuery RemovePermission where
        toQuery RemovePermission{..}
          = Core.toQueryPair "Action" ("RemovePermission" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription to forward real-time Directory Service domain controller security logs to the specified Amazon CloudWatch log group in your AWS account.
module Network.AWS.DirectoryService.CreateLogSubscription
    (
    -- * Creating a request
      CreateLogSubscription (..)
    , mkCreateLogSubscription
    -- ** Request lenses
    , clsDirectoryId
    , clsLogGroupName

    -- * Destructuring the response
    , CreateLogSubscriptionResponse (..)
    , mkCreateLogSubscriptionResponse
    -- ** Response lenses
    , clsrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLogSubscription' smart constructor.
data CreateLogSubscription = CreateLogSubscription'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
  , logGroupName :: Types.LogGroupName
    -- ^ The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogSubscription' value with any optional fields omitted.
mkCreateLogSubscription
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.LogGroupName -- ^ 'logGroupName'
    -> CreateLogSubscription
mkCreateLogSubscription directoryId logGroupName
  = CreateLogSubscription'{directoryId, logGroupName}

-- | Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsDirectoryId :: Lens.Lens' CreateLogSubscription Types.DirectoryId
clsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE clsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogGroupName :: Lens.Lens' CreateLogSubscription Types.LogGroupName
clsLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE clsLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.ToQuery CreateLogSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLogSubscription where
        toHeaders CreateLogSubscription{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateLogSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLogSubscription where
        toJSON CreateLogSubscription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("LogGroupName" Core..= logGroupName)])

instance Core.AWSRequest CreateLogSubscription where
        type Rs CreateLogSubscription = CreateLogSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateLogSubscriptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLogSubscriptionResponse' smart constructor.
newtype CreateLogSubscriptionResponse = CreateLogSubscriptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogSubscriptionResponse' value with any optional fields omitted.
mkCreateLogSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLogSubscriptionResponse
mkCreateLogSubscriptionResponse responseStatus
  = CreateLogSubscriptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsrrsResponseStatus :: Lens.Lens' CreateLogSubscriptionResponse Core.Int
clsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

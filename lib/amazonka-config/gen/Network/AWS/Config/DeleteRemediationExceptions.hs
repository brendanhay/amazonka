{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more remediation exceptions mentioned in the resource keys.
module Network.AWS.Config.DeleteRemediationExceptions
    (
    -- * Creating a request
      DeleteRemediationExceptions (..)
    , mkDeleteRemediationExceptions
    -- ** Request lenses
    , dConfigRuleName
    , dResourceKeys

    -- * Destructuring the response
    , DeleteRemediationExceptionsResponse (..)
    , mkDeleteRemediationExceptionsResponse
    -- ** Response lenses
    , drerfrsFailedBatches
    , drerfrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRemediationExceptions' smart constructor.
data DeleteRemediationExceptions = DeleteRemediationExceptions'
  { configRuleName :: Types.ConfigRuleName
    -- ^ The name of the AWS Config rule for which you want to delete remediation exception configuration.
  , resourceKeys :: Core.NonEmpty Types.RemediationExceptionResourceKey
    -- ^ An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemediationExceptions' value with any optional fields omitted.
mkDeleteRemediationExceptions
    :: Types.ConfigRuleName -- ^ 'configRuleName'
    -> Core.NonEmpty Types.RemediationExceptionResourceKey -- ^ 'resourceKeys'
    -> DeleteRemediationExceptions
mkDeleteRemediationExceptions configRuleName resourceKeys
  = DeleteRemediationExceptions'{configRuleName, resourceKeys}

-- | The name of the AWS Config rule for which you want to delete remediation exception configuration.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigRuleName :: Lens.Lens' DeleteRemediationExceptions Types.ConfigRuleName
dConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE dConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys. 
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceKeys :: Lens.Lens' DeleteRemediationExceptions (Core.NonEmpty Types.RemediationExceptionResourceKey)
dResourceKeys = Lens.field @"resourceKeys"
{-# INLINEABLE dResourceKeys #-}
{-# DEPRECATED resourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead"  #-}

instance Core.ToQuery DeleteRemediationExceptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRemediationExceptions where
        toHeaders DeleteRemediationExceptions{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DeleteRemediationExceptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRemediationExceptions where
        toJSON DeleteRemediationExceptions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleName" Core..= configRuleName),
                  Core.Just ("ResourceKeys" Core..= resourceKeys)])

instance Core.AWSRequest DeleteRemediationExceptions where
        type Rs DeleteRemediationExceptions =
             DeleteRemediationExceptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRemediationExceptionsResponse' Core.<$>
                   (x Core..:? "FailedBatches") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRemediationExceptionsResponse' smart constructor.
data DeleteRemediationExceptionsResponse = DeleteRemediationExceptionsResponse'
  { failedBatches :: Core.Maybe [Types.FailedDeleteRemediationExceptionsBatch]
    -- ^ Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemediationExceptionsResponse' value with any optional fields omitted.
mkDeleteRemediationExceptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRemediationExceptionsResponse
mkDeleteRemediationExceptionsResponse responseStatus
  = DeleteRemediationExceptionsResponse'{failedBatches =
                                           Core.Nothing,
                                         responseStatus}

-- | Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerfrsFailedBatches :: Lens.Lens' DeleteRemediationExceptionsResponse (Core.Maybe [Types.FailedDeleteRemediationExceptionsBatch])
drerfrsFailedBatches = Lens.field @"failedBatches"
{-# INLINEABLE drerfrsFailedBatches #-}
{-# DEPRECATED failedBatches "Use generic-lens or generic-optics with 'failedBatches' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerfrsResponseStatus :: Lens.Lens' DeleteRemediationExceptionsResponse Core.Int
drerfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drerfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

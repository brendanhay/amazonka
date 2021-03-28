{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRemediationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the remediation configuration with a specific AWS Config rule with the selected target or action. The API creates the @RemediationConfiguration@ object for the AWS Config rule. The AWS Config rule must already exist for you to add a remediation configuration. The target (SSM document) must exist and have permissions to use the target. 
module Network.AWS.Config.PutRemediationConfigurations
    (
    -- * Creating a request
      PutRemediationConfigurations (..)
    , mkPutRemediationConfigurations
    -- ** Request lenses
    , prcRemediationConfigurations

    -- * Destructuring the response
    , PutRemediationConfigurationsResponse (..)
    , mkPutRemediationConfigurationsResponse
    -- ** Response lenses
    , prcrrsFailedBatches
    , prcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRemediationConfigurations' smart constructor.
newtype PutRemediationConfigurations = PutRemediationConfigurations'
  { remediationConfigurations :: [Types.RemediationConfiguration]
    -- ^ A list of remediation configuration objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutRemediationConfigurations' value with any optional fields omitted.
mkPutRemediationConfigurations
    :: PutRemediationConfigurations
mkPutRemediationConfigurations
  = PutRemediationConfigurations'{remediationConfigurations =
                                    Core.mempty}

-- | A list of remediation configuration objects.
--
-- /Note:/ Consider using 'remediationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcRemediationConfigurations :: Lens.Lens' PutRemediationConfigurations [Types.RemediationConfiguration]
prcRemediationConfigurations = Lens.field @"remediationConfigurations"
{-# INLINEABLE prcRemediationConfigurations #-}
{-# DEPRECATED remediationConfigurations "Use generic-lens or generic-optics with 'remediationConfigurations' instead"  #-}

instance Core.ToQuery PutRemediationConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutRemediationConfigurations where
        toHeaders PutRemediationConfigurations{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.PutRemediationConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutRemediationConfigurations where
        toJSON PutRemediationConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("RemediationConfigurations" Core..= remediationConfigurations)])

instance Core.AWSRequest PutRemediationConfigurations where
        type Rs PutRemediationConfigurations =
             PutRemediationConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutRemediationConfigurationsResponse' Core.<$>
                   (x Core..:? "FailedBatches") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { failedBatches :: Core.Maybe [Types.FailedRemediationBatch]
    -- ^ Returns a list of failed remediation batch objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRemediationConfigurationsResponse' value with any optional fields omitted.
mkPutRemediationConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutRemediationConfigurationsResponse
mkPutRemediationConfigurationsResponse responseStatus
  = PutRemediationConfigurationsResponse'{failedBatches =
                                            Core.Nothing,
                                          responseStatus}

-- | Returns a list of failed remediation batch objects.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrrsFailedBatches :: Lens.Lens' PutRemediationConfigurationsResponse (Core.Maybe [Types.FailedRemediationBatch])
prcrrsFailedBatches = Lens.field @"failedBatches"
{-# INLINEABLE prcrrsFailedBatches #-}
{-# DEPRECATED failedBatches "Use generic-lens or generic-optics with 'failedBatches' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrrsResponseStatus :: Lens.Lens' PutRemediationConfigurationsResponse Core.Int
prcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a replication instance).
module Network.AWS.DMS.ApplyPendingMaintenanceAction
    (
    -- * Creating a request
      ApplyPendingMaintenanceAction (..)
    , mkApplyPendingMaintenanceAction
    -- ** Request lenses
    , apmaReplicationInstanceArn
    , apmaApplyAction
    , apmaOptInType

    -- * Destructuring the response
    , ApplyPendingMaintenanceActionResponse (..)
    , mkApplyPendingMaintenanceActionResponse
    -- ** Response lenses
    , apmarrsResourcePendingMaintenanceActions
    , apmarrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
  , applyAction :: Core.Text
    -- ^ The pending maintenance action to apply to this resource.
  , optInType :: Core.Text
    -- ^ A value that specifies the type of opt-in request, or undoes an opt-in request. You can't undo an opt-in request of type @immediate@ .
--
-- Valid values:
--
--     * @immediate@ - Apply the maintenance action immediately.
--
--
--     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
--
--
--     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyPendingMaintenanceAction' value with any optional fields omitted.
mkApplyPendingMaintenanceAction
    :: Core.Text -- ^ 'replicationInstanceArn'
    -> Core.Text -- ^ 'applyAction'
    -> Core.Text -- ^ 'optInType'
    -> ApplyPendingMaintenanceAction
mkApplyPendingMaintenanceAction replicationInstanceArn applyAction
  optInType
  = ApplyPendingMaintenanceAction'{replicationInstanceArn,
                                   applyAction, optInType}

-- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaReplicationInstanceArn :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
apmaReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE apmaReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The pending maintenance action to apply to this resource.
--
-- /Note:/ Consider using 'applyAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaApplyAction :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
apmaApplyAction = Lens.field @"applyAction"
{-# INLINEABLE apmaApplyAction #-}
{-# DEPRECATED applyAction "Use generic-lens or generic-optics with 'applyAction' instead"  #-}

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. You can't undo an opt-in request of type @immediate@ .
--
-- Valid values:
--
--     * @immediate@ - Apply the maintenance action immediately.
--
--
--     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
--
--
--     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
--
--
--
-- /Note:/ Consider using 'optInType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaOptInType :: Lens.Lens' ApplyPendingMaintenanceAction Core.Text
apmaOptInType = Lens.field @"optInType"
{-# INLINEABLE apmaOptInType #-}
{-# DEPRECATED optInType "Use generic-lens or generic-optics with 'optInType' instead"  #-}

instance Core.ToQuery ApplyPendingMaintenanceAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ApplyPendingMaintenanceAction where
        toHeaders ApplyPendingMaintenanceAction{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.ApplyPendingMaintenanceAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ApplyPendingMaintenanceAction where
        toJSON ApplyPendingMaintenanceAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn),
                  Core.Just ("ApplyAction" Core..= applyAction),
                  Core.Just ("OptInType" Core..= optInType)])

instance Core.AWSRequest ApplyPendingMaintenanceAction where
        type Rs ApplyPendingMaintenanceAction =
             ApplyPendingMaintenanceActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ApplyPendingMaintenanceActionResponse' Core.<$>
                   (x Core..:? "ResourcePendingMaintenanceActions") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { resourcePendingMaintenanceActions :: Core.Maybe Types.ResourcePendingMaintenanceActions
    -- ^ The AWS DMS resource that the pending maintenance action will be applied to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ApplyPendingMaintenanceActionResponse' value with any optional fields omitted.
mkApplyPendingMaintenanceActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ApplyPendingMaintenanceActionResponse
mkApplyPendingMaintenanceActionResponse responseStatus
  = ApplyPendingMaintenanceActionResponse'{resourcePendingMaintenanceActions
                                             = Core.Nothing,
                                           responseStatus}

-- | The AWS DMS resource that the pending maintenance action will be applied to.
--
-- /Note:/ Consider using 'resourcePendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarrsResourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Core.Maybe Types.ResourcePendingMaintenanceActions)
apmarrsResourcePendingMaintenanceActions = Lens.field @"resourcePendingMaintenanceActions"
{-# INLINEABLE apmarrsResourcePendingMaintenanceActions #-}
{-# DEPRECATED resourcePendingMaintenanceActions "Use generic-lens or generic-optics with 'resourcePendingMaintenanceActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarrsResponseStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Core.Int
apmarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE apmarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

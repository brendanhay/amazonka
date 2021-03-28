{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run. If a property already exists for the specified run, then it overrides the value otherwise adds the property to existing properties.
module Network.AWS.Glue.PutWorkflowRunProperties
    (
    -- * Creating a request
      PutWorkflowRunProperties (..)
    , mkPutWorkflowRunProperties
    -- ** Request lenses
    , pwrpName
    , pwrpRunId
    , pwrpRunProperties

    -- * Destructuring the response
    , PutWorkflowRunPropertiesResponse (..)
    , mkPutWorkflowRunPropertiesResponse
    -- ** Response lenses
    , pwrprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutWorkflowRunProperties' smart constructor.
data PutWorkflowRunProperties = PutWorkflowRunProperties'
  { name :: Types.Name
    -- ^ Name of the workflow which was run.
  , runId :: Types.RunId
    -- ^ The ID of the workflow run for which the run properties should be updated.
  , runProperties :: Core.HashMap Types.IdString Types.GenericString
    -- ^ The properties to put for the specified run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutWorkflowRunProperties' value with any optional fields omitted.
mkPutWorkflowRunProperties
    :: Types.Name -- ^ 'name'
    -> Types.RunId -- ^ 'runId'
    -> PutWorkflowRunProperties
mkPutWorkflowRunProperties name runId
  = PutWorkflowRunProperties'{name, runId,
                              runProperties = Core.mempty}

-- | Name of the workflow which was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpName :: Lens.Lens' PutWorkflowRunProperties Types.Name
pwrpName = Lens.field @"name"
{-# INLINEABLE pwrpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the workflow run for which the run properties should be updated.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunId :: Lens.Lens' PutWorkflowRunProperties Types.RunId
pwrpRunId = Lens.field @"runId"
{-# INLINEABLE pwrpRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

-- | The properties to put for the specified run.
--
-- /Note:/ Consider using 'runProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunProperties :: Lens.Lens' PutWorkflowRunProperties (Core.HashMap Types.IdString Types.GenericString)
pwrpRunProperties = Lens.field @"runProperties"
{-# INLINEABLE pwrpRunProperties #-}
{-# DEPRECATED runProperties "Use generic-lens or generic-optics with 'runProperties' instead"  #-}

instance Core.ToQuery PutWorkflowRunProperties where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutWorkflowRunProperties where
        toHeaders PutWorkflowRunProperties{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.PutWorkflowRunProperties")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutWorkflowRunProperties where
        toJSON PutWorkflowRunProperties{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("RunId" Core..= runId),
                  Core.Just ("RunProperties" Core..= runProperties)])

instance Core.AWSRequest PutWorkflowRunProperties where
        type Rs PutWorkflowRunProperties = PutWorkflowRunPropertiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutWorkflowRunPropertiesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutWorkflowRunPropertiesResponse' smart constructor.
newtype PutWorkflowRunPropertiesResponse = PutWorkflowRunPropertiesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutWorkflowRunPropertiesResponse' value with any optional fields omitted.
mkPutWorkflowRunPropertiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutWorkflowRunPropertiesResponse
mkPutWorkflowRunPropertiesResponse responseStatus
  = PutWorkflowRunPropertiesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrprrsResponseStatus :: Lens.Lens' PutWorkflowRunPropertiesResponse Core.Int
pwrprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pwrprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

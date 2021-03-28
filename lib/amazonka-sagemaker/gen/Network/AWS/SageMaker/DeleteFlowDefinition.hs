{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Network.AWS.SageMaker.DeleteFlowDefinition
    (
    -- * Creating a request
      DeleteFlowDefinition (..)
    , mkDeleteFlowDefinition
    -- ** Request lenses
    , dfdFlowDefinitionName

    -- * Destructuring the response
    , DeleteFlowDefinitionResponse (..)
    , mkDeleteFlowDefinitionResponse
    -- ** Response lenses
    , dfdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteFlowDefinition' smart constructor.
newtype DeleteFlowDefinition = DeleteFlowDefinition'
  { flowDefinitionName :: Types.FlowDefinitionName
    -- ^ The name of the flow definition you are deleting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowDefinition' value with any optional fields omitted.
mkDeleteFlowDefinition
    :: Types.FlowDefinitionName -- ^ 'flowDefinitionName'
    -> DeleteFlowDefinition
mkDeleteFlowDefinition flowDefinitionName
  = DeleteFlowDefinition'{flowDefinitionName}

-- | The name of the flow definition you are deleting.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFlowDefinitionName :: Lens.Lens' DeleteFlowDefinition Types.FlowDefinitionName
dfdFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# INLINEABLE dfdFlowDefinitionName #-}
{-# DEPRECATED flowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead"  #-}

instance Core.ToQuery DeleteFlowDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFlowDefinition where
        toHeaders DeleteFlowDefinition{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteFlowDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteFlowDefinition where
        toJSON DeleteFlowDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FlowDefinitionName" Core..= flowDefinitionName)])

instance Core.AWSRequest DeleteFlowDefinition where
        type Rs DeleteFlowDefinition = DeleteFlowDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteFlowDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFlowDefinitionResponse' smart constructor.
newtype DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowDefinitionResponse' value with any optional fields omitted.
mkDeleteFlowDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFlowDefinitionResponse
mkDeleteFlowDefinitionResponse responseStatus
  = DeleteFlowDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrrsResponseStatus :: Lens.Lens' DeleteFlowDefinitionResponse Core.Int
dfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

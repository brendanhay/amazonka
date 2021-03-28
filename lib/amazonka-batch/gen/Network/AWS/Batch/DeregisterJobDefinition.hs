{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DeregisterJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an AWS Batch job definition. Job definitions will be permanently deleted after 180 days.
module Network.AWS.Batch.DeregisterJobDefinition
    (
    -- * Creating a request
      DeregisterJobDefinition (..)
    , mkDeregisterJobDefinition
    -- ** Request lenses
    , djdJobDefinition

    -- * Destructuring the response
    , DeregisterJobDefinitionResponse (..)
    , mkDeregisterJobDefinitionResponse
    -- ** Response lenses
    , djdrfrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterJobDefinition' smart constructor.
newtype DeregisterJobDefinition = DeregisterJobDefinition'
  { jobDefinition :: Core.Text
    -- ^ The name and revision (@name:revision@ ) or full Amazon Resource Name (ARN) of the job definition to deregister.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterJobDefinition' value with any optional fields omitted.
mkDeregisterJobDefinition
    :: Core.Text -- ^ 'jobDefinition'
    -> DeregisterJobDefinition
mkDeregisterJobDefinition jobDefinition
  = DeregisterJobDefinition'{jobDefinition}

-- | The name and revision (@name:revision@ ) or full Amazon Resource Name (ARN) of the job definition to deregister.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdJobDefinition :: Lens.Lens' DeregisterJobDefinition Core.Text
djdJobDefinition = Lens.field @"jobDefinition"
{-# INLINEABLE djdJobDefinition #-}
{-# DEPRECATED jobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead"  #-}

instance Core.ToQuery DeregisterJobDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterJobDefinition where
        toHeaders DeregisterJobDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterJobDefinition where
        toJSON DeregisterJobDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobDefinition" Core..= jobDefinition)])

instance Core.AWSRequest DeregisterJobDefinition where
        type Rs DeregisterJobDefinition = DeregisterJobDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/deregisterjobdefinition",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeregisterJobDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterJobDefinitionResponse' smart constructor.
newtype DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterJobDefinitionResponse' value with any optional fields omitted.
mkDeregisterJobDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterJobDefinitionResponse
mkDeregisterJobDefinitionResponse responseStatus
  = DeregisterJobDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrfrsResponseStatus :: Lens.Lens' DeregisterJobDefinitionResponse Core.Int
djdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

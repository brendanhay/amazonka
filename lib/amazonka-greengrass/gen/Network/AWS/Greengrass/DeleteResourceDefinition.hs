{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource definition.
module Network.AWS.Greengrass.DeleteResourceDefinition
    (
    -- * Creating a request
      DeleteResourceDefinition (..)
    , mkDeleteResourceDefinition
    -- ** Request lenses
    , drdResourceDefinitionId

    -- * Destructuring the response
    , DeleteResourceDefinitionResponse (..)
    , mkDeleteResourceDefinitionResponse
    -- ** Response lenses
    , drdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourceDefinition' smart constructor.
newtype DeleteResourceDefinition = DeleteResourceDefinition'
  { resourceDefinitionId :: Core.Text
    -- ^ The ID of the resource definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDefinition' value with any optional fields omitted.
mkDeleteResourceDefinition
    :: Core.Text -- ^ 'resourceDefinitionId'
    -> DeleteResourceDefinition
mkDeleteResourceDefinition resourceDefinitionId
  = DeleteResourceDefinition'{resourceDefinitionId}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdResourceDefinitionId :: Lens.Lens' DeleteResourceDefinition Core.Text
drdResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# INLINEABLE drdResourceDefinitionId #-}
{-# DEPRECATED resourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead"  #-}

instance Core.ToQuery DeleteResourceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResourceDefinition where
        toHeaders DeleteResourceDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteResourceDefinition where
        type Rs DeleteResourceDefinition = DeleteResourceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/definition/resources/" Core.<>
                             Core.toText resourceDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteResourceDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceDefinitionResponse' smart constructor.
newtype DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDefinitionResponse' value with any optional fields omitted.
mkDeleteResourceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteResourceDefinitionResponse
mkDeleteResourceDefinitionResponse responseStatus
  = DeleteResourceDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrrsResponseStatus :: Lens.Lens' DeleteResourceDefinitionResponse Core.Int
drdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

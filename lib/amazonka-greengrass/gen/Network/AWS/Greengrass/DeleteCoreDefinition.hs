{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
    (
    -- * Creating a request
      DeleteCoreDefinition (..)
    , mkDeleteCoreDefinition
    -- ** Request lenses
    , dcdCoreDefinitionId

    -- * Destructuring the response
    , DeleteCoreDefinitionResponse (..)
    , mkDeleteCoreDefinitionResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCoreDefinition' smart constructor.
newtype DeleteCoreDefinition = DeleteCoreDefinition'
  { coreDefinitionId :: Core.Text
    -- ^ The ID of the core definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCoreDefinition' value with any optional fields omitted.
mkDeleteCoreDefinition
    :: Core.Text -- ^ 'coreDefinitionId'
    -> DeleteCoreDefinition
mkDeleteCoreDefinition coreDefinitionId
  = DeleteCoreDefinition'{coreDefinitionId}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdCoreDefinitionId :: Lens.Lens' DeleteCoreDefinition Core.Text
dcdCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# INLINEABLE dcdCoreDefinitionId #-}
{-# DEPRECATED coreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead"  #-}

instance Core.ToQuery DeleteCoreDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCoreDefinition where
        toHeaders DeleteCoreDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteCoreDefinition where
        type Rs DeleteCoreDefinition = DeleteCoreDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/definition/cores/" Core.<>
                             Core.toText coreDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCoreDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCoreDefinitionResponse' smart constructor.
newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCoreDefinitionResponse' value with any optional fields omitted.
mkDeleteCoreDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCoreDefinitionResponse
mkDeleteCoreDefinitionResponse responseStatus
  = DeleteCoreDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteCoreDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

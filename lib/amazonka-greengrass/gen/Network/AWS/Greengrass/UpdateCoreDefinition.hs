{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a core definition.
module Network.AWS.Greengrass.UpdateCoreDefinition
    (
    -- * Creating a request
      UpdateCoreDefinition (..)
    , mkUpdateCoreDefinition
    -- ** Request lenses
    , ucdCoreDefinitionId
    , ucdName

    -- * Destructuring the response
    , UpdateCoreDefinitionResponse (..)
    , mkUpdateCoreDefinitionResponse
    -- ** Response lenses
    , ursResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCoreDefinition' smart constructor.
data UpdateCoreDefinition = UpdateCoreDefinition'
  { coreDefinitionId :: Core.Text
    -- ^ The ID of the core definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCoreDefinition' value with any optional fields omitted.
mkUpdateCoreDefinition
    :: Core.Text -- ^ 'coreDefinitionId'
    -> UpdateCoreDefinition
mkUpdateCoreDefinition coreDefinitionId
  = UpdateCoreDefinition'{coreDefinitionId, name = Core.Nothing}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdCoreDefinitionId :: Lens.Lens' UpdateCoreDefinition Core.Text
ucdCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# INLINEABLE ucdCoreDefinitionId #-}
{-# DEPRECATED coreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdName :: Lens.Lens' UpdateCoreDefinition (Core.Maybe Core.Text)
ucdName = Lens.field @"name"
{-# INLINEABLE ucdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateCoreDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCoreDefinition where
        toHeaders UpdateCoreDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCoreDefinition where
        toJSON UpdateCoreDefinition{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateCoreDefinition where
        type Rs UpdateCoreDefinition = UpdateCoreDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/definition/cores/" Core.<>
                             Core.toText coreDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateCoreDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCoreDefinitionResponse' smart constructor.
newtype UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCoreDefinitionResponse' value with any optional fields omitted.
mkUpdateCoreDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateCoreDefinitionResponse
mkUpdateCoreDefinitionResponse responseStatus
  = UpdateCoreDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateCoreDefinitionResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
